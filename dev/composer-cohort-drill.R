# Composer demographic summary -> drill a level -> cohort in the patient
# profile. The full workflow discussed for CEDX cohort drill-downs:
#
#   data (safetyData ADaM) -> cdisc -> adsl pull -> demog (composer table)
#                               |            -> tbl (drillable summary)
#                               |                  -> send (fn, ctrl_send)
#                               `-> cohort_filter (value filter) -> profile
#
# Click "F" on the Sex block of the summary: the drilled output carries the
# selection as a real column (SEX = "F"). The `send` function block reads that
# condition and pushes it -- via blockr.extra::ctrl_send(), the board's
# external-control channel -- into the VALUE FILTER's state. The filter
# narrows the dm (dm_filter cascades through FKs), so the patient profile
# receives the 143 females as its cohort and its picker browses only them.
# Works for any condition: click a RACE level and the cohort swaps
# (latest write wins). Remove the pill in the filter (or clear its values)
# to get back to all 254 patients.
#
# Roles:
#   value filter   = which patients exist (cohort authority; also the manual
#                    editor and the reset point)
#   profile picker = which of them you are looking at
#
# There is NO data link from the summary table to the profile -- the drill
# travels over the control channel, so under lazy eval the profile never
# drags the table pipeline into its closure.
#
# Run from the workspace root (or from the package dir):
#   Rscript blockr.pharma/dev/composer-cohort-drill.R          # port 3838
#   Rscript blockr.pharma/dev/composer-cohort-drill.R 3839     # any port
#   BLOCKR_PORT=3839 Rscript blockr.pharma/dev/composer-cohort-drill.R
#
# NOTE: load_all() ALL of them, never a mix (theme asset resolution).
# blockr.sandbox provides as_annotated_df() for composer tables; composer
# itself is loaded from source alongside.

root <- if (file.exists("blockr.pharma/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.theme", "blockr.dplyr", "blockr.dm",
            "blockr.pharma", "blockr.viz", "blockr.extra", "blockr.sandbox",
            "composer", "blockr.dock", "blockr.dag")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

library(safetyData)

port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else "3838"
  p <- suppressWarnings(as.integer(raw))
  if (is.na(p)) stop("Not a port: ", raw, call. = FALSE)
  p
})

options(
  shiny.port = port,
  "g6R.preserve_elements_position" = TRUE,
  # Run everything eagerly: core main's render gate needs the dock's
  # visible-set report, and this demo runs on dock MAIN (see below).
  blockr.gate_visibility = FALSE
  # NOTE: dock@304-defer-offscreen-docks (the CEDX startup pin) BREAKS the
  # table block's control section here -- dt_result renders server-side but
  # the client element stays empty (the deferred dock loses the renderUI
  # delivery). Chart/profile/value-filter UIs survive (static containers +
  # custom messages). Run this demo with blockr.dock on MAIN.
)
message("composer cohort drill demo on http://127.0.0.1:", port, "/")

# No-UI extension exposing the board update channel to block code
# (blockr.extra::ctrl_send). Extensions receive `update`; the dock board's
# `callbacks` slot is taken by its visibility reporter.
new_ctrl_bridge_extension <- function() {
  new_dock_extension(
    server = function(id, board, update, ...) {
      shiny::moduleServer(id, function(input, output, session) {
        blockr.extra::install_ctrl_send(update)
        list(state = list())
      })
    },
    ui = function(ns, ...) htmltools::div(),
    name = "Control bridge",
    class = "ctrl_bridge_extension"
  )
}

serve(
  new_dock_board(
    blocks = c(
      data = new_dm_example_block(dataset = "safetydata_adam"),
      cdisc = new_cdisc_dm_block(),
      adsl = new_dm_pull_block(table = "adsl", block_name = "Pull ADSL"),

      # Composer demographic summary. The fn returns the composed table
      # object; the downstream table block coerces it via as_annotated_df()
      # (method registered by blockr.sandbox).
      demog = new_function_block(
        fn = "function(data) {
  data <- as.data.frame(data)
  data[] <- lapply(data, function(x) if (is.factor(x)) as.character(x) else x)
  data$AGE <- as.numeric(data$AGE)
  spec <- composer::table(
    title = 'Demographic Characteristics',
    population = 'All Subjects',
    data = data,
    denominator = composer::make_denom(data, trt = 'ARM'),
    column_header_left = 'Characteristic',
    bigN_format = 'xxx'
  )
  spec <- composer::colgroup(spec, composer::by(variable = 'ARM'))
  spec <- composer::block_continuous(spec,
    label = 'Age', variable = 'AGE',
    statistic = c('n' = '{N:xxx}',
                  'Mean (SD)' = '{mean:xx.x} ({sd:xx.x})'),
    blank_after = TRUE)
  spec <- composer::block_categorical(spec,
    label = 'Sex', variable = 'SEX',
    statistic = '{n:xx} ({pct:xx.x}%)', blank_after = TRUE)
  spec <- composer::block_categorical(spec,
    label = 'Race', variable = 'RACE',
    statistic = '{n:xx} ({pct:xx.x}%)')
  composer::compose(spec)
}",
        block_name = "Demographics (composer)"
      ),

      # Structured drill is opt-in: drill = "auto" makes every level row
      # clickable on its ARD identity (same convention as chart / tile).
      tbl = new_table_block(drill = "auto",
                            block_name = "Demographic Characteristics"),

      # Drill -> condition -> value filter, over the control channel. The
      # drilled annotated df is read on its ARD IDENTITY columns, directly:
      # `.variable` is the source column NAME (SEX), `.variable_level` the
      # raw source VALUE on that row (F). The drilled output is a pure
      # SUBSET of the annotated df -- no spread column, no attributes.
      #
      # The claim rule: a dimension (each `.group<k>` pair, and the
      # `.variable` leaf) becomes a filter condition only when the subset
      # resolves it to EXACTLY ONE value. One value = a decision; many =
      # not a claim. So a leaf click sends `SEX = F`; a SOC header click in
      # a nested table sends just the SOC (its PT leaf is multi-valued and
      # drops); the undrilled table -- at startup, after an upstream
      # recompute, or after a re-click cleared the drill -- resolves no
      # dimension and sends NOTHING new. Instead it calls ctrl_clear():
      # the filter resets ONLY if this block authored its current state
      # (ownership lives in the bridge), so an un-drill propagates while a
      # sibling sender's cohort or a restored board is never clobbered.
      send = new_function_block(
        fn = "function(data, table = 'adsl') {
  none <- data.frame(condition = '(click a level row in the summary)',
                     values = '')
  clear <- function() {
    blockr.extra::ctrl_clear('cohort_filter',
                             state = list(columns = list()))
    none
  }
  if (!is.data.frame(data)) return(clear())
  nms <- names(data)
  if (!all(c('.variable', '.variable_level') %in% nms)) return(clear())

  var <- as.character(data$.variable)
  lvl <- as.character(data$.variable_level)
  keep <- !is.na(var) & !is.na(lvl) & nzchar(lvl)
  if (!any(keep)) return(clear())

  single <- function(x) {
    u <- unique(x[!is.na(x) & nzchar(x)])
    if (length(u) == 1L) u else NULL
  }

  # One `columns` entry per single-valued dimension: the enclosing
  # `.group<k>` / `.group<k>_level` pairs (outermost first), then the
  # `.variable` leaf.
  cols <- list()
  gl <- nms[startsWith(nms, '.group') & endsWith(nms, '_level')]
  gl <- gl[order(as.integer(substr(gl, 7L, nchar(gl) - 6L)))]
  for (g in gl) {
    gname <- substr(g, 1L, nchar(g) - 6L)
    if (!gname %in% nms) next
    gn <- single(as.character(data[[gname]])[keep])
    gv <- single(as.character(data[[g]])[keep])
    if (is.null(gn) || is.null(gv)) next
    cols[[length(cols) + 1L]] <- list(name = gn, table = table,
                                      mode = 'multi', values = gv)
  }
  cond <- single(var[keep])
  val  <- single(lvl[keep])
  if (!is.null(cond) && !is.null(val)) {
    cols[[length(cols) + 1L]] <- list(name = cond, table = table,
                                      mode = 'multi', values = val)
  }
  if (!length(cols)) return(clear())

  blockr.extra::ctrl_send('cohort_filter', state = list(columns = cols))
  data.frame(
    condition = vapply(cols, function(x) x$name, character(1L)),
    values = vapply(cols, function(x) paste(x$values, collapse = ', '),
                    character(1L))
  )
}",
        block_name = "Send cohort to filter"
      ),

      # The cohort authority: filters the dm, cascades through FKs, shows
      # the active condition as an editable pill, and is the reset point.
      cohort_filter = new_value_filter_block(block_name = "Cohort"),

      profile = new_patient_profile_block(
        selected = c("patient_overview", "ae_gantt"),
        block_name = "Patient profile"
      )
    ),
    links = list(
      list(from = "data", to = "cdisc", input = "data"),
      list(from = "cdisc", to = "adsl", input = "data"),
      list(from = "adsl", to = "demog", input = "data"),
      list(from = "demog", to = "tbl", input = "data"),
      list(from = "tbl", to = "send", input = "data"),
      list(from = "cdisc", to = "cohort_filter", input = "data"),
      list(from = "cohort_filter", to = "profile", input = "data")
    ),
    extensions = list(
      dag_extension = new_dag_extension(),
      ctrl_bridge = new_ctrl_bridge_extension()
    ),
    grids = list(
      Cohort = dock_grid(
        list("tbl", "send"),
        list("cohort_filter", "profile"),
        sizes = c(1, 1)
      ),
      Pipeline = dock_grid("dag_extension"),
      Data = dock_grid(c("data", "cdisc", "adsl", "demog"))
    ),
    active = "Cohort"
  )
)
