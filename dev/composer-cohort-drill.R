# Composer demographic summary -> drill a level -> cohort in the patient
# profile. The full workflow discussed for CEDX cohort drill-downs:
#
#   data (safetyData ADaM) -> cdisc -> adsl pull -> demog (composer table)
#                               |            -> tbl (drillable summary,
#                               |                    sends via ctrl_target)
#                               `-> cohort_filter (value filter) -> profile
#
# Click "F" on the Sex block of the summary: the table block reads the claim
# off its own drill (SEX = F) and pushes it -- via blockr.viz::ctrl_send(),
# the board's external-control channel; the table's `ctrl_target` argument is
# the gear's "Send to filter" option -- into the VALUE FILTER's state. The
# filter narrows the dm (dm_filter cascades through FKs), so the patient
# profile receives the 143 females as its cohort and its picker browses only
# them. Works for any condition: click a RACE level and the cohort swaps
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

# The board update channel is exposed to block code by the packaged bridge
# extension, blockr.viz::new_ctrl_bridge_extension() (this script used to
# hand-roll it): a no-UI extension calling install_ctrl_send(). Extensions
# receive `update`; the dock board's `callbacks` slot is taken by its
# visibility reporter.

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
      #
      # The table is ALSO the sender: `ctrl_target` (the gear's "Send to
      # filter" option) pushes the drill's claim into the value filter, over
      # the control channel. The claim is read off the block's own drill
      # state: `.variable` is the source column NAME (SEX), `.variable_level`
      # the raw source VALUE on that row (F) -- the drilled output stays a
      # pure SUBSET of the annotated df.
      #
      # The claim rule: a dimension (each `.group<k>` pair, and the
      # `.variable` leaf) becomes a filter condition only when the drilled
      # subset resolves it to EXACTLY ONE value. One value = a decision;
      # many = not a claim. So a leaf click sends `SEX = F`; a SOC header
      # click in a nested table sends just the SOC (its PT leaf is
      # multi-valued and drops); an un-drill (re-click, upstream recompute)
      # resolves no dimension and calls ctrl_clear() instead: the filter
      # resets ONLY if this block authored its current state (ownership
      # lives in the bridge), so an un-drill propagates while a sibling
      # sender's cohort or a restored board is never clobbered.
      tbl = new_table_block(drill = "auto",
                            ctrl_target = "cohort_filter",
                            ctrl_table = "adsl",
                            block_name = "Demographic Characteristics"),

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
      list(from = "cdisc", to = "cohort_filter", input = "data"),
      list(from = "cohort_filter", to = "profile", input = "data")
    ),
    extensions = list(
      dag_extension = new_dag_extension(),
      ctrl_bridge = blockr.viz::new_ctrl_bridge_extension()
    ),
    grids = list(
      Cohort = dock_grid(
        "tbl",
        list("cohort_filter", "profile"),
        sizes = c(1, 1)
      ),
      Pipeline = dock_grid("dag_extension"),
      Data = dock_grid(c("data", "cdisc", "adsl", "demog"))
    ),
    active = "Cohort"
  )
)
