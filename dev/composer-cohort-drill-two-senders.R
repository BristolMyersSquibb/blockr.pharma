# TWO senders, ONE filter. Variant of composer-cohort-drill.R that puts a
# second drillable summary on the board, both pushing into the SAME value
# filter over the control channel -- via the TABLE BLOCKS' OWN send
# (ctrl_target, the gear's "Send to filter" option), no sender blocks:
#
#   adsl -> demog_a (Sex, Race)      -> tbl_a --.
#     |                                          >-- cohort_filter -> profile
#     `-> demog_b (Age group, Ethn.) -> tbl_b --'
#
# What to watch (the whole point of this demo):
#
#   1. Click `F` on Sex in table A. Filter holds SEX = F; A owns the claim.
#   2. Click `>80` on Age group in table B. The filter now holds ONLY
#      AGEGR1 = >80 -- SEX = F is GONE. ctrl_send() pushes the target's whole
#      `state$columns`, so the second sender REPLACES the first; it does not
#      compose with it. Two disjoint drills read like "narrow, then narrow
#      again" but the second one overwrites. This is the behaviour to decide
#      on (replace vs merge), not a bug to fix in this script.
#   3. Table A's status footer is now STALE: it says Filtered: SEX = F while
#      the cohort is AGEGR1 = >80. A has no data link to the filter, so
#      nothing re-evaluates it when B sends. The control channel has no
#      back-edge, by design.
#   4. Un-drill A (re-click `F`). A re-evaluates, calls ctrl_clear(), finds it
#      no longer owns the target (B does) -> no-op. B's cohort SURVIVES. This
#      is the ownership rule doing its job: an un-drill never clobbers a
#      sibling's claim.
#   5. Un-drill B. B owns -> the filter resets to all 254 subjects.
#
# Roles are unchanged from the single-sender demo: the value filter is the
# cohort authority (and the manual reset point), the profile picker chooses
# among the cohort.
#
# Run from the workspace root (or from the package dir):
#   Rscript blockr.pharma/dev/composer-cohort-drill-two-senders.R        # 3838
#   Rscript blockr.pharma/dev/composer-cohort-drill-two-senders.R 3839
#
# NOTE: load_all() ALL of them, never a mix (theme asset resolution), and run
# blockr.dock on MAIN -- dock@304-defer-offscreen-docks breaks the table
# block's control section (see composer-cohort-drill.R).

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
  blockr.gate_visibility = FALSE
)
message("two-sender cohort drill demo on http://127.0.0.1:", port, "/")

# The board update channel is exposed to block code by the packaged bridge
# extension, blockr.viz::new_ctrl_bridge_extension() (this script used to
# hand-roll it). It is one extension for the whole board on purpose: the
# last-author registry lives inside install_ctrl_send(), so BOTH senders here
# share one registry -- that is what makes step 4 above work. It also hands the
# board to the channel, which is what lets each table's gear offer a picker of
# the board's value filter blocks.

# One composer summary per pair of variables. `vars` is a list of
# label/variable pairs, spliced into block_categorical() calls.
demog_fn <- function(title, vars) {
  blocks <- paste(
    vapply(
      vars,
      function(v) sprintf(
        "  spec <- composer::block_categorical(spec,
    label = '%s', variable = '%s',
    statistic = '{n:xx} ({pct:xx.x}%%)', blank_after = TRUE)",
        v$label, v$variable
      ),
      character(1L)
    ),
    collapse = "\n"
  )
  sprintf("function(data) {
  data <- as.data.frame(data)
  data[] <- lapply(data, function(x) if (is.factor(x)) as.character(x) else x)
  spec <- composer::table(
    title = '%s',
    population = 'All Subjects',
    data = data,
    denominator = composer::make_denom(data, trt = 'ARM'),
    column_header_left = 'Characteristic',
    bigN_format = 'xxx'
  )
  spec <- composer::colgroup(spec, composer::by(variable = 'ARM'))
%s
  composer::compose(spec)
}", title, blocks)
}

# The senders are the TABLE BLOCKS THEMSELVES: `ctrl_target` names the value
# filter to drive and `ctrl_table` the dm table the conditions apply to (the
# gear's "Send to filter (beta)" option). The send rides on the drill's own
# clicks, so it keeps working when the table is parked on an off-screen view
# under lazy eval -- the failure mode that killed the standalone sender block
# this demo used to wire per table.
#
# Both send to the SAME target. Authorship is scoped by the module namespace a
# claim was sent from, so each sender owns its own claim and an un-drill from
# one never clears the other's (see ctrl_send()'s "Clearing" section). The
# claim is read off the block's own drill state -- a structured table drills
# on its ARD identity (.variable / .group<k>), a chart or tile on its real
# drill column, and the block knows which, so nothing is configured.


serve(
  new_dock_board(
    blocks = c(
      data = new_dm_example_block(dataset = "safetydata_adam"),
      cdisc = new_cdisc_dm_block(),
      adsl = new_dm_pull_block(table = "adsl", block_name = "Pull ADSL"),

      # Sender A: Sex / Race
      demog_a = new_function_block(
        fn = demog_fn(
          "Demographics (Sex, Race)",
          list(list(label = "Sex", variable = "SEX"),
               list(label = "Race", variable = "RACE"))
        ),
        block_name = "Demographics A (composer)"
      ),
      tbl_a = new_table_block(
        drill = "auto", ctrl_target = "cohort_filter", ctrl_table = "adsl",
        block_name = "Sex / Race"
      ),

      # Sender B: Age group / Ethnicity -- deliberately DISJOINT dimensions
      # from A, so a merge would be meaningful and a replace is visible.
      demog_b = new_function_block(
        fn = demog_fn(
          "Demographics (Age group, Ethnicity)",
          list(list(label = "Age group", variable = "AGEGR1"),
               list(label = "Ethnicity", variable = "ETHNIC"))
        ),
        block_name = "Demographics B (composer)"
      ),
      tbl_b = new_table_block(
        drill = "auto", ctrl_target = "cohort_filter", ctrl_table = "adsl",
        block_name = "Age group / Ethnicity"
      ),

      # The single cohort authority both senders write to.
      cohort_filter = new_value_filter_block(block_name = "Cohort"),

      profile = new_patient_profile_block(
        selected = c("patient_overview", "ae_gantt"),
        block_name = "Patient profile"
      )
    ),
    links = list(
      list(from = "data", to = "cdisc", input = "data"),
      list(from = "cdisc", to = "adsl", input = "data"),
      list(from = "adsl", to = "demog_a", input = "data"),
      list(from = "demog_a", to = "tbl_a", input = "data"),
      list(from = "adsl", to = "demog_b", input = "data"),
      list(from = "demog_b", to = "tbl_b", input = "data"),
      list(from = "cdisc", to = "cohort_filter", input = "data"),
      list(from = "cohort_filter", to = "profile", input = "data")
    ),
    extensions = list(
      dag_extension = new_dag_extension(),
      ctrl_bridge = blockr.viz::new_ctrl_bridge_extension()
    ),
    grids = list(
      Cohort = dock_grid(
        "tbl_a",
        "tbl_b",
        list("cohort_filter", "profile"),
        sizes = c(1, 1, 1)
      ),
      Pipeline = dock_grid("dag_extension"),
      Data = dock_grid(c("data", "cdisc", "adsl", "demog_a", "demog_b"))
    ),
    active = "Cohort"
  )
)
