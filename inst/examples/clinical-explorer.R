# Clinical Explorer -- cross-filter, drilldown charts, AE heatmap, summary
# tables and the patient profile over the ADaM safety tables (the demo shown at
# R/Medicine 2026).
#
# The board is BUILT IN R, right here. There is no pinned board JSON any more:
# this file is the definition, so what you read is what runs. Run it with:
#
#   source(system.file("examples/clinical-explorer.R", package = "blockr.pharma"))
#
# It is a demo, deliberately small: 24 blocks over six views. The production
# CDEx board it is modelled on carries about 110 blocks over ten views, most of
# them clinical tables composed with an internal package. Those do not appear
# here. Where the production board composes a table, this one uses
# `new_summary_table_block()`, which is open and does the same job for the
# handful of tables a demo needs.

# ---- Package loading (dual: installed vs local source) ---------------------
# `dev_local = FALSE` (the default, and what ships) attaches the INSTALLED
# packages with library(). Set it to TRUE -- or source this file from the
# dev/clinical-explorer.R wrapper -- to load every blockr package from its LOCAL
# source checkout with pkgload::load_all(). One board, two loaders, no drift.
if (!exists("dev_local")) dev_local <- FALSE

blockr_pkgs <- c(
  "blockr.core",
  "blockr.ui",        # html_table_display, the tabular display below
  "blockr.dock",
  "blockr.dag",
  "blockr.dplyr",
  "blockr.io",
  "blockr.viz",       # charts, summary tables, table renderer
  "blockr.dm",        # the dm, the crossfilter, the value filters
  "blockr.pharma",    # patient profile, AE heatmap, the study roles option
  "blockr.extra",     # the search block
  "blockr.outline",   # the outline rail
  "blockr.assistant", # the LLM chat pane
  "blockr.session"
)

for (pkg in blockr_pkgs) {
  if (dev_local) pkgload::load_all(pkg, quiet = TRUE)
  else library(pkg, character.only = TRUE)
}

# The board's `data` block rebuilds its dm from safetyData (the ADaM example
# tables adsl/adae/adlbc/advs), so that package must be installed.
library(safetyData)

# ---- Curate the block browser ----------------------------------------------
# Keep ONLY `dataset` and `glue` from blockr.core; drop its low-level / noise
# blocks (subset, merge, rbind, head, scatter, csv, filebrowser, upload) via
# unregister_blocks(), selecting by the registry `package` attribute so only
# core blocks are affected. What is left is what the block browser offers and
# what the assistant can reach for.
core_keep <- c("dataset_block", "glue_block")
core_drop <- setdiff(
  names(Filter(
    function(entry) identical(attr(entry, "package"), "blockr.core"),
    available_blocks()
  )),
  core_keep
)
unregister_blocks(core_drop)

options(
  blockr.dock_is_locked = FALSE,
  blockr.tabular_display = blockr.ui::html_table_display,
  blockr.eval_parent_env = asNamespace("stats"),
  # Build a block only once it is required, instead of staggering every
  # off-screen block into existence in the background. This board carries 25
  # blocks across six views, so the default pass spends the whole startup
  # constructing blocks nobody is looking at.
  blockr.background_construction_delay = Inf,
  # blockr.session: build the board from the request URL (board_name / user /
  # version handle) so named projects are shareable / bookmarkable.
  blockr.session_url_params = TRUE
)

# ---- Blocks ----------------------------------------------------------------
# Ids are the names below and are what links, views and grids refer to. They
# are readable on purpose: this file is the board's documentation.
#
# `visible = "inputs"` on a block means its panel shows the block's OWN
# output (the drawn table, the chart, the heatmap, the profile) and NOT the
# result-preview data frame underneath it, which is the default. The blocks
# left at the default are the plumbing ones, where the preview IS the point.

blocks <- blockr.core::as_blocks(list(

  # -- Source: the ADaM dm, keyed, then filtered once for the whole board ----
  data = blockr.dm::new_dm_example_block(
    dataset = "safetydata_adam",
    block_name = "ADaM data"
  ),
  cdisc = blockr.dm::new_cdisc_dm_block(
    set_keys = TRUE,
    dedup_cols = TRUE,
    block_name = "CDISC"
  ),
  # One filter for every view. Selecting an arm here narrows demographics,
  # adverse events, labs and vitals at the same time, which is the point of
  # putting it on the left of every layout below.
  global_filter = blockr.dm::new_crossfilter_block(
    active_dims = list(
      adsl = c("ARM", "SEX"),
      adae = "AESEV",
      advs = c("PARAM", "AVAL")
    ),
    measure = ".count",
    agg_func = "sum",
    visible = "inputs",
    block_name = "Filter"
  ),

  # -- One flat table per domain, each joined back to adsl for the arm ------
  dx_pull = blockr.dm::new_dm_pull_block(
    table = "adsl",
    block_name = "Subjects"
  ),
  ae_flat = blockr.dm::new_dm_flatten_block(
    start_table = "adae",
    include_tables = "adsl",
    join_type = "left",
    # NOT the default TRUE: dm's recursive flatten wants a join function, not
    # a join name, and errors with "Recursive flattening only supports
    # left_join(), inner_join(), or full_join()". One hop to adsl is all this
    # board needs anyway.
    recursive = FALSE,
    block_name = "Adverse Events"
  ),
  lab_flat = blockr.dm::new_dm_flatten_block(
    start_table = "adlbc",
    include_tables = "adsl",
    join_type = "left",
    # NOT the default TRUE: dm's recursive flatten wants a join function, not
    # a join name, and errors with "Recursive flattening only supports
    # left_join(), inner_join(), or full_join()". One hop to adsl is all this
    # board needs anyway.
    recursive = FALSE,
    block_name = "Lab Values"
  ),
  vs_flat = blockr.dm::new_dm_flatten_block(
    start_table = "advs",
    include_tables = "adsl",
    join_type = "left",
    # NOT the default TRUE: dm's recursive flatten wants a join function, not
    # a join name, and errors with "Recursive flattening only supports
    # left_join(), inner_join(), or full_join()". One hop to adsl is all this
    # board needs anyway.
    recursive = FALSE,
    block_name = "Vital Signs"
  ),

  # -- Population -----------------------------------------------------------
  # Table 1. `new_summary_table_block()` emits the annotated data frame and
  # `new_table_block()` draws it; that pairing is the open stand-in for the
  # composed tables the production board carries.
  pop_demog = blockr.viz::new_summary_table_block(
    vars = c("AGE", "SEX", "RACE", "ETHNIC", "AGEGR1"),
    by = "ARM",
    stats = "mean_sd",
    add_overall = TRUE,
    overall_label = "Total",
    visible = "inputs",
    block_name = "Demographics (spec)"
  ),
  # `drill = "source"` is what makes a summary table drillable. On a
  # structured table `drill` is a MODE, not a column: "auto" hands downstream
  # the clicked DISPLAY row ("Sex: F"), "source" hands the RECORDS behind it
  # (the 143 female subjects), resolved from the row's claim against the frame
  # `summary_table()` stamped on its output. The claim is ROW-shaped by
  # design: the arms are columns here and a column carries no machine
  # identity, so clicking F in the Placebo column claims SEX == F, not
  # SEX == F AND ARM == Placebo.
  pop_demog_tbl = blockr.viz::new_table_block(
    max_height = "600px",
    drill = "source",
    ctrl_target = "pt_drill",
    visible = "inputs",
    block_name = "Demographics"
  ),
  # Drills like every other exhibit: clicking a bar segment sends that arm /
  # discontinuation-reason COHORT to the profile, which is what its patient
  # picker is for. A drill need not resolve to a single subject.
  dispo_chart = blockr.viz::new_chart_block(
    chart_type = "bar",
    group = "ARM",
    color = "DCREASCD",
    value = ".count",
    func = "count",
    drill = "auto",
    ctrl_target = "pt_drill",
    visible = "inputs",
    block_name = "Disposition"
  ),

  # -- Data explorer --------------------------------------------------------
  dx_select = blockr.dplyr::new_select_block(
    visible = "inputs",
    block_name = "Columns"
  ),
  dx_search = blockr.extra::new_search_block(block_name = "Search"),
  dx_download = blockr.io::new_download_block(
    format = "csv",
    visible = "inputs",
    block_name = "Download"
  ),

  # -- Adverse events -------------------------------------------------------
  # Patients with at least one event of each severity / seriousness /
  # relatedness, by actual arm. `id_var` is what makes the cells count
  # PATIENTS rather than event rows, which is the only way an AE table is
  # read.
  ae_summary = blockr.viz::new_summary_table_block(
    vars = c("AESEV", "AESER", "AEREL"),
    by = "TRT01A",
    id_var = "USUBJID",
    add_overall = TRUE,
    overall_label = "Total",
    visible = "inputs",
    block_name = "AE Summary (spec)"
  ),
  ae_summary_tbl = blockr.viz::new_table_block(
    max_height = "600px",
    drill = "source",
    ctrl_target = "pt_drill",
    visible = "inputs",
    block_name = "AE Summary"
  ),
  # Subject x preferred term matrix, cells painted by worst severity, rows
  # grouped by arm. This replaces the hand-written function block plus shaded
  # table the demo used to carry: `new_ae_heatmap_block()` now owns it.
  # safetyData's ADAE has no CTCAE grade column (AETOXGR), so the paint runs
  # off AESEV, whose MILD / MODERATE / SEVERE happens to sort correctly.
  ae_heatmap = blockr.pharma::new_ae_heatmap_block(
    color = "AESEV",
    group = "TRT01A",
    top_n = 25L,
    drill = TRUE,
    ctrl_target = "pt_drill",
    visible = "inputs",
    block_name = "AE Heatmap"
  ),
  ae_freq = blockr.viz::new_chart_block(
    chart_type = "bar",
    group = "AEDECOD",
    color = "AESEV",
    value = ".count",
    func = "count",
    sort_by = "value",
    sort_dir = "desc",
    drill = "auto",
    ctrl_target = "pt_drill",
    visible = "inputs",
    block_name = "Most Frequent AE"
  ),
  ae_gantt = blockr.viz::new_chart_block(
    chart_type = "gantt",
    x = "ASTDY",
    xend = "AENDY",
    y = "USUBJID",
    color = "AESEV",
    series = "AETERM",
    sort_by = "onset",
    sort_dir = "asc",
    drill = "auto",
    ctrl_target = "pt_drill",
    visible = "inputs",
    block_name = "AE Swimlanes"
  ),

  # -- Labs and vitals ------------------------------------------------------
  # Each modality pins one PARAM, then plots every subject's trajectory.
  lab_param = blockr.dm::new_value_filter_block(
    state = list(columns = list(
      list(
        name = "PARAM", mode = "single",
        values = "Alanine Aminotransferase (U/L)"
      )
    )),
    visible = "inputs",
    block_name = "Lab parameter"
  ),
  lab_traj = blockr.viz::new_chart_block(
    chart_type = "line",
    x = "ADY", y = "AVAL", series = "USUBJID",
    drill = "auto",
    ctrl_target = "pt_drill",
    visible = "inputs",
    block_name = "Lab Trajectory"
  ),
  vs_param = blockr.dm::new_value_filter_block(
    state = list(columns = list(
      list(
        name = "PARAM", mode = "single",
        values = "Pulse Rate (BEATS/MIN)"
      )
    )),
    visible = "inputs",
    block_name = "Vital parameter"
  ),
  vs_traj = blockr.viz::new_chart_block(
    chart_type = "line",
    x = "ADY", y = "AVAL", series = "USUBJID",
    drill = "auto",
    ctrl_target = "pt_drill",
    visible = "inputs",
    block_name = "Vital Signs Trajectory"
  ),

  # -- Patient drill --------------------------------------------------------
  # EVERY exhibit nominates a cohort: disposition, AE frequency, AE swimlanes,
  # the AE heatmap, the lab trajectory and the vitals trajectory. They do it
  # over the board's CONTROL CHANNEL, not over links -- each carries
  # `ctrl_target = "pt_drill"` and pushes the claim its click makes ("AEDECOD
  # = HEADACHE", "USUBJID = 01-710-1315") into this one block.
  #
  # `new_drill_filter_block()` is the receiving end. It is a value filter that
  # resolves a claimed column to a table in its own dm by itself: exactly one
  # table carries the column, or the table where it is the primary key, since
  # `dm::dm_filter()` cascades along foreign keys. So a claim on AEDECOD
  # filters adae and narrows adsl to the subjects who had that event, and a
  # claim on USUBJID lands on adsl and narrows everything below it. The sender
  # never has to know the data model.
  #
  # It replaces a `new_latest_block()` collector plus a
  # `new_dm_filter_by_data_block()`, which needed one link per sender and
  # could only ever carry the last clicked subject.
  #
  # It appears in no view, so it never takes a panel; omission from every view
  # is how a dock board hides a block.
  pt_drill = blockr.dm::new_drill_filter_block(
    block_name = "Drill filter"
  ),
  # The three panels the profile opens on. `adlbc_all` is not a static viz id
  # like the other two: findings groups are derived from the data, and this
  # one is safetyData's chemistry lab table taken whole (adlbc carries one
  # PARCAT1 value throughout, so the TABLE is the group, and its card is
  # titled "Chemistry"). It is therefore specific to this dm -- a board on
  # another study's data names its own groups. Static ids are validated at
  # construction; group ids at runtime, when the dm is in hand.
  #
  # The arm column comes from the board's study roles option below, not from
  # a constructor argument. The profile lives on the right rail of every view
  # (see the layout section), so a drill from any view lands somewhere the
  # user is already looking, and its input is the drilled dm, so a claim from
  # any exhibit narrows the cohort it offers.
  pt_profile = blockr.pharma::new_patient_profile_block(
    selected = c("patient_overview", "adlbc_all", "questionnaire_heatmap"),
    visible = "inputs",
    block_name = "Patient profile"
  )
))

# ---- Links -----------------------------------------------------------------

# The last two edges are the drill chain: the dm goes into the drill filter and
# the filtered dm into the profile. It hangs off `cdisc`, not off
# `global_filter`, so a drilled patient stays visible whatever the crossfilter
# is set to.
links <- blockr.core::links(
  from = c(
    "data", "cdisc",
    "global_filter", "global_filter", "global_filter", "global_filter",
    "dx_pull", "dx_pull", "dx_pull",
    "dx_select", "dx_search",
    "pop_demog",
    "ae_flat", "ae_summary", "ae_flat", "ae_flat", "ae_flat",
    "lab_flat", "lab_param",
    "vs_flat", "vs_param",
    "cdisc", "pt_drill"
  ),
  to = c(
    "cdisc", "global_filter",
    "dx_pull", "ae_flat", "lab_flat", "vs_flat",
    "pop_demog", "dispo_chart", "dx_select",
    "dx_search", "dx_download",
    "pop_demog_tbl",
    "ae_summary", "ae_summary_tbl", "ae_heatmap", "ae_freq", "ae_gantt",
    "lab_param", "lab_traj",
    "vs_param", "vs_traj",
    "pt_drill", "pt_profile"
  )
)

# ---- Layout ----------------------------------------------------------------
# Membership (which panels a view holds, plus its display name) and geometry
# (nesting, tab groups, sizes, rails) are two separate slots: `views` and
# `grids`. `panels(..., active =)` picks the tab that OPENS -- the default is
# the first one, which lands on the raw head of the chain rather than what the
# view is about, so every tab group names its own open tab.
dg <- blockr.dock::dock_grid
pn <- blockr.dock::panels
gr <- blockr.dock::group
vw <- blockr.dock::dock_view
rl <- blockr.dock::rail
ext <- blockr.dock::ext

# The outline, the patient profile and the assistant ride a right-hand rail in
# every view, the way the production board carries them. The profile belongs
# here rather than in the splitview because every view can drill into it: it
# is the one panel that has to stay put while you move between adverse
# events, labs and vitals. It opens; the outline and the assistant are tabs
# behind it.
side_rail <- function() {
  rl(
    ext("outline"), "pt_profile", ext("assistant"),
    position = "right",
    active = "pt_profile",
    size = 560
  )
}
# A view's membership is a plain character vector of panel ids, so an
# extension reference has to be flattened with as.character() first. Passing
# the `ext()` object itself splices its fields into the vector.
ext_panel <- function(id) as.character(ext(id))
side_panels <- c(ext_panel("outline"), "pt_profile", ext_panel("assistant"))

views <- list(
  Setup = vw(c("data", "cdisc", ext_panel("dag"), side_panels)),
  Population = vw(c(
    "global_filter", "pop_demog_tbl", "dispo_chart", side_panels
  )),
  DataExplorer = vw(
    c("global_filter", "dx_pull", "dx_select", "dx_search", "dx_download",
      side_panels),
    name = "Data Explorer"
  ),
  AdverseEvents = vw(
    c("global_filter", "ae_heatmap", "ae_freq", "ae_gantt", "ae_summary_tbl",
      side_panels),
    name = "Adverse Events"
  ),
  Lab = vw(c("global_filter", "lab_param", "lab_traj", side_panels)),
  VitalSigns = vw(
    c("global_filter", "vs_param", "vs_traj", side_panels),
    name = "Vital Signs"
  )
)

grids <- list(
  Setup = dg("data", "cdisc", ext("dag"), side_rail()),
  Population = dg(
    "global_filter",
    pn("pop_demog_tbl", "dispo_chart", active = "pop_demog_tbl"),
    side_rail(),
    sizes = c(1.3, 4)
  ),
  # The searchable table, not the raw pull, and not the download button that
  # follows it.
  DataExplorer = dg(
    "global_filter",
    pn("dx_pull", "dx_select", "dx_search", "dx_download",
       active = "dx_search"),
    side_rail(),
    sizes = c(1.3, 4)
  ),
  # Opens on the heatmap: it is the one exhibit that shows the whole safety
  # picture at once, and clicking a row drives the profile in the rail.
  AdverseEvents = dg(
    "global_filter",
    pn("ae_heatmap", "ae_freq", "ae_gantt", "ae_summary_tbl",
       active = "ae_heatmap"),
    side_rail(),
    sizes = c(1.3, 4)
  ),
  Lab = dg(
    gr("global_filter", "lab_param"),
    "lab_traj",
    side_rail(),
    sizes = c(1, 3)
  ),
  VitalSigns = dg(
    gr("global_filter", "vs_param"),
    "vs_traj",
    side_rail(),
    sizes = c(1, 3)
  )
)

# ---- Board -----------------------------------------------------------------

board <- blockr.dock::new_dock_board(
  blocks = blocks,
  links = links,
  # Named, so the ids the layout refers to (`ext("outline")` and friends) are
  # these names rather than a class-derived fallback.
  extensions = list(
    dag = blockr.dag::new_dag_extension(),
    outline = blockr.outline::new_outline_extension(),
    assistant = blockr.assistant::new_assistant_extension(),
    # Opens the control channel the drill runs on. It goes with the drill
    # filter block: a bridge with no drill filter has senders with nowhere to
    # send, and a drill filter with no bridge is just a value filter.
    ctrl_bridge = blockr.viz::new_ctrl_bridge_extension()
  ),
  # safetyData's ADSL carries no ACTARM, so the profile's arm role must be
  # declared or the block stops with a named error (that is the design --
  # never silently fall back to ARM, the *planned* arm, in a safety view).
  options = c(
    blockr.dock::dock_board_options(),
    blockr.core::new_board_options(
      blockr.pharma::new_study_roles_option(arm = "TRT01A")
    )
  ),
  views = views,
  grids = grids,
  active = "Population"
)

# blockr.session drives project management: `manage_project()` adds the
# save / load / share plugin (the served demo board stays visible on a cold
# load; users save named projects on top of it). The default backend is a
# local pins board (pins::board_local()) off Posit Connect, so this works
# as-is locally. NB: do NOT pair with `rack_loader()` here -- that loader
# clears the board on a handle-less cold load, which would hide the demo.
serve(
  board,
  plugins = custom_plugins(list(manage_project()))
)
