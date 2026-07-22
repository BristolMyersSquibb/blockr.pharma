# Regenerate inst/examples/clinical-explorer.json
# ================================================
#
# The bundled demo board was serialized against blocks that have since been
# removed or renamed (the blockr.sandbox drilldown/heatmap/patient-profile
# blocks, the pre-graduation blockr.viz `bi_filter`, and the old
# blockr.dm `dm_semi_filter`). This script rebuilds the SAME board topology
# with the current, maintained blocks and writes a fresh, self-contained
# JSON.
#
# Block migration (old -> new):
#   new_drilldown_chart_block (blockr.sandbox) -> new_chart_block (blockr.viz)
#       arg renames: group_by->group, color_by->color, facet_by->facet,
#       metric->value, agg_fn->func, x_col->x, y_col->y, x_end_col->xend,
#       series_by->series; drilldown is now the explicit `drill=` arg.
#   new_ae_heatmap_block (blockr.sandbox) -> new_function_block (subject x PT
#       max-grade matrix) + new_table_block(sequential shading), the usual
#       AE-heatmap pattern (there is no `heatmap` chart type).
#   new_bi_filter_block (blockr.viz) -> new_value_filter_block (blockr.dm)
#   new_dm_semi_filter_block (blockr.dm) -> new_dm_filter_by_data_block
#       (blockr.dm); its link inputs change dm->data, ids->by.
#   new_sandbox_patient_profile_block (blockr.sandbox) ->
#       new_patient_profile_block (blockr.pharma)
#   react_extension (drilldown transport) -> dropped; drill is now carried by
#       plain links, and a blockr.dag DAG editor extension is added instead.
#
# Run from the workspace root:
#   Rscript blockr.pharma/dev/regenerate-clinical-explorer.R
#
# NB: this is the board's *reference* definition, not a byte-for-byte generator
# of the shipped JSON -- that file has since been patched in place (a rerun
# rewrites every constructor version, mints fresh link ids, and re-keys the DAG
# extension from `dag_extension` to `dag`). Read it to see what the board is;
# before overwriting the shipped JSON with its output, diff the two.

setwd("/workspace")

suppressMessages({
  for (p in c("blockr.core", "blockr.dock", "blockr.dag", "blockr.dplyr",
              "blockr.ggplot", "blockr.io", "blockr.viz", "blockr.dm",
              "blockr.pharma", "blockr.extra")) {
    pkgload::load_all(file.path("/workspace", p), quiet = TRUE)
  }
})

json_path <- "blockr.pharma/inst/examples/clinical-explorer.json"

old <- jsonlite::fromJSON(json_path, simplifyDataFrame = FALSE,
                          simplifyMatrix = FALSE)
old_blocks <- old$payload$blocks$payload

# --- Working blocks: harvested unchanged from the existing board ----------
# These deserialize cleanly against the current packages, so reuse them
# verbatim (zero-risk fidelity for the data pipeline, filters, tables and
# download configuration).
keep_ids <- c(
  "data", "cdisc", "global_filter", "pop_demog", "pop_demog_tbl",
  "dx_pull", "dx_select", "dx_search", "dx_download", "ae_flat",
  "fretful_nandoo", "blank_nautilus", "pt_latest"
)
kept <- lapply(old_blocks[keep_ids], blockr.core::blockr_deser)

# --- Migrated blocks: rebuilt with the current APIs -----------------------

# Per-patient trajectory / frequency / swim-lane charts. Each emits the
# clicked subject (drill = "USUBJID") into pt_latest.
traj_chart <- blockr.viz::new_chart_block(
  chart_type = "line", x = "ADY", y = "AVAL", series = "USUBJID",
  drill = "USUBJID", block_name = "Lab Trajectory"
)
cut_wryneck <- blockr.viz::new_chart_block(
  chart_type = "line", x = "ADY", y = "AVAL", series = "USUBJID",
  drill = "USUBJID", block_name = "Vital Signs Trajectory"
)
gantt_chart <- blockr.viz::new_chart_block(
  chart_type = "gantt", x = "ASTDY", xend = "AENDY", y = "USUBJID",
  color = "AESEV", series = "AETERM", sort_by = "onset", sort_dir = "asc",
  drill = "USUBJID", block_name = "AE Swimmlanes"
)
loving_noddy <- blockr.viz::new_chart_block(
  chart_type = "bar", group = "AEDECOD", color = "AESEV",
  value = ".count", func = "count", sort_by = "value", sort_dir = "desc",
  drill = "USUBJID", block_name = "Most Frequent AE"
)
# Demographic bar chart (not a patient drill target).
bony_urson <- blockr.viz::new_chart_block(
  chart_type = "bar", group = "ARM", color = "DCREASCD",
  value = ".count", func = "count", block_name = "Drilldown chart"
)

# Per-modality value filters (pin the modality to one PARAM). New column-object
# state shape (name/mode/values), configured directly.
radiant_gaur <- blockr.dm::new_value_filter_block(
  state = list(columns = list(
    list(name = "PARAM", mode = "single",
         values = "Alanine Aminotransferase (U/L)")
  )),
  block_name = "Analysis Value"
)
wayward_cardinal <- blockr.dm::new_value_filter_block(
  state = list(columns = list(
    list(name = "PARAM", mode = "single", values = "Pulse Rate (BEATS/MIN)")
  )),
  block_name = "Analysis Value"
)

# AE heatmap: subject x top-N-term matrix coloured by max toxicity grade
# (falls back to event count when no grade column). Rendered by a downstream
# table block with sequential shading -- the usual AE-heatmap pattern.
public_pike <- blockr.extra::new_function_block(
  fn = function(data) {
    top_n <- 30L
    term_var <- "AEDECOD"
    if (!term_var %in% names(data)) return(data.frame(message = "No data"))
    subj_col <- if ("USUBJID" %in% names(data)) "USUBJID" else
      if ("SUBJID" %in% names(data)) "SUBJID" else NULL
    if (is.null(subj_col)) return(data.frame(message = "No data"))
    grade_col <- if ("AETOXGR" %in% names(data)) "AETOXGR" else NULL
    tc <- as.data.frame(table(data[[term_var]]), stringsAsFactors = FALSE)
    names(tc) <- c("term", "n")
    tc <- tc[order(-tc$n), , drop = FALSE]
    top_terms <- utils::head(tc$term, top_n)
    filt <- data[data[[term_var]] %in% top_terms, , drop = FALSE]
    if (nrow(filt) == 0L) return(data.frame(message = "No data"))
    key <- paste(filt[[subj_col]], filt[[term_var]], sep = "\r")
    parts <- split(seq_len(nrow(filt)), key)
    rows <- lapply(parts, function(ix) {
      s <- filt[ix, , drop = FALSE]
      if (!is.null(grade_col)) {
        g <- suppressWarnings(as.numeric(as.character(s[[grade_col]])))
        v <- suppressWarnings(max(g, na.rm = TRUE))
        if (!is.finite(v)) v <- NA_real_
      } else {
        v <- nrow(s)
      }
      data.frame(subj = s[[subj_col]][1L], term = s[[term_var]][1L],
                 value = v, stringsAsFactors = FALSE)
    })
    agg <- do.call(rbind, rows)
    term_order <- top_terms[top_terms %in% agg$term]
    bd <- stats::aggregate(value ~ subj, agg,
                           function(x) sum(x, na.rm = TRUE))
    subj_order <- bd$subj[order(-bd$value)]
    out <- data.frame(USUBJID = subj_order, stringsAsFactors = FALSE,
                      check.names = FALSE)
    for (tm in term_order) {
      sub <- agg[agg$term == tm, , drop = FALSE]
      out[[tm]] <- sub$value[match(subj_order, sub$subj)]
    }
    out
  },
  block_name = "AE heatmap prep (subject x PT)"
)
ae_heatmap_tbl <- blockr.viz::new_table_block(
  cell_color = blockr.viz::drilldown_table_color("sequential"),
  block_name = "Heatmap"
)

# Patient filter (restricts the dm to the clicked subjects) + profile.
pt_semi <- blockr.dm::new_dm_filter_by_data_block(
  table = "adsl", key_col = "USUBJID", distinct_only = TRUE,
  block_name = "Patient filter"
)
pt_profile <- blockr.pharma::new_patient_profile_block(
  selected = c("patient_overview", "ae_gantt"),
  block_name = "Patient profile"
)

# --- Assemble the block set (original order + the added heatmap table) -----
blocks <- blockr.core::as_blocks(c(
  kept["data"], kept["cdisc"], kept["global_filter"],
  kept["pop_demog"], kept["pop_demog_tbl"],
  kept["dx_pull"], kept["dx_select"], kept["dx_search"], kept["dx_download"],
  kept["ae_flat"],
  list(
    traj_chart = traj_chart,
    gantt_chart = gantt_chart
  ),
  kept["pt_latest"],
  list(
    pt_semi = pt_semi,
    pt_profile = pt_profile,
    public_pike = public_pike,
    ae_heatmap_tbl = ae_heatmap_tbl,
    loving_noddy = loving_noddy
  ),
  kept["fretful_nandoo"], kept["blank_nautilus"],
  list(
    wayward_cardinal = wayward_cardinal,
    cut_wryneck = cut_wryneck,
    radiant_gaur = radiant_gaur,
    bony_urson = bony_urson
  )
))

# --- Links -----------------------------------------------------------------
# Single-input edges (input name defaults to the target's sole input).
links_main <- blockr.core::links(
  from = c(
    "data", "cdisc",
    "global_filter", "global_filter", "global_filter", "global_filter",
    "dx_pull", "dx_pull", "dx_pull",
    "dx_select", "dx_search",
    "pop_demog",
    "ae_flat", "public_pike", "ae_flat", "ae_flat",
    "fretful_nandoo", "radiant_gaur",
    "blank_nautilus", "wayward_cardinal",
    "pt_semi"
  ),
  to = c(
    "cdisc", "global_filter",
    "dx_pull", "ae_flat", "fretful_nandoo", "blank_nautilus",
    "pop_demog", "dx_select", "bony_urson",
    "dx_search", "dx_download",
    "pop_demog_tbl",
    "public_pike", "ae_heatmap_tbl", "loving_noddy", "gantt_chart",
    "radiant_gaur", "traj_chart",
    "wayward_cardinal", "cut_wryneck",
    "pt_profile"
  )
)

# Drill-collector edges into the pt_latest aggregator (numbered variadic
# inputs) and the two named inputs of pt_semi (the dm on `data`, the clicked
# subject ids on `by`).
links_drill <- blockr.core::links(
  from  = c("traj_chart", "gantt_chart", "cut_wryneck", "loving_noddy",
            "cdisc", "pt_latest"),
  to    = c("pt_latest", "pt_latest", "pt_latest", "pt_latest",
            "pt_semi", "pt_semi"),
  input = c("1", "2", "3", "4", "data", "by")
)

links <- c(links_main, links_drill)

# --- Layout (5 workflow views + a Setup/DAG view) --------------------------
# Membership (which panels a view holds, plus its display name) and geometry
# (nesting, tab groups, sizes) are two separate slots: `views` and `grids`.
# `panels(..., active =)` picks the tab that OPENS -- the default is the first
# one, which lands on the raw head of the chain rather than what the view is
# about, so every tab group names its own open tab.
dg <- blockr.dock::dock_grid
pn <- blockr.dock::panels
gr <- blockr.dock::group
vw <- blockr.dock::dock_view

views <- list(
  Setup = vw(c("data", "cdisc", blockr.dock::ext("dag"))),
  Population = vw(c("global_filter", "pop_demog", "bony_urson",
                    "pop_demog_tbl")),
  DataExplorer = vw(c("global_filter", "dx_pull", "dx_select", "dx_search",
                      "dx_download"), name = "Data Explorer"),
  AdverseEvents = vw(c("global_filter", "ae_heatmap_tbl", "loving_noddy",
                       "gantt_chart", "pt_profile"), name = "Adverse Events"),
  Lab = vw(c("global_filter", "radiant_gaur", "traj_chart", "pt_profile")),
  VitalSigns = vw(c("global_filter", "wayward_cardinal", "cut_wryneck",
                    "pt_profile"), name = "Vital Signs")
)

grids <- list(
  Setup = dg("data", "cdisc", blockr.dock::ext("dag")),
  # The rendered Table 1, not the summary block's raw preview.
  Population = dg("global_filter",
                  pn("pop_demog", "bony_urson", "pop_demog_tbl",
                     active = "pop_demog_tbl"),
                  sizes = c(1, 4)),
  # The searchable table, not the raw pull -- and not the download button
  # that follows it.
  DataExplorer = dg("global_filter",
                    pn("dx_pull", "dx_select", "dx_search", "dx_download",
                       active = "dx_search"),
                    sizes = c(1, 4)),
  AdverseEvents = dg("global_filter",
                     pn("ae_heatmap_tbl", "loving_noddy", "gantt_chart",
                        active = "gantt_chart"),
                     "pt_profile", sizes = c(1, 2, 2)),
  Lab = dg(gr("global_filter", "radiant_gaur"), "traj_chart", "pt_profile",
           sizes = c(1, 2, 2)),
  VitalSigns = dg(gr("global_filter", "wayward_cardinal"), "cut_wryneck",
                  "pt_profile", sizes = c(1, 2, 2))
)

board <- blockr.dock::new_dock_board(
  blocks = blocks,
  links = links,
  extensions = list(blockr.dag::new_dag_extension()),
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

# --- Serialize -------------------------------------------------------------
json <- jsonlite::toJSON(blockr.core::blockr_ser(board), null = "null")
writeLines(json, json_path)
cat("Wrote", json_path, "\n")

# Round-trip sanity check.
rt <- jsonlite::fromJSON(json_path, simplifyDataFrame = FALSE,
                         simplifyMatrix = FALSE)
board2 <- blockr.core::blockr_deser(rt)
cat("Round-trip OK; blocks =",
    length(blockr.core::board_blocks(board2)), "\n")
