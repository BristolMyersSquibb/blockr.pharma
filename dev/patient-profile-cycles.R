# Patient Profile with TREATMENT CYCLES — on demo data or on a real study.
#
# Usage (serves on 3838, or the next free port if that is taken):
#
#   Rscript blockr.pharma/dev/patient-profile-cycles.R
#       pharmaverseadam, with a 21-day cycle schedule SYNTHESIZED into
#       adlb$AVISIT. Runs anywhere. A preview of the chart, and NOT evidence
#       about any study -- the demo is week-based and speaks no cycles.
#
#   Rscript blockr.pharma/dev/patient-profile-cycles.R ca-244-0001
#   Rscript blockr.pharma/dev/patient-profile-cycles.R /path/to/study/prod/
#       The real thing: reads the study through blockr.sandbox's
#       read_study_dm() and shows the cycles the study actually carries.
#       Needs the CDEx data share, so this form only runs where that is
#       mounted (Workbench / Connect), not in the devcontainer.
#
# WHAT TO LOOK AT
#   - Treatment Cycles: alternating bands, C1/C2/... centred, x-aligned with
#     every lane above and below -- the lanes share time_range/ref_ms/mode, so
#     the band is a ruler you can sight up from. A held cycle is a visibly
#     wider band; that is the point.
#   - Dashed border = that cycle's DAY 1 visit was missing and its start is
#     back-calculated (pp-cycle.R). Common on the demo, ~0.4% on CA-244.
#   - Hover an AE bar: "D25 (C2 D4)" -- the cycle rides BEHIND the study day,
#     it never replaces it.
#   - Flip the gear's timeline between relative day and date: the bands stay
#     put and the cycle keeps riding in the tooltip. It is a label and a lane,
#     not a third mode, so relative day cannot be lost.
#   - On a study that is not dosed in cycles there is no card at all, and
#     nothing in Data coverage either (see pp_cycle_vizs()).
#
# NOTE: load_all() ALL of them, never a mix (assets + extension contracts;
# see patient-profile-sidebar.R for the long form of why).
root <- if (file.exists("blockr.pharma/DESCRIPTION")) "." else ".."

# MUST stay above the load_all() loop, mirroring prod (blockr.sandbox/app.R):
# dm reads this option once, from its .onLoad, and latches the choice by
# rebinding its graph functions. Set it afterwards and it is silently ignored.
options(dm.use_igraph = FALSE)

for (p in c("blockr.core", "blockr.theme", "blockr.dplyr", "blockr.dm",
            "blockr.extra", "blockr.pharma", "blockr.dag", "blockr.dock")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

# Prefer 3838 (the only port the devcontainer forwards), but fall back to a
# free one so a second launch does not die on "address already in use"
# (mirrors blockr.cdex/dev/run-view.R).
host <- "0.0.0.0"
port <- tryCatch(
  httpuv::randomPort(min = 3838L, max = 3838L, n = 1L, host = host),
  error = function(e) httpuv::randomPort(host = host)
)
options(
  shiny.port = port,
  shiny.host = host,
  "g6R.preserve_elements_position" = TRUE,
  # Match prod: construct a block only once it is needed (see
  # patient-profile-minimal.R for why dev must run the deployed code path).
  blockr.background_construction_delay = Inf
)

study <- commandArgs(trailingOnly = TRUE)[1L]

# Every call inside a block's fn is qualified: the fn evaluates against the
# default-package chain, never the search path, so a bare dm() or mutate() is
# not found there even though it works at the console.
if (is.na(study)) {
  message("No study given -> pharmaverseadam with SYNTHESIZED cycles.")
  message("For real cycles: Rscript blockr.pharma/dev/patient-profile-cycles.R <study-id|path>")
  library(pharmaverseadam)

  # A function block on purpose: it is the shape the real ad-hoc plugin takes,
  # so this doubles as its template. On CA-244 the body is unnecessary --
  # VISIT already says "CYCLE 2 DAY 1" and nothing needs synthesizing.
  synth_fn <- '
function(data) {
  tbls <- dm::dm_get_tables(data)
  lb <- as.data.frame(tbls$adlb)
  cyc <- ((lb$ADY - 1L) %/% 21L) + 1L
  day <- ((lb$ADY - 1L) %% 21L) + 1L
  hit <- !is.na(lb$ADY) & lb$ADY > 0 & day %in% c(1L, 8L, 15L) & cyc <= 8L
  lb$AVISIT <- as.character(lb$AVISIT)
  lb$AVISIT[hit] <- sprintf("CYCLE %d DAY %d", cyc[hit], day[hit])
  tbls$adlb <- lb
  do.call(dm::dm, lapply(tbls, as.data.frame))
}
'
  blocks <- c(
    data = new_dm_example_block(dataset = "pharmaverseadam"),
    cycles = new_function_block(fn = synth_fn),
    profile = new_patient_profile_block(
      selected = c("patient_overview", "cycle_lane", "ae_gantt", "cm_gantt")
    )
  )
  links <- list(
    list(from = "data", to = "cycles", input = "data"),
    list(from = "cycles", to = "profile", input = "data")
  )
  data_view <- c("data", "cycles")

} else {
  # read_study_dm() is sourced, not exported (blockr.sandbox's R/ files are
  # source()d by its app.R rather than namespaced).
  cdex_data <- file.path(root, "blockr.sandbox", "R", "cdex-data.R")
  if (!file.exists(cdex_data)) {
    stop("need blockr.sandbox for read_study_dm(): ", cdex_data, call. = FALSE)
  }
  source(cdex_data)

  # A bare study id needs the mapping; a raw path is read_study_dm()'s
  # documented fallback and needs nothing. blockr.sandbox/app.R is the source
  # of truth for these -- mirrored, not owned, so check there if an id fails.
  if (!length(getOption("cedx.study_paths", character()))) {
    options(cedx.study_paths = c(
      "ca-244-0001" = "/cdrsce_cda/non-std/cdex-wks/data/ca-244-0001/prod/",
      "ca-242-0001" = "/cdrsce_cda/non-std/cdex-wks/data/ca-242-0001/prod/",
      "ca-230-1019" = "/cdrsce_cda/non-std/cdex-wks/data/ca-230-1019/prod/"
    ))
  }
  options(cedx.read_study_dm = read_study_dm)

  # Fail here rather than inside a block: a red dot in the UI says far less
  # than the error read_study_dm() raises (it names the known study ids).
  message("Reading study: ", study, " ...")
  invisible(getOption("cedx.read_study_dm")(study))
  message("  ok")

  # The variadic block is the CDEx read-block pattern: getOption() is the
  # base-R bridge that IS visible from a block's eval env, where a
  # globalenv-sourced read_study_dm would not be.
  blocks <- c(
    data = new_function_var_block(
      fn = sprintf('function(...) getOption("cedx.read_study_dm")("%s")', study)
    ),
    profile = new_patient_profile_block(
      selected = c("patient_overview", "cycle_lane", "ae_gantt", "cm_gantt")
    )
  )
  links <- list(list(from = "data", to = "profile", input = "data"))
  data_view <- "data"
}

message("Patient profile CYCLES on http://127.0.0.1:", port, "/")

serve(
  new_dock_board(
    blocks = blocks,
    links = links,
    extensions = blockr.dag::new_dag_extension(),
    options = c(
      dock_board_options(),
      new_board_options(new_study_roles_option())
    ),
    grids = list(
      Profile = dock_grid("profile"),
      Pipeline = dock_grid(ext("dag")),
      Data = do.call(dock_grid, as.list(data_view))
    ),
    active = "Profile"
  )
)
