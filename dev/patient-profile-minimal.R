# MINIMAL dock + core dashboard: two blocks, one link.
#
#   data (pharmaverseadam via blockr.dm's example block)  ->  profile
#
# The smallest board that exercises the full product stack (dock views, dag
# extension, board options, lazy eval) against the patient profile. No
# cdisc block: pharmaverseadam ships keyed ADaM tables (adsl/adae/adcm/...)
# and carries ACTARM, so the arm role resolves without a declaration and
# the Concomitant Medications panel has data.
#
# Run from the workspace root (or from the package dir):
#   Rscript blockr.pharma/dev/patient-profile-minimal.R          # port 3838
#   BLOCKR_PORT=4000 Rscript blockr.pharma/dev/patient-profile-minimal.R
#
# NOTE: load_all() ALL of them, never a mix (assets + extension contracts;
# see patient-profile-sidebar.R for the long form of why).
root <- if (file.exists("blockr.pharma/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.theme", "blockr.dplyr", "blockr.dm",
            "blockr.pharma", "blockr.dag", "blockr.dock")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

library(pharmaverseadam)

# Port: positional arg wins, then BLOCKR_PORT, then 3838 (the only port the
# devcontainer forwards, so the one worth defaulting to).
pp_port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else "3838"
  port <- suppressWarnings(as.integer(raw))
  if (is.na(port)) stop("Not a port: ", raw, call. = FALSE)
  port
})

options(
  shiny.port = pp_port,
  "g6R.preserve_elements_position" = TRUE,
  # Match prod (blockr.sandbox/app.R): construct a block only once it is
  # needed (on screen, or on a view switch). Unset, core defaults to 50ms and
  # runs the paced background construction train instead -- a different code
  # path to the one deployed. Parity with prod is the whole reason; it is not
  # known to fix any particular symptom here.
  blockr.background_construction_delay = Inf
)
message("Patient profile minimal demo on http://127.0.0.1:", pp_port, "/")

serve(
  new_dock_board(
    blocks = c(
      data = new_dm_example_block(dataset = "pharmaverseadam"),
      profile = new_patient_profile_block(
        selected = c("patient_overview", "ae_gantt", "cm_gantt")
      )
    ),
    links = list(
      list(from = "data", to = "profile", input = "data")
    ),
    extensions = blockr.dag::new_dag_extension(),
    options = c(
      dock_board_options(),
      new_board_options(new_study_roles_option())
    ),
    grids = list(
      Profile = dock_grid("profile"),
      # ext("dag"), not "dag_extension": dock #318 moved an extension's id onto
      # its container, so the id is the list key ("dag" -- extension_key()
      # strips the `_extension` suffix from an unnamed one) and the panel is
      # `ext_panel-dag`. A bare string is sugar that resolves BLOCK-first, so
      # "dag_extension" does not error -- it silently yields a Pipeline view
      # with NO members and the DAG never appears.
      Pipeline = dock_grid(ext("dag")),
      Data = dock_grid("data")
    ),
    active = "Profile"
  )
)
