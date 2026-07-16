# Patient Profile on a REAL SDTM study (pharmaversesdtm) — domain table
# names (dm/ae/vs/lb/cm), SDTM column spellings, character *DTC timestamps.
#
#   data (static SDTM dm)  ->  profile
#
# What to look at:
#   - The picker labels carry the arm from the SDTM `dm` domain's ACTARM —
#     no declaration needed, the arm role's default is the actual arm.
#   - Patient Overview / Adverse Events / Concomitant Medications render
#     from ae/cm via the dm-wide normalization catalog (AESTDTC -> ASTDT,
#     CMSTDTC -> ASTDT, coerced to Date, never just renamed).
#   - Labs and vitals findings groups source from the combined `lb` / `vs`.
#   - Gear > Study variables reports the resolved roles (Arm: ACTARM,
#     Severity: AESEV); Data coverage reports the ADAS/NPI-X vizs as
#     unavailable — the QS split is a derivation, not a renaming, so a raw
#     SDTM study genuinely does not carry those tables.
#
# Run from the workspace root (or from the package dir):
#   Rscript blockr.pharma/dev/patient-profile-sdtm.R          # port 3838
#   BLOCKR_PORT=4000 Rscript blockr.pharma/dev/patient-profile-sdtm.R
#
# NOTE: load_all() ALL of them, never a mix (assets + extension contracts;
# see patient-profile-sidebar.R for the long form of why).
root <- if (file.exists("blockr.pharma/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.theme", "blockr.dplyr", "blockr.dm",
            "blockr.pharma", "blockr.dag", "blockr.dock")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

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
  # Match prod: construct a block only once it is needed (see
  # patient-profile-minimal.R for why dev must run the deployed code path).
  blockr.background_construction_delay = Inf
)
message("Patient profile SDTM demo on http://127.0.0.1:", pp_port, "/")

serve(
  new_dock_board(
    blocks = c(
      data = new_dm_example_block(dataset = "pharmaversesdtm"),
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
      Pipeline = dock_grid(ext("dag")),
      Data = dock_grid("data")
    ),
    active = "Profile"
  )
)
