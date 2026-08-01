# Patient Profile on a COMBINATION REGIMEN — the shape clinical review
# reported against: two drugs, one dosed D1 and D8 of each cycle, the other
# D1 only, on an SDTM-style `ex` domain carrying VISIT ("CYCLE 2 DAY 1").
#
#   Rscript blockr.pharma/dev/patient-profile-combination.R
#   Rscript blockr.pharma/dev/patient-profile-combination.R 3901   # pick a port
#
# WHAT TO LOOK AT
#   - The treatment lane holds one SLOT PER DRUG. Both same-day infusions are
#     drawn and both can be hovered; one lane drew them on top of each other
#     and the tooltip answered for whichever painted last.
#   - Hover a dose: the timepoint is that row's own VISIT, printed as the study
#     wrote it, never a calendar we worked out. The infusions dated three days
#     after their cycle opened still say CYCLE n DAY 8, because that is what
#     the study recorded, and the date beside it says when it happened.
#   - Hover a lab point: same rule, so an unscheduled draw says UNSCHEDULED
#     rather than going quiet.
#   - The pre-dose labs here are drawn 0-3 days AHEAD of each infusion, which
#     is the routine that made every dose read D2/D3/D4 when the cycle
#     calendar was derived from them.
#
# NOTE: load_all() ALL of them, never a mix (assets + extension contracts).
root <- if (file.exists("blockr.pharma/DESCRIPTION")) "." else ".."

options(dm.use_igraph = FALSE)

for (p in c("blockr.core", "blockr.theme", "blockr.dplyr", "blockr.dm",
            "blockr.extra", "blockr.pharma", "blockr.dag", "blockr.dock")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

host <- "0.0.0.0"
# Positional arg wins, then BLOCKR_PORT, then 3838 (the only port the dev
# container forwards, so the only shareable one). Falls back to a random port
# when 3838 is busy -- on the host it always is, because Docker publishes the
# container's 3838 onto it.
port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else ""
  if (nzchar(raw)) {
    p <- suppressWarnings(as.integer(raw))
    if (is.na(p)) stop("Not a port: ", raw, call. = FALSE)
    return(p)
  }
  tryCatch(
    httpuv::randomPort(min = 3838L, max = 3838L, n = 1L, host = host),
    error = function(e) httpuv::randomPort(host = host)
  )
})
options(
  shiny.port = port,
  shiny.host = host,
  "g6R.preserve_elements_position" = TRUE,
  blockr.background_construction_delay = Inf
)

# Every call inside a block's fn is qualified: the fn evaluates against the
# default-package chain, never the search path.
build_fn <- '
function(...) {
  cyc <- 1:6
  d1 <- as.Date("2024-01-08") + (cyc - 1L) * 21L
  # D8 infusions, two of them genuinely slipped (the delay clinicians expect
  # to keep seeing, as a date; the row still says DAY 8).
  d8 <- d1 + 7L + c(0L, 1L, 3L, 0L, 2L, 0L)
  # Safety labs for the DAY 1 visit, drawn pre-dose.
  lead <- c(0L, 1L, 3L, 2L, 1L, 0L)

  ex <- data.frame(
    USUBJID = "SUBJ-001",
    EXTRT = rep(c("STUDY DRUG A", "STUDY DRUG B"), c(12L, 6L)),
    EXDOSE = rep(c(800, 200), c(12L, 6L)),
    EXDOSU = "mg",
    VISIT = c(sprintf("CYCLE %d DAY 1", cyc), sprintf("CYCLE %d DAY 8", cyc),
              sprintf("CYCLE %d DAY 1", cyc)),
    EXSTDTC = as.character(c(d1, d8, d1)),
    EXENDTC = as.character(c(d1, d8, d1)),
    stringsAsFactors = FALSE
  )
  lb <- data.frame(
    USUBJID = "SUBJ-001",
    VISIT = c(sprintf("CYCLE %d DAY 1", cyc), sprintf("CYCLE %d DAY 8", cyc)),
    LBDTC = as.character(c(d1 - lead, d8)),
    LBTESTCD = "ALT", LBTEST = "Alanine Aminotransferase",
    LBSTRESN = c(28, 34, 51, 44, 39, 61, 30, 36, 48, 42, 37, 55),
    stringsAsFactors = FALSE
  )
  ae <- data.frame(
    USUBJID = "SUBJ-001",
    AEDECOD = c("Fatigue", "Rash", "Nausea"),
    AESEV = c("MILD", "MODERATE", "MILD"),
    AESTDTC = as.character(d1[c(1L, 2L, 4L)] + c(2L, 5L, 1L)),
    AEENDTC = as.character(d1[c(1L, 3L, 5L)] + c(9L, 4L, 6L)),
    stringsAsFactors = FALSE
  )
  dm <- data.frame(
    USUBJID = "SUBJ-001",
    ACTARM = "Study Drug A 800 mg + Study Drug B 200 mg",
    RFXSTDTC = as.character(min(d1)), RFXENDTC = as.character(max(d8)),
    stringsAsFactors = FALSE
  )
  dm::dm(dm = dm, ex = ex, lb = lb, ae = ae)
}
'

blocks <- c(
  data = new_function_var_block(fn = build_fn),
  profile = new_patient_profile_block(
    selected = c("patient_overview", "cycle_lane", "ae_gantt", "adlb_all")
  )
)

message("Patient profile COMBINATION on http://127.0.0.1:", port, "/")

serve(
  new_dock_board(
    blocks = blocks,
    links = list(list(from = "data", to = "profile", input = "data")),
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
