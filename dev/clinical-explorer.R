# Run the Clinical Explorer board against LOCAL source checkouts (your latest
# uncommitted changes to any blockr package). This is the pkgload::load_all()
# counterpart of the shipped, library()-based inst/examples/clinical-explorer.R:
# it just flips the loader and sources it, so the two can never drift.
#
# Run from an R session at the workspace root:
#   source("blockr.pharma/dev/clinical-explorer.R")
#
# (End users without the source checkouts run the shipped copy instead:
#   source(system.file("examples/clinical-explorer.R", package = "blockr.pharma")))

# blockr_port() is the devcontainer helper returning the first free port in
# 3838:3847, the range forwarded to the host. Never hardcode one: a second
# session on 3838 would collide, and a port outside the range serves fine and
# is invisible from the browser.
options(
  shiny.port = if (exists("blockr_port")) blockr_port() else 3838L,
  shiny.host = "0.0.0.0"
)
message("clinical-explorer: http://127.0.0.1:", getOption("shiny.port"), "/")

dev_local <- TRUE
source("blockr.pharma/inst/examples/clinical-explorer.R")
