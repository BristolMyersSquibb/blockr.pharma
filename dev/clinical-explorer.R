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

options(shiny.port = 3838, shiny.host = "0.0.0.0")

dev_local <- TRUE
source("blockr.pharma/inst/examples/clinical-explorer.R")
