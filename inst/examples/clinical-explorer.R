# Clinical Explorer demo — local launcher.
#
# Deserializes the bundled board JSON (the same demo shown at R/Medicine 2026)
# and serves it. Run with:
#
#   source(system.file("examples/clinical-explorer.R", package = "blockr.pharma"))

library(blockr.core)
library(blockr.dock)
library(blockr.dag)
library(blockr.dplyr)
library(blockr.ggplot)
library(blockr.io)
library(blockr.bi)
library(blockr.dm)
library(blockr.pharma)
library(pharmaverseadam)

# The pinned demo board references blocks from these packages, so they must be
# attached for blockr_deser() to find their constructors. Drop them only if
# the JSON is rebuilt without those blocks.
library(blockr.react)    # new_react_extension
library(blockr.extra)    # new_latest_block, new_search_block
library(blockr.sandbox)  # ae_heatmap, drilldown_chart, sandbox_patient_profile

options(
  blockr.dock_is_locked = FALSE,
  blockr.eval_parent_env = asNamespace("stats")
)

plugins <- list()
if (requireNamespace("blockr.ai", quietly = TRUE)) {
  library(blockr.ai)
  plugins <- c(plugins, list(ai_ctrl_block()))
}

json_path <- system.file(
  "examples/clinical-explorer.json",
  package = "blockr.pharma"
)
stopifnot(nzchar(json_path))

dashboard_json <- jsonlite::fromJSON(
  json_path,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)

board <- blockr.core::blockr_deser(dashboard_json)

serve(
  board,
  plugins = if (length(plugins)) custom_plugins(plugins) else NULL
)
