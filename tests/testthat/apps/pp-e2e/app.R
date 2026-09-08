# Browser fixture for the patient profile block (tests/testthat/test-shinytest2.R).
#
# The same three-block chain dev/patient-profile-cohort.R serves, on a plain
# board rather than a dock board: the tests drive the block's own JS, and the
# dock chrome adds panels and a layout restore the pins do not need.
#
#   data (safetyData ADaM)  ->  cdisc (dm keys)  ->  profile (254 subjects)
#
# shinytest2 runs this in a child R process that loads the INSTALLED
# packages, so `R CMD INSTALL --no-docs blockr.pharma` before running the
# tests against edited source.

library(blockr.core)
library(blockr.dm)
library(blockr.pharma)
library(safetyData)

serve(
  new_board(
    blocks = c(
      data = new_dm_example_block(dataset = "safetydata_adam"),
      cdisc = new_cdisc_dm_block(),
      profile = new_patient_profile_block(
        selected = c("patient_overview", "ae_gantt", "adlbc_all")
      )
    ),
    links = list(
      list(from = "data", to = "cdisc", input = "data"),
      list(from = "cdisc", to = "profile", input = "data")
    ),
    # safetyData ships no ACTARM; TRT01A is the actual arm.
    options = c(
      default_board_options(),
      new_board_options(new_study_roles_option(arm = "TRT01A"))
    )
  )
)
