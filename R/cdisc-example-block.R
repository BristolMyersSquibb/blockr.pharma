#' CDISC Example Block
#'
#' A data block that loads a CDISC example study as a keyed `dm`: ADSL (or DM
#' for SDTM) carries USUBJID as primary key, every other table refers to it.
#' The dropdown shows only datasets whose data package is installed.
#'
#' @param dataset Character, the ID of the study to load. One of
#'   `"pharmaverseadam"` (ADaM, from \pkg{pharmaverseadam}),
#'   `"pharmaversesdtm"` (raw SDTM domains, from \pkg{pharmaversesdtm}) and
#'   `"safetydata_adam"` (ADaM, from \pkg{safetyData}).
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @return A data block of class `c("cdisc_example_block", "dm_block")`. The
#'   `dm_block` class lets blockr.dm's `block_ui()` and `block_output()`
#'   methods render it.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_cdisc_example_block())
#' }
#'
#' @export
new_cdisc_example_block <- function(dataset = "pharmaverseadam", ...) {

  # Resolve the initial value once so the UI can use a plain string even when
  # `dataset` is already a reactiveVal (injected by external_ctrl).
  init_dataset <- if (inherits(dataset, "reactiveVal")) {
    shiny::isolate(dataset())
  } else {
    dataset
  }

  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        # If dataset is already a reactiveVal (injected by external_ctrl),
        # reuse it; otherwise create a fresh one.
        dat <- if (inherits(dataset, "reactiveVal")) {
          dataset
        } else {
          shiny::reactiveVal(dataset)
        }

        shiny::observeEvent(
          shiny::req(input$dataset),
          dat(input$dataset)
        )

        shiny::observeEvent(
          shiny::req(dat()),
          ignoreInit = TRUE,
          {
            if (!identical(dat(), input$dataset)) {
              shiny::updateSelectInput(
                session, "dataset",
                choices = cdisc_example_choices(),
                selected = dat()
              )
            }
          }
        )

        list(
          expr = shiny::reactive({
            selected <- dat()
            shiny::req(nzchar(selected))
            cdisc_example_expr(selected)
          }),
          state = list(
            dataset = dat
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::selectInput(
        inputId = ns("dataset"),
        label = "CDISC Dataset",
        choices = cdisc_example_choices(),
        selected = init_dataset
      )
    },
    allow_empty_state = TRUE,
    class = c("cdisc_example_block", "dm_block"),
    external_ctrl = "dataset",
    ...
  )
}

#' @importFrom blockr.core block_render_trigger
#' @method block_render_trigger cdisc_example_block
#' @export
block_render_trigger.cdisc_example_block <- function(
  x,
  session = blockr.core::get_session()
) {
  NULL
}

# -- Choices & dispatch -------------------------------------------------------

#' List available CDISC example datasets
#'
#' Returns a named character vector (display name -> id) of the CDISC example
#' studies whose data package is installed.
#'
#' @return Named character vector.
#' @keywords internal
cdisc_example_choices <- function() {
  choices <- character()

  if (requireNamespace("pharmaverseadam", quietly = TRUE)) {
    choices[["Pharmaverse ADaM (pharmaverseadam)"]] <- "pharmaverseadam"
  }
  if (requireNamespace("pharmaversesdtm", quietly = TRUE)) {
    choices[["Pharmaverse SDTM (pharmaversesdtm)"]] <- "pharmaversesdtm"
  }
  if (requireNamespace("safetyData", quietly = TRUE)) {
    choices[["Safety ADaM (safetyData)"]] <- "safetydata_adam"
  }

  choices
}

#' Get a quoted expression for a CDISC example
#'
#' @param id Character, the dataset ID.
#' @return A quoted R expression that evaluates to a `dm` object.
#' @keywords internal
cdisc_example_expr <- function(id) {
  switch(id,
    pharmaverseadam = pharmaverseadam_expr(),
    pharmaversesdtm = pharmaversesdtm_expr(),
    safetydata_adam = safetydata_adam_expr(),
    stop("Unknown CDISC example: ", id)
  )
}

# -- Expression builders ------------------------------------------------------

pharmaverseadam_expr <- function() {
  quote(local({
    adsl <- pharmaverseadam::adsl
    adae <- pharmaverseadam::adae
    adlb <- pharmaverseadam::adlb
    advs <- pharmaverseadam::advs
    adcm <- pharmaverseadam::adcm
    adeg <- pharmaverseadam::adeg
    adex <- pharmaverseadam::adex

    result <- dm::dm(
      adsl = adsl, adae = adae, adlb = adlb, advs = advs,
      adcm = adcm, adeg = adeg, adex = adex
    )
    result <- dm::dm_add_pk(result, adsl, USUBJID)
    result <- dm::dm_add_fk(result, adae, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adlb, USUBJID, adsl)
    result <- dm::dm_add_fk(result, advs, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adcm, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adeg, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adex, USUBJID, adsl)
    result
  }))
}

# Raw SDTM domains (not ADaM): domain table names (dm/ae/vs/lb/cm) and SDTM
# column spellings (AESTDTC, VSTESTCD, LBSTRESN, ...). The patient profile
# reconciles these with its ADaM names in its own normalization; this block
# hands over the study as shipped.
pharmaversesdtm_expr <- function() {
  quote(local({
    dm <- pharmaversesdtm::dm
    ae <- pharmaversesdtm::ae
    vs <- pharmaversesdtm::vs
    lb <- pharmaversesdtm::lb
    cm <- pharmaversesdtm::cm
    eg <- pharmaversesdtm::eg
    ex <- pharmaversesdtm::ex

    result <- dm::dm(
      dm = dm, ae = ae, vs = vs, lb = lb, cm = cm, eg = eg, ex = ex
    )
    result <- dm::dm_add_pk(result, dm, USUBJID)
    result <- dm::dm_add_fk(result, ae, USUBJID, dm)
    result <- dm::dm_add_fk(result, vs, USUBJID, dm)
    result <- dm::dm_add_fk(result, lb, USUBJID, dm)
    result <- dm::dm_add_fk(result, cm, USUBJID, dm)
    result <- dm::dm_add_fk(result, eg, USUBJID, dm)
    result <- dm::dm_add_fk(result, ex, USUBJID, dm)
    result
  }))
}

safetydata_adam_expr <- function() {
  quote(local({
    adsl     <- safetyData::adam_adsl
    adae     <- safetyData::adam_adae
    adlbc    <- safetyData::adam_adlbc
    adlbh    <- safetyData::adam_adlbh
    adlbhy   <- safetyData::adam_adlbhy
    adqsadas <- safetyData::adam_adqsadas
    adqscibc <- safetyData::adam_adqscibc
    adqsnpix <- safetyData::adam_adqsnpix
    adtte    <- safetyData::adam_adtte
    advs     <- safetyData::adam_advs

    result <- dm::dm(
      adsl = adsl, adae = adae, adlbc = adlbc, adlbh = adlbh,
      adlbhy = adlbhy, adqsadas = adqsadas, adqscibc = adqscibc,
      adqsnpix = adqsnpix, adtte = adtte, advs = advs
    )
    result <- dm::dm_add_pk(result, adsl, USUBJID)
    result <- dm::dm_add_fk(result, adae, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adlbc, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adlbh, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adlbhy, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adqsadas, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adqscibc, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adqsnpix, USUBJID, adsl)
    result <- dm::dm_add_fk(result, adtte, USUBJID, adsl)
    result <- dm::dm_add_fk(result, advs, USUBJID, adsl)
    result
  }))
}
