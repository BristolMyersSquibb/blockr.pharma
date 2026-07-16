# Board-level study role declarations: which column plays which clinical
# part, carried as the "study_roles" board option and read by consumers that
# have nobody to ask (the patient profile). Spec: blockr.design
# open/study-metadata (3-design.md "Shape", 4-implementation.md "Roles to
# start with"). Structural sibling of blockr.theme's scale map; it lives
# here because the roles are clinical.
#
# Three fields, uniform semantics (undeclared = the package convention;
# declared-but-missing = a named error, never a fallback):
#
#   arm       -- ADSL treatment/arm label column. Default ACTARM.
#   severity  -- ADAE severity column. Default: detect AETOXGR over AESEV.
#   timeline  -- ADSL column anchoring relative-day mode. Default TRTSDT
#                (which pp_normalize_dm() derives from RFXSTDTC/RFSTDTC for
#                SDTM-shaped studies).
#
# Deliberately NOT fields:
#   - table aliases: the SDTM domain names (ae, vs, lb, cm, dm, ...) resolve
#     through pp_table_catalog() with no declaration; a table under a
#     genuinely custom name is an upstream rename -- a block's job, not
#     metadata (see what-is-not-metadata.md).
#   - level/display order (a mutate, or the scale map's job) and PARAMCD
#     recodes (a value mutate) -- both fail the "can a block do it?" test.
#
# IMPORTANT for future fields: the transform runs on EVERY editor input
# change, inside an observer -- a stop() reachable from typed input kills
# the whole Shiny session mid-keystroke. Normalization must be total over
# anything a text field can hold; validation that can fail belongs at
# RESOLUTION (pp_resolve_roles() and friends), where it surfaces as a named
# block error instead.

#' Study roles: a board option
#'
#' Declares, per study board, which columns carry the profile's clinical
#' roles. The value serializes with the board, is editable in the settings
#' sidebar (category "Study"), and is deliberately not block state: a
#' constructor formal would round-trip through `state` and become reachable
#' by the AI assistant, and these are study facts, not analysis choices.
#' Board options are structurally invisible to the assistant.
#'
#' Every field is optional. An empty field means the package convention; a
#' declared column that the data does not carry is a named error at
#' resolution, never a fallback -- a study that declared and got it wrong
#' must stop, not draw something plausible.
#'
#' @param arm ADSL arm column; `""` = undeclared (convention: `ACTARM`).
#' @param severity ADAE severity column; `""` = undeclared (convention:
#'   detect `AETOXGR`, then `AESEV`).
#' @param timeline ADSL column anchoring relative-day mode; `""` =
#'   undeclared (convention: `TRTSDT`).
#' @param category Settings sidebar category
#' @param ... Forwarded to [blockr.core::new_board_option()]
#'
#' @return A `board_option` with id `"study_roles"`.
#'
#' @export
new_study_roles_option <- function(arm = "", severity = "", timeline = "",
                                   category = "Study", ...) {
  value <- study_roles_normalize(list(
    arm = arm, severity = severity, timeline = timeline
  ))

  # The placeholder names the convention; no helper text below the field
  # (tried, and it read as noise next to three short inputs).
  field <- function(id, input, label, placeholder) {
    shiny::textInput(
      shiny::NS(id, input),
      label,
      value = value[[sub("^study_", "", input)]],
      placeholder = placeholder
    )
  }

  blockr.core::new_board_option(
    id = "study_roles",
    default = value,
    ui = function(id) {
      htmltools::tagList(
        field(id, "study_arm", "Arm column", "ACTARM (default)"),
        field(id, "study_severity", "Severity column",
              "AETOXGR / AESEV (detected)"),
        field(id, "study_timeline", "Timeline reference",
              "TRTSDT (default)")
      )
    },
    server = function(board, ..., session) {
      # Sync the inputs when the value changes from elsewhere (board
      # restore, a programmatic set); the write direction is the option's
      # update trigger (the three inputs), no observer needed here.
      shiny::observeEvent(
        blockr.core::get_board_option_or_null("study_roles", session),
        {
          val <- study_roles_normalize(
            blockr.core::get_board_option_value("study_roles", session)
          )
          shiny::updateTextInput(session, "study_arm", value = val$arm)
          shiny::updateTextInput(session, "study_severity",
                                 value = val$severity)
          shiny::updateTextInput(session, "study_timeline",
                                 value = val$timeline)
        }
      )
    },
    update_trigger = c("study_arm", "study_severity", "study_timeline"),
    transform = function(x) study_roles_normalize(x),
    category = category,
    ...
  )
}

#' Normalize a study_roles option value
#'
#' TOTAL over the shapes a value can arrive in: the constructor's arguments,
#' the update trigger's `study_*`-keyed input list, and the `list()` shapes
#' JSON deserialization hands back. `""` is the undeclared state. Never
#' errors on typed input (see the file header); anything unusable collapses
#' to `""` here and, if a declared name is wrong, fails loudly at
#' resolution instead.
#'
#' @param x Raw value.
#' @return `list(arm, severity, timeline)`.
#' @noRd
study_roles_normalize <- function(x) {
  if (is.null(x)) x <- list()
  stopifnot(is.list(x))

  # accept both the ctor keys and the editor's input keys
  key <- function(nm) x[[nm]] %||% x[[paste0("study_", nm)]]

  scalar <- function(v) {
    v <- trimws(as.character(unlist(v, use.names = FALSE)))
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v) == 1L) v else ""
  }

  list(
    arm = scalar(key("arm")),
    severity = scalar(key("severity")),
    timeline = scalar(key("timeline"))
  )
}

#' Reactive reader for the board's declared study roles
#'
#' Mirrors blockr.theme::board_scale_map(): call once in a block server,
#' read the returned reactive per resolution. Yields the normalized value
#' with `NULL` (not `""`) for undeclared roles, so consumers can `%||%`
#' their conventions, or `NULL` when the board carries no "study_roles"
#' option at all.
#'
#' @return A reactive yielding `list(arm, severity, timeline)` or `NULL`.
#' @noRd
board_study_roles <- function() {
  shiny::reactive({
    val <- blockr.core::get_board_option_or_null(
      "study_roles", blockr.core::get_session()
    )
    if (is.null(val)) return(NULL)
    val <- study_roles_normalize(val)
    list(
      arm = if (nzchar(val$arm)) val$arm,
      severity = if (nzchar(val$severity)) val$severity,
      timeline = if (nzchar(val$timeline)) val$timeline
    )
  })
}
