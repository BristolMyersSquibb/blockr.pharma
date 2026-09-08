#' Messages the patient profile sends its client
#'
#' Every custom message goes through here, so the channel table lives in one
#' place. The client side is the matching `Shiny.addCustomMessageHandler` in
#' inst/js/pp-*.js; tests/js pins what each handler does with its payload.
#'
#' | channel          | payload                              | client effect |
#' |------------------|--------------------------------------|---------------|
#' | `sync_selected`  | character: panel ids on the profile  | On list, catalogue ticks, count |
#' | `sync_subject`   | `list(id = )`, `""` for none         | moves the selected class |
#' | `sync_params`    | character: `viz@@PARAMCD` keys       | parameter ticks |
#' | `sync_band`      | `list(viz_id = )`, `""` for none     | tags the band's panel |
#' | `subject_picker` | `list(count = )`                     | the cohort count tag |
#' | `dl_menu_state`  | `list(single, picked, n)`            | download menu scopes |
#'
#' @param session The module's session.
#' @param channel One of the channels above.
#' @param payload What the handler receives.
#' @noRd
pp_send <- function(session, channel, payload) {
  session$sendCustomMessage(session$ns(channel), payload)
}
