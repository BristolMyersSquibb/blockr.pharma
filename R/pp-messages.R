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
  # sendCustomMessage serialises with auto_unbox: a length-one vector
  # becomes a JSON scalar and the client iterating it would get characters.
  # The array channels are wrapped here, once, so the client never has to
  # tolerate a bare string (blockr.docs, js-driven-blocks: the auto-unbox
  # rule).
  if (channel %in% c("sync_selected", "sync_params")) {
    payload <- as.list(payload %||% character())
  }
  session$sendCustomMessage(session$ns(channel), payload)
}
