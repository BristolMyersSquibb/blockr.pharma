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
#' | `slot`           | `pp_slot_update()`: viz_id, header HTML, opts_json, height | updates one panel in place |
#' | `find_vocab`     | `list(viz_id, token, groups)` or `list(viz_id, token, unchanged = TRUE)` | the cohort's terms, for the find popover |
#'
#' A test can watch the traffic: with the option
#' `blockr.pharma.pp_message_sink` set to a function of `(channel, payload)`,
#' every message is handed to it as well. `tests/js/fixtures/render.R`
#' records the fixtures' messages that way.
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
  sink <- getOption("blockr.pharma.pp_message_sink")
  if (is.function(sink)) sink(channel, payload)
  session$sendCustomMessage(session$ns(channel), payload)
}
