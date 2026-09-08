# pp_send() is the one door every custom message leaves by. The array
# channels are wrapped there, because sendCustomMessage serialises with
# auto_unbox and a one-panel selection would otherwise reach the client as a
# bare string (blockr.docs, js-driven-blocks: the auto-unbox rule).

mock_session <- function() {
  sent <- list()
  list(
    ns = function(x) paste0("pp-", x),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    },
    sent = function() sent
  )
}

wire <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}

test_that("pp_send namespaces the channel", {
  s <- mock_session()
  pp_send(s, "sync_subject", list(id = "A"))
  expect_identical(s$sent()[[1L]]$type, "pp-sync_subject")
  expect_identical(wire(s$sent()[[1L]]$message), "{\"id\":\"A\"}")
})

test_that("the array channels reach the wire as arrays, one entry or none", {
  s <- mock_session()
  pp_send(s, "sync_selected", "ae_gantt")
  pp_send(s, "sync_selected", character())
  pp_send(s, "sync_selected", NULL)
  pp_send(s, "sync_selected", c("a", "b"))
  pp_send(s, "sync_params", "x@@Y")
  got <- vapply(s$sent(), function(m) wire(m$message), character(1L))
  expect_identical(got, c("[\"ae_gantt\"]", "[]", "[]", "[\"a\",\"b\"]", "[\"x@@Y\"]"))
})

test_that("object channels are left as they are", {
  s <- mock_session()
  pp_send(s, "subject_picker", list(count = 1L))
  pp_send(s, "dl_menu_state", list(single = FALSE, picked = "", n = 3L))
  got <- vapply(s$sent(), function(m) wire(m$message), character(1L))
  expect_identical(got, c("{\"count\":1}", "{\"single\":false,\"picked\":\"\",\"n\":3}"))
})
