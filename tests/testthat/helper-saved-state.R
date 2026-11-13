# A block saved and loaded the way a board does it: the state its server
# returns (every argument of the table block underneath, not only the
# block's own), written to JSON and read back as blockr.session does.
board_round_trip <- function(blk, data) {
  live <- function(b) {
    st <- NULL
    shiny::testServer(
      blockr.core:::block_expr_server(b),
      args = list(data = shiny::reactive(data)),
      {
        session$flushReact()
        st <<- lapply(session$getReturned()$state, shiny::isolate)
        st <<- lapply(st, function(x) if (is.function(x)) x() else x)
      }
    )
    st
  }
  st <- live(blk)
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f))
  jsonlite::write_json(blockr.core::blockr_ser(blk, state = st), f,
                       null = "null")
  ser <- jsonlite::fromJSON(f, simplifyDataFrame = FALSE,
                            simplifyMatrix = FALSE)
  back <- blockr.core::blockr_deser(ser)
  list(saved = st, block = back, restored = live(back))
}

# A state read from JSON holds factors and dates as strings.
plain_state <- function(st) {
  plain <- function(v) {
    if (is.factor(v)) as.character(v) else if (inherits(v, "Date")) {
      format(v)
    } else {
      v
    }
  }
  st$script_values <- lapply(st$script_values, plain)
  st
}
