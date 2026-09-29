#' A panel's sentence
#'
#' What a panel shows, as one sentence under its title (design system, "The
#' sentence and its slots"): "6 events by *preferred term*, showing *all*".
#' Each control a viz declares (`viz$controls`) is a live word in it, a
#' `.blockr-slot` that opens blockr.ui's `Select.menu` listing its values
#' (pp-slots.js); an on/off control is a checkbox after the sentence. The
#' words print what the panel draws, so the same sentence reads as a caption.
#'
#' Per control type:
#'
#' * `pill`, `radio`: one of a few values. The word is the value's name.
#' * `checkbox`: several of the values the data carries. The word lists the
#'   first three picks, then "+N more", or `all_word` when all are picked.
#' * `find`: the panel's filter, over the values of the level the lanes show.
#'   The word is `all_word` ("all") or the picks; the sentence then says how
#'   many of the patient's records are left. The control's `noun` puts a
#'   count in front ("6 events"): of records, or with `count = "lanes"` of
#'   the lanes the chart draws ("4 medications").
#' * `toggle`: a checkbox, labelled with the control's label.
#'
#' A control's `phrase` places its word, `"by {}"` for the lanes; the default
#' is the word alone. A control with fewer than two values to offer draws
#' nothing: no options, no control.
#'
#' @param viz The `pp_viz` definition.
#' @param viz_id Its id on the profile.
#' @param dm_obj The patient's dm, for choices read off the data.
#' @param settings The current settings for this panel.
#' @noRd
pp_controls_ui <- function(viz, viz_id, dm_obj, settings) {
  controls <- viz$controls
  if (is.null(controls) || length(controls) == 0) return(NULL)

  words <- list()
  checks <- list()
  prefix <- NULL
  find_word <- NULL

  for (param in names(controls)) {
    ctrl <- controls[[param]]
    cur <- settings[[param]] %||% ctrl$default
    if (identical(ctrl$type, "toggle")) {
      checks[[length(checks) + 1L]] <- pp_ctrl_checkbox(viz_id, param, ctrl,
                                                         isTRUE(cur))
    } else if (identical(ctrl$type, "find")) {
      found <- pp_find_word(viz, viz_id, param, ctrl, dm_obj, settings)
      if (!is.null(found)) {
        prefix <- found$prefix
        find_word <- found$word
      }
    } else {
      w <- switch(ctrl$type,
        pill = ,
        radio = pp_single_word(viz, viz_id, param, ctrl, dm_obj, cur),
        checkbox = pp_multi_word(viz, viz_id, param, ctrl, dm_obj, cur),
        NULL
      )
      if (!is.null(w)) words[[length(words) + 1L]] <- w
    }
  }
  if (!is.null(find_word)) words[[length(words) + 1L]] <- find_word
  if (!length(words) && !length(checks)) return(NULL)

  # The first word starts the sentence, so it takes the capital.
  if (length(words) && is.null(prefix)) {
    words[[1L]] <- pp_capitalise_word(words[[1L]])
  }

  # One string: htmltools puts a line break between a tag's children, which
  # reads as a space before every comma.
  text <- ""
  if (!is.null(prefix)) text <- htmltools::htmlEscape(prefix)
  for (i in seq_along(words)) {
    lead <- if (i == 1L && !is.null(prefix)) " " else if (i > 1L) ", " else ""
    text <- paste0(text, lead, words[[i]])
  }

  shiny::div(
    class = "pp-chart-controls",
    if (nzchar(text)) shiny::span(class = "pp-chart-sentence", shiny::HTML(text)),
    checks
  )
}

#' A live word: a `.blockr-slot` button the client opens a menu from
#'
#' @param text What the word says.
#' @param phrase Its place in the sentence: `"by {}"`, or `"{}"`.
#' @param attrs The data the menu needs, as `data-*` attributes.
#' @noRd
pp_slot_word <- function(text, phrase, attrs) {
  phrase <- phrase %||% "{}"
  parts <- strsplit(phrase, "{}", fixed = TRUE)[[1L]]
  before <- if (length(parts)) parts[[1L]] else ""
  after <- if (length(parts) > 1L) parts[[2L]] else ""
  btn <- do.call(shiny::tags$button, c(
    list(type = "button", class = "blockr-slot pp-slot", text),
    attrs
  ))
  paste0(htmltools::htmlEscape(before), as.character(btn),
         htmltools::htmlEscape(after))
}

#' Lower-case a value's name for use inside a sentence, unless it is an
#' acronym ("ADAS-Cog", "NPI-X") or a code.
#' @noRd
pp_sentence_case <- function(x) {
  if (grepl("^[A-Z][a-z]", x)) {
    paste0(tolower(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
  } else {
    x
  }
}

#' Capitalise the first letter of the sentence's first word, whether it is
#' plain text or a slot button.
#' @noRd
pp_capitalise_word <- function(word) {
  # The word is HTML: capitalise the first letter of its text, whether that
  # is plain text or the label of the slot button it starts with.
  sub("^((?:<[^>]+>)*)([a-z])", "\\1\\U\\2", word, perl = TRUE)
}

#' One of a few values: the lanes, a findings card's value, a radio.
#' @noRd
pp_single_word <- function(viz, viz_id, param, ctrl, dm_obj, cur) {
  choices <- ctrl$choices %||% character(0)
  choices <- pp_ctrl_present_choices(choices, ctrl, dm_obj, viz$tables)
  if (length(choices) < 2L) return(NULL)
  if (is.null(cur) || !cur %in% choices) cur <- choices[[1L]]
  names_ <- unname(names(choices) %||% choices)
  opts <- lapply(seq_along(choices), function(i) {
    if (identical(names_[[i]], unname(choices[[i]]))) {
      list(value = unname(choices[[i]]))
    } else {
      list(value = unname(choices[[i]]), label = names_[[i]])
    }
  })
  shown <- names_[[match(cur, choices)]]
  pp_slot_word(pp_sentence_case(shown), ctrl$phrase, list(
    `data-viz-id` = viz_id,
    `data-param` = param,
    `data-kind` = "single",
    `data-title` = ctrl$label %||% ctrl$title %||% "",
    `data-options` = as.character(jsonlite::toJSON(opts, auto_unbox = TRUE)),
    `data-value` = as.character(jsonlite::toJSON(unname(cur), auto_unbox = TRUE))
  ))
}

#' Several of the values the data carries: items, visits.
#' @noRd
pp_multi_word <- function(viz, viz_id, param, ctrl, dm_obj, cur) {
  choices <- ctrl$choices
  if (is.null(choices) && !is.null(ctrl$choices_from)) {
    tbls <- dm::dm_get_tables(dm_obj)
    for (tbl_name in viz$tables) {
      if (!tbl_name %in% names(tbls)) next
      tbl <- as.data.frame(tbls[[tbl_name]])
      col <- ctrl$choices_from
      if (!col %in% colnames(tbl)) next
      # Visits come in visit order (AVISITN when present): lexical order
      # puts "Week 10" before "Week 2".
      choices <- if (identical(col, "AVISIT")) {
        pp_visit_levels(tbl)
      } else {
        sort(unique(as.character(tbl[[col]])))
      }
      if (!is.null(ctrl$choices_subset)) {
        choices <- intersect(ctrl$choices_subset, choices)
      }
      break
    }
  }
  choices <- as.character(choices %||% character(0))
  if (length(choices) < 2L) return(NULL)
  picked <- intersect(as.character(cur %||% choices), choices)
  if (!length(picked)) picked <- choices

  labs <- ctrl$choice_labels
  opts <- lapply(choices, function(ch) {
    full <- if (!is.null(labs) && ch %in% names(labs)) unname(labs[[ch]])
    if (is.null(full) || identical(full, ch)) list(value = ch) else
      list(value = ch, label = full)
  })
  name_of <- function(ch) {
    full <- if (!is.null(labs) && ch %in% names(labs)) unname(labs[[ch]])
    pp_param_short(full %||% ch)
  }
  text <- if (length(picked) == length(choices)) {
    ctrl$all_word %||% "all"
  } else {
    pp_slot_list_text(vapply(picked, name_of, character(1)))
  }
  pp_slot_word(text, ctrl$phrase, list(
    `data-viz-id` = viz_id,
    `data-param` = param,
    `data-kind` = "multi",
    `data-title` = ctrl$label %||% "",
    `data-options` = as.character(jsonlite::toJSON(opts, auto_unbox = TRUE)),
    `data-value` = as.character(jsonlite::toJSON(picked))
  ))
}

#' The first three picks, then how many more (design system, "Many values").
#' @noRd
pp_slot_list_text <- function(x, max_shown = 3L) {
  if (length(x) <= max_shown) return(paste(x, collapse = ", "))
  paste0(paste(x[seq_len(max_shown)], collapse = ", "),
         " +", length(x) - max_shown, " more")
}

#' The panel's filter, as a word
#'
#' Over the values of the level the lanes show. The options are this
#' patient's terms; pp-slots.js adds the rest of the
#' cohort's terms, fetched once over `find_vocab`, so a filter can be armed
#' for a term this patient does not have before paging through the cohort.
#'
#' @return `list(prefix, word)`, or `NULL` when the patient has no records.
#' @noRd
pp_find_word <- function(viz, viz_id, param, ctrl, dm_obj, settings) {
  tbl <- pp_find_table(dm_obj, viz$tables)
  if (is.null(tbl) || !nrow(tbl)) return(NULL)

  lanes <- viz$controls$lanes
  col <- if (!is.null(lanes)) {
    pp_lane_column(tbl, lanes$choices, settings$lanes, lanes$default)
  }
  col <- col %||% (names(ctrl$levels) %||% unname(ctrl$levels))[[1L]]
  if (is.null(col) || !col %in% colnames(tbl)) return(NULL)

  picks <- pp_find_picks(settings[[param]] %||% ctrl$default)
  v <- as.character(tbl[[col]])
  v <- v[!is.na(v) & nzchar(trimws(v))]
  tt <- table(v)
  opts <- lapply(names(tt), function(k) {
    list(value = k, n = unname(as.integer(tt[[k]])))
  })

  hits <- pp_find_hits(ctrl, dm_obj, viz$tables, picks)
  text <- if (length(picks)) {
    pp_slot_list_text(pp_find_labels(picks))
  } else {
    ctrl$all_word %||% "all"
  }
  word <- paste0(
    pp_slot_word(text, ctrl$phrase %||% "showing {}", list(
      `data-viz-id` = viz_id,
      `data-param` = param,
      `data-kind` = "find",
      `data-title` = ctrl$title %||% "Show",
      `data-col` = col,
      `data-options` = as.character(jsonlite::toJSON(opts, auto_unbox = TRUE)),
      `data-picks` = as.character(jsonlite::toJSON(picks, auto_unbox = TRUE))
    )),
    # How many of this patient's records the filter leaves, so an emptied
    # panel is never mistaken for a patient with no records.
    if (!is.null(hits)) sprintf(" (%d of %d)", hits$n, hits$total) else ""
  )

  # What the count counts: records (an adverse event is one), or the lanes
  # the chart draws (a medication given twenty times is one medication).
  noun <- ctrl$noun
  prefix <- if (!is.null(noun)) {
    n <- if (identical(ctrl$count, "lanes")) length(tt) else nrow(tbl)
    paste(n, if (n == 1L) noun[[1L]] else noun[[length(noun)]])
  }
  list(prefix = prefix, word = word)
}

#' An on/off control: blockr.ui's checkbox, labelled with what "on" does.
#' @noRd
pp_ctrl_checkbox <- function(viz_id, param, ctrl, on) {
  shiny::tags$label(
    class = "blockr-checkbox pp-ctrl-check",
    shiny::tags$input(
      type = "checkbox",
      `data-viz-id` = viz_id,
      `data-param` = param,
      checked = if (on) NA
    ),
    shiny::span(class = "blockr-checkbox__box", shiny::HTML(paste0(
      '<svg width="10" height="10" viewBox="0 0 16 16" fill="currentColor">',
      '<path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5',
      '-3.5a.5.5 0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/>',
      '</svg>'
    ))),
    shiny::span(class = "blockr-checkbox__label", ctrl$label %||% param)
  )
}
