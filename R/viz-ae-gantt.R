# Patient Profile Viz: Adverse Events Gantt
#
# Gantt bars showing AE duration by preferred term, colored by severity.
#
# Each lane is labelled in the plot area, on the row just above its own bars.
# The y axis carries no text: the shared 60px left margin (which keeps the x
# axis aligned with the other patient-profile panels) only fits ~8 characters,
# and preferred terms collide well before that ("APPLICATION SITE ERYTHEMA"
# and "...PRURITUS" both truncate to "APPLICAT..."). Labelling above the bar
# instead of beside it costs no horizontal room, so single-day events -- the
# common case -- are labelled as legibly as month-long ones. The label is
# truncated at the right edge of the grid; the tooltip always has the full
# term.
#
# Data requirements (declared via new_pp_viz()):
#   adae:
#     required — AEDECOD, and a time source: ASTDT (or ASTDTC) or ASTDY
#     optional — AENDT (or AENDTC), AENDY, a severity column (AETOXGR or
#                AESEV, see pp_sev_column()), AEBODSYS (or AESOC),
#                AESER, AEOUT
#
# A study may ship the AE onset as an analysis date, a study day, or both.
# Relative-day mode plots days, so a native ASTDY is used as-is rather than
# reconstructed from a date; date mode needs ASTDT and reports the panel
# unavailable without it. See pp_xval_pref_day().
#
# The dispatcher in patient-profile-block.R resolves these via
# pp_resolve_requires(): aliases are renamed to canonical names before this
# render runs, so the body can assume canonical column names exist.

#' AE Gantt visualization definition
#' @noRd
ae_gantt_viz <- new_pp_viz(
  id = "ae_gantt",
  label = "Adverse Events",
  domain = "Adverse Events",
  icon = "exclamation-triangle",
  color = "#7C3AED",
  description = "Gantt bars showing AE duration by preferred term",
  tables = "adae",
  requires = list(adae = list(
    AEDECOD = NULL
  )),
  requires_any = list(adae = list(c("ASTDT", "ASTDY"))),
  optional = list(adae = list(
    ASTDT    = "ASTDTC",
    AENDT    = "AENDTC",
    ASTDY    = "AESTDY",
    AENDY    = "AEENDY",
    AETOXGR  = NULL,
    AESEV    = NULL,
    AEBODSYS = "AESOC",
    AESER    = NULL,
    AEOUT    = NULL
  )),
  render = function(dm_obj, time_range, settings = list(),
                   ref_ms = NA_real_, mode = "date") {
    tbls <- dm::dm_get_tables(dm_obj)
    tbl <- as.data.frame(tbls[["adae"]])

    # Prefer the study day the data already carries; fall back to the date.
    # Date mode has no day-based fallback, so it needs ASTDT outright.
    has_day <- "ASTDY" %in% colnames(tbl)
    use_day <- identical(mode, "rday") && has_day
    if (!use_day && !"ASTDT" %in% colnames(tbl)) {
      return(pp_empty_chart(
        "Calendar dates unavailable for adverse events; switch the timeline to relative day"
      ))
    }

    tbl <- tbl[!is.na(if (use_day) tbl$ASTDY else tbl$ASTDT), , drop = FALSE]
    if (nrow(tbl) == 0) return(pp_empty_chart("No AE records"))

      # Severity colors: the board scale map (injected as
      # settings$sev_colors by the block server when a severity binding
      # resolves) beats the built-in constants; absent either way -> grey.
      sev_color <- function(sev) {
        s <- as.character(sev)
        fixed <- settings$sev_colors
        if (!is.null(fixed)) {
          if (s %in% names(fixed)) return(unname(fixed[[s]]))
          if (toupper(s) %in% names(fixed)) {
            return(unname(fixed[[toupper(s)]]))
          }
        }
        pp_sev_fallback_color(s)
      }

      terms <- sort(unique(as.character(tbl$AEDECOD)))
      sev_col <- pp_sev_column(colnames(tbl))
      has_sev <- !is.null(sev_col)
      has_end <- "AENDT" %in% colnames(tbl)
      has_bodsys <- "AEBODSYS" %in% colnames(tbl)
      has_serious <- "AESER" %in% colnames(tbl)
      has_outcome <- "AEOUT" %in% colnames(tbl)

      # In relative-day mode a native study day is plotted as-is; the end lane
      # only counts as present when it carries whichever source we are using.
      end_day <- use_day && "AENDY" %in% colnames(tbl)
      has_end <- if (use_day) end_day else has_end

      day_unit <- if (identical(mode, "rday")) 1 else 86400000
      start_at <- function(i) {
        if (use_day) tbl$ASTDY[i] else tbl$ASTDT[i]
      }
      end_at <- function(i) {
        if (use_day) tbl$AENDY[i] else tbl$AENDT[i]
      }
      # One label per lane, drawn on the lane's earliest bar: the lane *is*
      # the term, so repeating it on every bar of the lane is noise.
      lane_first <- vapply(terms, function(term) {
        rows <- which(as.character(tbl$AEDECOD) == term)
        starts <- vapply(rows, function(i) {
          as.numeric(if (use_day) tbl$ASTDY[i] else tbl$ASTDT[i])
        }, numeric(1L))
        rows[order(starts)][1L]
      }, integer(1L))

      bar_data <- lapply(seq_len(nrow(tbl)), function(i) {
        s <- pp_xval_pref_day(
          if (use_day) NULL else tbl$ASTDT[i],
          if (use_day) tbl$ASTDY[i] else NULL,
          ref_ms, mode
        )
        e <- if (has_end && !is.na(end_at(i))) {
          pp_xval_pref_day(
            if (use_day) NULL else tbl$AENDT[i],
            if (use_day) tbl$AENDY[i] else NULL,
            ref_ms, mode
          )
        } else {
          s + day_unit
        }
        term <- as.character(tbl$AEDECOD[i])
        lane <- match(term, terms) - 1L
        sev <- if (has_sev) as.character(tbl[[sev_col]][i]) else "UNKNOWN"
        bodsys <- if (has_bodsys) as.character(tbl$AEBODSYS[i]) else ""
        serious <- if (has_serious) as.character(tbl$AESER[i]) else ""
        outcome <- if (has_outcome) as.character(tbl$AEOUT[i]) else ""
        s_lab <- if (use_day) pp_day_label(tbl$ASTDY[i]) else {
          pp_xlabel(tbl$ASTDT[i], ref_ms, mode)
        }
        e_lab <- if (has_end && !is.na(end_at(i))) {
          if (use_day) pp_day_label(tbl$AENDY[i]) else {
            pp_xlabel(tbl$AENDT[i], ref_ms, mode)
          }
        } else {
          s_lab
        }

        col <- sev_color(sev)
        is_ser <- identical(toupper(serious), "Y")
        lab <- if (i %in% lane_first) pp_term_label(term) else ""
        list(
          value = list(s, e, lane, term, sev, bodsys, serious, outcome,
                       s_lab, e_lab, col, lab),
          # Serious is a regulatory axis of its own, independent of severity:
          # it gets the outline, severity keeps the fill.
          itemStyle = list(
            color = col,
            borderColor = if (is_ser) "#111827" else "transparent",
            borderWidth = if (is_ser) 1.5 else 0
          )
        )
      })

      # AE bars, each lane labelled on the row above its bars (value 11 is
      # empty for every bar but the lane's first, so the term is written once).
      series_list <- list(list(
        type = "custom",
        name = "Adverse Events",
        renderItem = htmlwidgets::JS("
          function(params, api) {
            var start = api.coord([api.value(0), api.value(2)]);
            var end   = api.coord([api.value(1), api.value(2)]);
            var laneH = api.size([0, 1])[1];
            var h     = Math.min(14, laneH * 0.36);
            var barW  = Math.max(end[0] - start[0], 4);
            // The bar sits below the lane centre so the label has the top of
            // the lane to itself.
            var barY  = start[1] + laneH * 0.18 - h / 2;
            var cs    = params.coordSys;
            var rect  = echarts.graphic.clipRectByRect(
              { x: start[0], y: barY, width: barW, height: h },
              { x: cs.x, y: cs.y, width: cs.width, height: cs.height }
            );
            if (!rect) return;

            var children = [{
              type: 'rect',
              shape: Object.assign({}, rect, { r: 3 }),
              style: api.style()
            }];

            var label = api.value(11);
            if (label) {
              var tx = Math.max(start[0], cs.x + 2);
              children.push({
                type: 'text',
                style: {
                  text: label,
                  x: tx,
                  y: start[1] - laneH * 0.26,
                  fill: '#4b5563',
                  fontSize: 10,
                  fontFamily: 'system-ui, -apple-system, sans-serif',
                  textVerticalAlign: 'middle',
                  truncate: { outerWidth: cs.x + cs.width - tx }
                }
              });
            }
            return { type: 'group', children: children };
          }
        "),
        encode = list(x = list(0, 1), y = 2),
        data = bar_data,
        tooltip = list(
          formatter = htmlwidgets::JS("
            function(params) {
              var v = params.value;
              var s = v[8] || '';
              var e = v[9] || '';
              var term = v[3] || '';
              var sev = '' + (v[4] == null ? '' : v[4]);
              // A bare CTCAE grade reads as noise in the badge.
              var sevDisp = /^[0-9]+$/.test(sev) ? 'Grade ' + sev : sev;
              var bodsys = v[5] || '';
              var serious = v[6] || '';
              var outcome = v[7] || '';
              var sevColors = {
                'SEVERE': '#DC2626', 'MODERATE': '#D97706', 'MILD': '#CA8A04'
              };
              var col = v[10] || sevColors[sev] || '#90a4ae';
              var html = '<div style=\"min-width:180px\">';
              html += '<div style=\"font-size:14px;font-weight:700;margin-bottom:4px\">' +
                term + '</div>';
              if (sev) {
                html += '<span style=\"display:inline-block;background:' + col +
                  ';color:#fff;padding:1px 8px;border-radius:3px;font-size:11px;' +
                  'font-weight:600;margin-bottom:4px\">' + sevDisp + '</span><br/>';
              }
              if (bodsys) {
                html += '<span style=\"color:#888;font-size:11px\">' +
                  bodsys.toUpperCase() + '</span><br/>';
              }
              html += '<span style=\"font-size:12px\">' +
                s + ' \\u2192 ' + e + '</span><br/>';
              if (serious) {
                html += '<span style=\"font-size:12px\">Serious: ' +
                  serious + '</span><br/>';
              }
              if (outcome) {
                html += '<span style=\"font-size:12px\">Outcome: ' +
                  outcome + '</span>';
              }
              html += '</div>';
              return html;
            }
          ")
        )
      ))

      # A little taller per lane than a bare bar row: the lane now carries
      # its label above the bar.
      chart_height <- max(250, length(terms) * 38 + 80)

      echarts4r::e_charts(height = chart_height) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          tooltip = pp_tooltip(),
          toolbox = pp_toolbox(),
          grid = list(
            left = 60, right = 20, top = 10, bottom = 30,
            borderColor = "transparent"
          ),
          xAxis = pp_time_axis(time_range, ref_ms, mode),
          yAxis = list(
            type = "category",
            data = terms,
            inverse = TRUE,
            axisLine = list(show = FALSE),
            axisTick = list(show = FALSE),
            # No text in the gutter: the term is written above the bar
            # instead. The 60px margin itself stays (the grid must keep
            # aligning with the other patient-profile panels).
            axisLabel = list(show = FALSE),
            splitLine = list(show = FALSE)
          ),
          series = series_list
        )) |>
        echarts4r::e_text_style(fontFamily = "system-ui, -apple-system, sans-serif")
    }
)

#' Severity legend for the AE panel header
#'
#' The bars have been severity-colored all along, but nothing said so. This
#' renders one swatch per severity level *actually present for this patient*,
#' from the same colors the bars use (`sev_colors` when the board scale map
#' resolves the severity binding, the built-in constants otherwise), so the
#' legend and the bars cannot drift apart. Grade-coded levels (AETOXGR) are
#' listed in numeric order and labelled "Grade N". Serious AEs (AESER) get
#' the outlined swatch that
#' marks them in the plot.
#'
#' @param dm_obj Subject-scoped dm.
#' @param sev_colors Resolved level -> color vector, or NULL.
#' @return A `shiny::div`, or NULL when the study carries no severity.
#' @noRd
pp_sev_legend_ui <- function(dm_obj, sev_colors = NULL) {
  adae <- tryCatch(
    as.data.frame(dm::dm_get_tables(dm_obj)[["adae"]]),
    error = function(e) NULL
  )
  if (is.null(adae) || !nrow(adae)) {
    return(NULL)
  }

  sev_col <- pp_sev_column(colnames(adae))
  sev <- if (!is.null(sev_col)) {
    as.character(adae[[sev_col]])
  } else {
    character()
  }
  sev <- unique(sev[!is.na(sev) & nzchar(sev)])

  # Grades in numeric order, then the canonical words, then anything else
  # the study happens to use.
  is_grade <- grepl("^[0-9]+$", sev)
  known <- c("MILD", "MODERATE", "SEVERE")
  sev <- c(
    sev[is_grade][order(as.integer(sev[is_grade]))],
    known[known %in% toupper(sev)],
    sort(sev[!is_grade & !toupper(sev) %in% known])
  )

  swatch <- function(color, label, outline = FALSE) {
    shiny::span(
      class = "pp-legend-item",
      shiny::span(
        class = if (outline) "pp-legend-swatch is-serious" else
          "pp-legend-swatch",
        style = if (outline) NULL else paste0("background:", color, ";")
      ),
      label
    )
  }

  items <- lapply(sev, function(s) {
    color <- if (!is.null(sev_colors)) {
      sev_colors[[s]] %||% sev_colors[[toupper(s)]] %||% NULL
    }
    if (is.null(color)) {
      color <- pp_sev_fallback_color(s)
    }
    swatch(unname(color), pp_sev_label(s))
  })

  has_serious <- "AESER" %in% colnames(adae) &&
    any(toupper(as.character(adae$AESER)) %in% "Y", na.rm = TRUE)
  if (has_serious) {
    items <- c(items, list(swatch(NULL, "Serious", outline = TRUE)))
  }

  if (!length(items)) {
    return(NULL)
  }
  shiny::div(class = "pp-chart-legend", items)
}

#' Display form of a preferred term
#'
#' AEDECOD ships upper case ("APPLICATION SITE ERYTHEMA"), which is shouting
#' once it sits in the plot area next to sentence-case panel text.
#' @noRd
pp_term_label <- function(term) {
  term <- as.character(term)
  ifelse(
    is.na(term) | !nzchar(term),
    "",
    paste0(substr(term, 1L, 1L), tolower(substring(term, 2L)))
  )
}

#' Minimal empty chart placeholder
#' @param msg Message to display
#' @noRd
pp_empty_chart <- function(msg) {
  echarts4r::e_charts(height = 80) |>
    echarts4r::e_list(list(
      title = list(
        text = msg,
        left = "center", top = "center",
        textStyle = list(fontSize = 13, color = "#9ca3af", fontWeight = 400)
      ),
      xAxis = list(show = FALSE),
      yAxis = list(show = FALSE)
    ))
}

