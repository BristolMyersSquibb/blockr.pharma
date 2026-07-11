# Patient Profile Viz: Adverse Events Gantt
#
# Gantt bars showing AE duration by preferred term, colored by severity.
# Lanes are labelled on the y axis with small ellipsized term names that
# fit the shared 60px left margin (the x axis must stay aligned with the
# other patient-profile panels); the bar tooltip shows the full term.
#
# Data requirements (declared via new_pp_viz()):
#   adae:
#     required — AEDECOD, and a time source: ASTDT (or ASTDTC) or ASTDY
#     optional — AENDT (or AENDTC), AENDY, AESEV, AEBODSYS (or AESOC),
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
      # settings$sev_colors by the block server when an AESEV binding
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
        switch(toupper(s),
          SEVERE   = "#DC2626",
          MODERATE = "#D97706",
          MILD     = "#CA8A04",
          "#9ca3af"
        )
      }

      terms <- sort(unique(as.character(tbl$AEDECOD)))
      has_sev <- "AESEV" %in% colnames(tbl)
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
        sev <- if (has_sev) as.character(tbl$AESEV[i]) else "UNKNOWN"
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
        list(
          value = list(s, e, lane, term, sev, bodsys, serious, outcome,
                       s_lab, e_lab, col),
          itemStyle = list(color = col)
        )
      })

      # AE bars with inside labels
      series_list <- list(list(
        type = "custom",
        name = "Adverse Events",
        renderItem = htmlwidgets::JS("
          function(params, api) {
            var start = api.coord([api.value(0), api.value(2)]);
            var end   = api.coord([api.value(1), api.value(2)]);
            var h     = api.size([0, 1])[1] * 0.6;
            var barW  = Math.max(end[0] - start[0], 4);
            var rect  = echarts.graphic.clipRectByRect(
              { x: start[0], y: start[1] - h/2,
                width: barW, height: h },
              { x: params.coordSys.x, y: params.coordSys.y,
                width: params.coordSys.width, height: params.coordSys.height }
            );
            if (!rect) return;
            return {
              type: 'rect',
              shape: Object.assign({}, rect, { r: 3 }),
              style: api.style()
            };
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
              var sev = v[4] || '';
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
                  'font-weight:600;margin-bottom:4px\">' + sev + '</span><br/>';
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

      chart_height <- max(250, length(terms) * 32 + 80)

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
            # Ellipsized lane labels in the shared 60px left margin (the
            # grid must stay aligned with the other patient-profile
            # panels). They only hint at the term; the bar tooltip shows
            # it in full on hover.
            axisLabel = list(
              show = TRUE,
              fontSize = 9,
              color = "#6b7280",
              margin = 4,
              formatter = htmlwidgets::JS("
                function(v) {
                  return v.length > 9 ? v.slice(0, 8) + '…' : v;
                }
              ")
            ),
            splitLine = list(show = FALSE)
          ),
          series = series_list
        )) |>
        echarts4r::e_text_style(fontFamily = "system-ui, -apple-system, sans-serif")
    }
)

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

#' Resolve AE severity colors from the board scale map
#'
#' Returns the resolved color vector for the severity levels present in the
#' patient's adae (binding "AESEV"), or NULL when there is no map, no
#' binding, or no severity data — the gantt then uses its built-in constants.
#' @noRd
pp_sev_scale_colors <- function(map, dm_obj) {
  if (is.null(map)) {
    return(NULL)
  }

  adae <- tryCatch(
    dm::dm_get_tables(dm_obj)[["adae"]],
    error = function(e) NULL
  )

  if (is.null(adae) || !"AESEV" %in% colnames(adae)) {
    return(NULL)
  }

  sev <- unique(as.character(adae$AESEV))
  sev <- sev[!is.na(sev) & nzchar(sev)]

  if (!length(sev)) {
    return(NULL)
  }

  blockr.theme::resolve_scales(map, "AESEV", levels = sev)$color
}
