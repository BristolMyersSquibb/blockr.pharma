# Patient Profile Viz: Concomitant Medications Gantt
#
# Gantt bars showing medication periods, one lane per medication. The proof
# that the viz catalogue is extensible: this viz is a declaration against
# canonical names plus a render -- no new exceptions anywhere in the block.
# pp_normalize_dm() already reconciles an ADaM adcm, an ADaM-shaped `cm`,
# and a real SDTM cm domain (CMSTDTC/CMENDTC/CMSTDY/CMENDY) before this
# render ever runs.
#
# Data requirements (declared via new_pp_viz()):
#   adcm:
#     required — CMTRT (the reported name; every CM table carries it), and a
#                time source: ASTDT or ASTDY
#     optional — CMDECOD (coded name, preferred for lanes), AENDT, AENDY,
#                CMDOSE, CMDOSU, CMDOSFRQ, CMROUTE, CMCLAS
#
# Which coding level the lanes come from is the user's ("Lanes" pill,
# settings$lanes) whenever the study carries more than one: the coded name
# by default, the drug class when the point is what the patient was on
# rather than which product. See pp-lanes.R. The tooltip always names the
# medication, at every lane setting.
#     roles    — indication (the resolved column arrives as
#                settings$roles$indication, its colors as
#                settings$indc_colors)
#
# Lanes are labelled above their bars, exactly like the AE gantt, and for
# the same reason: the shared left margin (PP_GRID_LEFT) fits only a few
# characters and medication names collide well before that.
#
# Bars are colored by indication (why the medication was given), the way the
# AE gantt colors by severity. The colors come from blockr.theme -- the
# board's scale-map binding for the indication column, else the theme's
# categorical palette -- and this file knows neither the levels nor the hexes;
# see pp-indication.R, including when the coloring is skipped entirely.

#' Concomitant medications visualization definition
#' @noRd
cm_gantt_viz <- new_pp_viz(
  id = "cm_gantt",
  label = "Concomitant Medications",
  domain = "Medications",
  icon = "capsule",
  color = "#0891B2",
  description = "Gantt bars showing medication periods",
  tables = "adcm",
  requires = list(adcm = "CMTRT"),
  requires_any = list(adcm = list(c("ASTDT", "ASTDY"))),
  optional = list(adcm = c(
    "CMDECOD", "AENDT", "AENDY", "CMDOSE", "CMDOSU", "CMDOSFRQ",
    "CMROUTE", "CMCLAS"
  )),
  controls = pp_lane_control(PP_CM_LANES, default = "CMDECOD"),
  uses = "indication",
  legend_ui = function(dm_obj, settings) {
    pp_indc_legend_ui(settings$indc_colors)
  },
  exhibit = function(dm_obj, time_range, settings = list(),
                     ref_ms = NA_real_, mode = "date") {
    pp_static_cm_gantt(dm_obj, time_range, settings, ref_ms, mode)
  },
  render = function(dm_obj, time_range, settings = list(),
                    ref_ms = NA_real_, mode = "date") {
    tbls <- dm::dm_get_tables(dm_obj)
    tbl <- as.data.frame(tbls[["adcm"]])

    # Prefer the study day the data already carries; fall back to the date.
    # Date mode has no day-based fallback, so it needs ASTDT outright.
    has_day <- "ASTDY" %in% colnames(tbl)
    use_day <- identical(mode, "rday") && has_day
    if (!use_day && !"ASTDT" %in% colnames(tbl)) {
      return(pp_empty_chart(paste0(
        "Calendar dates unavailable for medications; ",
        "switch the timeline to relative day"
      )))
    }

    tbl <- tbl[!is.na(if (use_day) tbl$ASTDY else tbl$ASTDT), , drop = FALSE]
    if (nrow(tbl) == 0) return(pp_empty_chart("No medication records"))

    # The medication's name for the tooltip: the coded name reads cleaner
    # than the verbatim report, falling back per row (a partially coded
    # table must not blank its labels). This is the tooltip's title at every
    # lane setting -- grouping by class must not cost the bar its identity.
    name_col <- if ("CMDECOD" %in% colnames(tbl)) "CMDECOD" else "CMTRT"
    tbl$..med <- pp_lane_values(tbl, name_col, "CMTRT")

    # Lanes come from whichever coding level the user is reading at; the
    # coded name unless the header pill says otherwise, and a level this
    # study does not carry falls back rather than erroring (the control that
    # could have produced it is data-conditional, so a saved board can name
    # a level the current study lacks).
    lane_col <- pp_lane_column(tbl, PP_CM_LANES, settings$lanes, "CMDECOD")
    tbl$..lane <- pp_lane_values(tbl, lane_col %||% "CMTRT", "CMTRT")

    has_end <- if (use_day) {
      "AENDY" %in% colnames(tbl)
    } else {
      "AENDT" %in% colnames(tbl)
    }
    opt_chr <- function(df, col, i) {
      if (col %in% colnames(df)) {
        v <- df[[col]][i]
        if (is.na(v)) "" else as.character(v)
      } else {
        ""
      }
    }

    day_unit <- if (identical(mode, "rday")) 1 else 86400000
    end_at <- function(i) if (use_day) tbl$AENDY[i] else tbl$AENDT[i]

    bar_span <- function(i) {
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
        pp_gantt_open_end(s, time_range, ref_ms, mode, day_unit)
      }
      c(s, e)
    }
    is_ongoing <- function(i) !(has_end && !is.na(end_at(i)))

    # Lanes come from what the window draws, not from the table: a med whose
    # every bar is off-axis (chronic meds started years pre-study are the
    # common case) otherwise reserved an empty lane. See pp_gantt_in_window().
    spans <- vapply(seq_len(nrow(tbl)), bar_span, numeric(2L))
    tbl <- tbl[
      pp_gantt_in_window(spans[1L, ], spans[2L, ], time_range, ref_ms, mode), ,
      drop = FALSE
    ]
    if (nrow(tbl) == 0) {
      return(pp_empty_chart("No medication records in this time range"))
    }
    meds <- sort(unique(tbl$..lane))

    # One label per lane, drawn on the lane's earliest bar (see ae_gantt).
    lane_first <- vapply(meds, function(med) {
      rows <- which(tbl$..lane == med)
      starts <- vapply(rows, function(i) {
        as.numeric(if (use_day) tbl$ASTDY[i] else tbl$ASTDT[i])
      }, numeric(1L))
      rows[order(starts)][1L]
    }, integer(1L))

    # The indication column is a role, resolved once by the block and
    # injected -- never re-detected here, or bars and legend could drift.
    indc_col <- settings$roles$indication
    has_indc <- !is.null(indc_col) && indc_col %in% colnames(tbl)
    indc_at <- function(i) if (has_indc) opt_chr(tbl, indc_col, i) else ""

    # Indication colors, resolved ONCE in the block (settings$indc_colors,
    # via blockr.theme) and read here and by the legend, so the two cannot
    # disagree. Absent -- no indication column, or levels the theme palette
    # cannot tell apart -- the panel keeps its single medication color rather
    # than greying every bar; grey is for the rows that carry no indication
    # while others do.
    default_color <- "#0891B2"
    indc_hex <- if (has_indc) settings$indc_colors else NULL
    bar_color <- function(indc) {
      if (is.null(indc_hex) || !length(indc_hex)) return(default_color)
      if (indc %in% names(indc_hex)) unname(indc_hex[[indc]]) else "#9ca3af"
    }

    bar_data <- lapply(seq_len(nrow(tbl)), function(i) {
      span <- bar_span(i)
      s <- span[1L]
      e <- span[2L]
      med <- tbl$..med[i]
      lane <- match(tbl$..lane[i], meds) - 1L

      dose <- trimws(paste(
        opt_chr(tbl, "CMDOSE", i), opt_chr(tbl, "CMDOSU", i),
        opt_chr(tbl, "CMDOSFRQ", i)
      ))
      s_lab <- if (use_day) pp_day_label(tbl$ASTDY[i]) else {
        pp_xlabel(tbl$ASTDT[i], ref_ms, mode)
      }
      e_lab <- if (has_end && !is.na(end_at(i))) {
        if (use_day) pp_day_label(tbl$AENDY[i]) else {
          pp_xlabel(tbl$AENDT[i], ref_ms, mode)
        }
      } else {
        PP_ONGOING_LABEL
      }
      lab <- if (i %in% lane_first) pp_term_label(tbl$..lane[i]) else ""
      indc <- indc_at(i)
      col <- bar_color(indc)
      list(
        # Value 12 is the tooltip's badge color: the color resolved in R, so
        # the badge cannot pick a palette of its own, and empty when the bars
        # are uniformly colored (a badge would then claim a distinction the
        # plot does not draw).
        value = list(s, e, lane, med, dose,
                     opt_chr(tbl, "CMROUTE", i), opt_chr(tbl, "CMCLAS", i),
                     indc, s_lab, e_lab, lab,
                     is_ongoing(i), if (length(indc_hex)) col else ""),
        itemStyle = list(color = col)
      )
    })

    series_list <- list(list(
      type = "custom",
      name = "Concomitant Medications",
      renderItem = pp_gantt_render_item(10, ongoing_idx = 11),
      encode = list(x = list(0, 1), y = 2),
      data = bar_data,
      tooltip = list(
        formatter = htmlwidgets::JS("
          function(params) {
            var v = params.value;
            var med = v[3] || '';
            var dose = v[4] || '';
            var route = v[5] || '';
            var klass = v[6] || '';
            var indc = '' + (v[7] == null ? '' : v[7]);
            var s = v[8] || '';
            var e = v[9] || '';
            var badge = v[12] || '';
            var html = '<div style=\"min-width:180px\">';
            html += '<div style=\"font-size:14px;font-weight:700;' +
              'margin-bottom:4px\">' + med + '</div>';
            if (indc && badge) {
              html += '<span style=\"display:inline-block;background:' +
                badge + ';color:#fff;padding:1px 8px;border-radius:3px;' +
                'font-size:11px;font-weight:600;margin-bottom:4px\">' +
                indc + '</span><br/>';
            }
            if (klass) {
              html += '<span style=\"color:#888;font-size:11px\">' +
                klass.toUpperCase() + '</span><br/>';
            }
            html += '<span style=\"font-size:12px\">' +
              s + ' \\u2192 ' + e + '</span><br/>';
            if (dose) {
              html += '<span style=\"font-size:12px\">Dose: ' +
                dose + '</span><br/>';
            }
            if (route) {
              html += '<span style=\"font-size:12px\">Route: ' +
                route + '</span><br/>';
            }
            if (indc && !badge) {
              html += '<span style=\"font-size:12px\">Indication: ' +
                indc + '</span>';
            }
            html += '</div>';
            return html;
          }
        ")
      )
    ))

    chart_height <- pp_gantt_height(length(meds))

    echarts4r::e_charts(height = chart_height) |>
      echarts4r::e_list(list(
        backgroundColor = "transparent",
        tooltip = pp_tooltip(),
        toolbox = pp_toolbox(),
        grid = list(
          left = PP_GRID_LEFT, right = 20,
          top = PP_GANTT_TOP, bottom = PP_GANTT_BOTTOM,
          borderColor = "transparent"
        ),
        xAxis = pp_time_axis(time_range, ref_ms, mode),
        yAxis = list(
          type = "category",
          data = meds,
          inverse = TRUE,
          axisLine = list(show = FALSE),
          axisTick = list(show = FALSE),
          axisLabel = list(show = FALSE),
          splitLine = list(show = FALSE)
        ),
        series = series_list
      )) |>
      echarts4r::e_text_style(
        fontFamily = "system-ui, -apple-system, sans-serif"
      )
  }
)
