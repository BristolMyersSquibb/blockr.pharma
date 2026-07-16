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
#                CMDOSE, CMDOSU, CMDOSFRQ, CMROUTE, CMCLAS, CMINDC
#
# Lanes are labelled above their bars, exactly like the AE gantt, and for
# the same reason: the shared 60px left margin fits ~8 characters and
# medication names collide well before that.

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
    "CMROUTE", "CMCLAS", "CMINDC"
  )),
  uses = "cycle",
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

    # Lane label: the coded name reads cleaner than the verbatim report;
    # fall back per row, a partially coded table must not blank its lanes.
    med_of <- function(df) {
      med <- if ("CMDECOD" %in% colnames(df)) as.character(df$CMDECOD)
      rep_name <- as.character(df$CMTRT)
      if (is.null(med)) return(rep_name)
      blank <- is.na(med) | !nzchar(trimws(med))
      med[blank] <- rep_name[blank]
      med
    }
    tbl$..med <- med_of(tbl)
    meds <- sort(unique(tbl$..med))

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
    # Cycle anchors, injected by the block (uses = "cycle"); NULL makes the
    # label helpers below no-ops.
    cyc <- settings$cycle_anchors

    # One label per lane, drawn on the lane's earliest bar (see ae_gantt).
    lane_first <- vapply(meds, function(med) {
      rows <- which(tbl$..med == med)
      starts <- vapply(rows, function(i) {
        as.numeric(if (use_day) tbl$ASTDY[i] else tbl$ASTDT[i])
      }, numeric(1L))
      rows[order(starts)][1L]
    }, integer(1L))

    bar_color <- "#0891B2"

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
      med <- tbl$..med[i]
      lane <- match(med, meds) - 1L

      dose <- trimws(paste(
        opt_chr(tbl, "CMDOSE", i), opt_chr(tbl, "CMDOSU", i),
        opt_chr(tbl, "CMDOSFRQ", i)
      ))
      s_lab <- if (use_day) pp_day_label(tbl$ASTDY[i], cyc) else {
        pp_xlabel(tbl$ASTDT[i], ref_ms, mode, cyc)
      }
      e_lab <- if (has_end && !is.na(end_at(i))) {
        if (use_day) pp_day_label(tbl$AENDY[i], cyc) else {
          pp_xlabel(tbl$AENDT[i], ref_ms, mode, cyc)
        }
      } else {
        s_lab
      }
      lab <- if (i %in% lane_first) pp_term_label(med) else ""
      list(
        value = list(s, e, lane, med, dose,
                     opt_chr(tbl, "CMROUTE", i), opt_chr(tbl, "CMCLAS", i),
                     opt_chr(tbl, "CMINDC", i), s_lab, e_lab, lab),
        itemStyle = list(color = bar_color)
      )
    })

    series_list <- list(list(
      type = "custom",
      name = "Concomitant Medications",
      renderItem = htmlwidgets::JS("
        function(params, api) {
          var start = api.coord([api.value(0), api.value(2)]);
          var end   = api.coord([api.value(1), api.value(2)]);
          var laneH = api.size([0, 1])[1];
          var h     = Math.min(14, laneH * 0.36);
          var barW  = Math.max(end[0] - start[0], 4);
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

          var label = api.value(10);
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
            var med = v[3] || '';
            var dose = v[4] || '';
            var route = v[5] || '';
            var klass = v[6] || '';
            var indc = v[7] || '';
            var s = v[8] || '';
            var e = v[9] || '';
            var html = '<div style=\"min-width:180px\">';
            html += '<div style=\"font-size:14px;font-weight:700;' +
              'margin-bottom:4px\">' + med + '</div>';
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
            if (indc) {
              html += '<span style=\"font-size:12px\">Indication: ' +
                indc + '</span>';
            }
            html += '</div>';
            return html;
          }
        ")
      )
    ))

    chart_height <- max(250, length(meds) * 38 + 80)

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
