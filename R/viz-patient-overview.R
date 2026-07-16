# Patient Profile Viz: Patient Overview
#
# Multi-lane overview chart (~180px) combining treatment timeline, adverse
# events, and key milestones in a single compact chart.
#
# Lanes (each data-gated; absent tables simply drop their lane):
#   1. Treatment — green bar from TRTSDT to TRTEDT with arm label
#   2. Exposure — dosing-period bars from adex, labelled with the dose
#   3. Adverse Events — compact overlapping bars colored by severity
#   4. Milestones — point markers: treatment start/end, end of study, death
#   5. Visits — one tick per visit the subject attended (reconstructed from
#      the findings tables; see pp_visit_schedule())
#
# Data requirements (declared via new_pp_viz()):
#   adsl: required TRTSDT, TRTEDT; optional RFENDT, DTHDT, DTHFL
#   adae (optional table): ASTDT, AENDT, ASTDY, AENDY, AEDECOD, AESER
#   adex (optional table): ASTDT, AENDT, ASTDY, AENDY, EXDOSE, EXDOSU, EXTRT
#   roles: arm (the ADSL arm column, settings$roles$arm) and severity
#          (the ADAE severity column, settings$roles$severity)
#
# All names are canonical: pp_normalize_dm() reconciles a study's spellings
# (EOSDT, DTHDTC, ASTDTC, ...) dm-wide before anything renders.

#' Patient Overview visualization definition
#' @noRd
patient_overview_viz <- new_pp_viz(
  id = "patient_overview",
  label = "Patient Overview",
  domain = "Treatment",
  icon = "capsule",
  color = "#059669",
  description = "Treatment, exposure, adverse events, visits & milestones",
  tables = "adsl",
  requires = list(adsl = c("TRTSDT", "TRTEDT")),
  uses = c("arm", "severity"),
  optional = list(
    adsl = c("RFENDT", "DTHDT", "DTHFL"),
    adae = c(
      "ASTDT", "AENDT", "ASTDY", "AENDY", "AEDECOD", "AESER"
    ),
    adex = c(
      "ASTDT", "AENDT", "ASTDY", "AENDY", "EXDOSE", "EXDOSU", "EXTRT"
    )
  ),
  render = function(dm_obj, time_range, settings = list(),
                   ref_ms = NA_real_, mode = "date") {
    tbls <- dm::dm_get_tables(dm_obj)
    adsl <- as.data.frame(tbls[["adsl"]])

    if (nrow(adsl) == 0) return(pp_empty_chart("No ADSL records"))

      sl <- adsl[1, , drop = FALSE]
      if (is.na(sl$TRTSDT[1]) || is.na(sl$TRTEDT[1])) {
        return(pp_empty_chart("Missing treatment dates"))
      }

      trt_start <- pp_xval(sl$TRTSDT[1], ref_ms, mode)
      trt_end <- pp_xval(sl$TRTEDT[1], ref_ms, mode)
      # The arm column is a role, resolved once by the block (board option
      # or ACTARM) and injected -- the render never picks its own, so the
      # lane and the subject picker cannot disagree. An unresolved arm has
      # already raised the named block error; the lane stays generic.
      arm_col <- settings$roles$arm
      arm_label <- if (!is.null(arm_col) && arm_col %in% colnames(sl)) {
        # The lane draws one line of text inside the treatment bar, so fold any
        # embedded whitespace (a study's own arm column may carry line breaks
        # the ADaM variables never do) rather than drawing lines on top of each
        # other.
        trimws(gsub("[[:space:]]+", " ", as.character(sl[[arm_col]][1])))
      } else {
        "Treatment"
      }

      # Determine lanes — omit AE lane when adae missing (or has no records).
      # The onset may arrive as a study day, a date, or both; relative-day mode
      # prefers the day, for the reasons in pp_xval_pref_day().
      has_adae <- "adae" %in% names(tbls)
      ae_use_day <- FALSE
      if (has_adae) {
        adae_raw <- as.data.frame(tbls[["adae"]])
        ae_use_day <- identical(mode, "rday") &&
          "ASTDY" %in% colnames(adae_raw)
        ae_src <- if (ae_use_day) "ASTDY" else "ASTDT"
        has_adae <- ae_src %in% colnames(adae_raw) &&
          nrow(adae_raw[!is.na(adae_raw[[ae_src]]), , drop = FALSE]) > 0
      }

      # Exposure lane, same date-or-day gating as the AE lane. adex is
      # parameterized in ADaM (several rows per dosing period), so periods
      # dedupe by (start, end, dose).
      has_adex <- "adex" %in% names(tbls)
      ex_use_day <- FALSE
      if (has_adex) {
        adex_raw <- as.data.frame(tbls[["adex"]])
        ex_use_day <- identical(mode, "rday") &&
          "ASTDY" %in% colnames(adex_raw)
        ex_src <- if (ex_use_day) "ASTDY" else "ASTDT"
        has_adex <- ex_src %in% colnames(adex_raw) &&
          nrow(adex_raw[!is.na(adex_raw[[ex_src]]), , drop = FALSE]) > 0
      }

      # Visit ruler: reconstructed from the findings tables; anchors every
      # other lane to when the subject was actually seen.
      visits <- pp_visit_schedule(tbls)
      if (nrow(visits)) {
        vis_ok <- if (identical(mode, "rday")) {
          !is.na(visits$day) | !is.na(visits$date)
        } else {
          !is.na(visits$date)
        }
        visits <- visits[vis_ok, , drop = FALSE]
      }
      has_vis <- nrow(visits) > 0

      # Short lane labels keep grid.left at 60 (aligned with the other
      # timeline charts). Tooltips on each item still carry the full
      # category context (Treatment / Exposure / AE term / milestone kind).
      lanes <- c("TRT", if (has_adex) "EX", if (has_adae) "AE", "MS",
                 if (has_vis) "VIS")
      lane_full <- c(TRT = "Treatment", EX = "Exposure",
                     AE = "Adverse Events", MS = "Milestones",
                     VIS = "Visits")
      lane_idx <- stats::setNames(seq_along(lanes) - 1L, lanes)
      n_lanes <- length(lanes)
      chart_height <- 60 + n_lanes * 40

      # A study's arm label is data, not a literal: encode it, never paste it.
      arm_js <- pp_js_str(arm_label)
      start_str <- pp_xlabel(sl$TRTSDT[1], ref_ms, mode)
      end_str <- pp_xlabel(sl$TRTEDT[1], ref_ms, mode)

      # ---------------------------------------------------------------
      # Treatment lane
      # ---------------------------------------------------------------
      trt_series <- list(
        type = "custom",
        name = "Treatment",
        renderItem = htmlwidgets::JS(sprintf("
          function(params, api) {
            var start = api.coord([api.value(0), api.value(2)]);
            var end   = api.coord([api.value(1), api.value(2)]);
            var h     = api.size([0, 1])[1] * 0.5;
            var barW  = Math.max(end[0] - start[0], 4);
            return {
              type: 'group',
              children: [{
                type: 'rect',
                shape: {
                  x: start[0], y: start[1] - h/2,
                  width: barW, height: h, r: 3
                },
                style: {
                  fill: 'rgba(5,150,105,0.2)',
                  stroke: 'rgba(5,150,105,0.5)',
                  lineWidth: 1
                }
              }, {
                type: 'text',
                style: {
                  text: %s,
                  x: start[0] + 8,
                  y: start[1],
                  fill: '#059669',
                  fontSize: 11,
                  fontWeight: 600,
                  fontFamily: 'system-ui, -apple-system, sans-serif',
                  textVerticalAlign: 'middle',
                  truncate: { outerWidth: barW - 16 }
                }
              }]
            };
          }
        ", arm_js)),
        data = list(list(
          value = list(trt_start, trt_end, lane_idx[["TRT"]])
        )),
        encode = list(x = list(0, 1), y = 2),
        tooltip = list(
          formatter = htmlwidgets::JS(sprintf("
            function(params) {
              return '<div style=\"min-width:160px\">' +
                '<div style=\"font-size:13px;font-weight:600;margin-bottom:4px\">' +
                %s + '</div>' +
                '<div style=\"font-size:12px;color:#6b7280\">' +
                %s + ' \\u2192 ' + %s + '</div></div>';
            }
          ", arm_js, pp_js_str(start_str), pp_js_str(end_str)))
        )
      )

      all_series <- list(trt_series)

      # ---------------------------------------------------------------
      # Adverse Events lane (omitted when adae missing)
      # ---------------------------------------------------------------
      if (has_adae) {
        adae <- adae_raw[!is.na(adae_raw[[ae_src]]), , drop = FALSE]
        ae_lane <- lane_idx[["AE"]]
        has_end <- if (ae_use_day) {
          "AENDY" %in% colnames(adae)
        } else {
          "AENDT" %in% colnames(adae)
        }
        # Severity is a role, injected by the block -- same source as the AE
        # gantt, so the two vizs always agree.
        sev_col <- settings$roles$severity
        has_sev <- !is.null(sev_col) && sev_col %in% colnames(adae)
        has_ser <- "AESER" %in% colnames(adae)
        has_term <- "AEDECOD" %in% colnames(adae)
        day_unit <- if (identical(mode, "rday")) 1 else 86400000

        # Severity colors: the board scale map (injected as
        # settings$sev_colors by the block server when a severity binding
        # resolves) beats the built-in constants — same precedence as the
        # AE gantt, so both vizs always agree.
        sev_hex <- pp_sev_colors
        fixed <- settings$sev_colors
        if (!is.null(fixed)) {
          vals <- unname(unlist(fixed))
          names(vals) <- toupper(names(fixed))
          sev_hex[names(vals)] <- vals
        }
        js_color_map <- function(values) {
          paste0("{", paste(
            sprintf("'%s': '%s'", names(values), unname(values)),
            collapse = ", "
          ), "}")
        }
        rgba <- function(hex, alpha) {
          v <- grDevices::col2rgb(hex)
          sprintf("rgba(%d,%d,%d,%s)", v[1L], v[2L], v[3L], alpha)
        }
        sev_fill_js <- js_color_map(vapply(sev_hex, rgba, "", alpha = "0.7"))
        sev_stroke_js <- js_color_map(vapply(sev_hex, rgba, "", alpha = "0.9"))
        sev_hex_js <- js_color_map(sev_hex)

        ae_end <- function(i) if (ae_use_day) adae$AENDY[i] else adae$AENDT[i]
        ae_x <- function(v) {
          pp_xval_pref_day(
            if (ae_use_day) NULL else v,
            if (ae_use_day) v else NULL,
            ref_ms, mode
          )
        }
        ae_lab <- function(v) {
          if (ae_use_day) pp_day_label(v) else pp_xlabel(v, ref_ms, mode)
        }

        ae_data <- lapply(seq_len(nrow(adae)), function(i) {
          s <- ae_x(adae[[ae_src]][i])
          e <- if (has_end && !is.na(ae_end(i))) {
            ae_x(ae_end(i))
          } else {
            s + day_unit
          }
          sev <- if (has_sev) {
            toupper(as.character(adae[[sev_col]][i]))
          } else {
            ""
          }
          ser <- if (has_ser) as.character(adae$AESER[i]) else ""
          term <- if (has_term) as.character(adae$AEDECOD[i]) else "AE"
          s_lab <- ae_lab(adae[[ae_src]][i])
          e_lab <- if (has_end && !is.na(ae_end(i))) {
            ae_lab(ae_end(i))
          } else {
            s_lab
          }
          list(value = list(s, e, ae_lane, term, sev, ser, s_lab, e_lab))
        })

        ae_series <- list(
          type = "custom",
          name = "Adverse Events",
          renderItem = htmlwidgets::JS(sprintf("
            function(params, api) {
              var start = api.coord([api.value(0), api.value(2)]);
              var end   = api.coord([api.value(1), api.value(2)]);
              var h     = api.size([0, 1])[1] * 0.45;
              var barW  = Math.max(end[0] - start[0], 4);
              // echarts coerces numeric-looking dims, so a CTCAE grade
              // arrives as a number here: stringify before any string op.
              var sev   = ('' + (api.value(4) || '')).toUpperCase();
              var ser   = api.value(5) || '';
              var sevFill = %s;
              var sevStroke = %s;
              var fill   = sevFill[sev]   || 'rgba(156,163,175,0.7)';
              var stroke = sevStroke[sev]  || 'rgba(156,163,175,0.9)';
              var children = [{
                type: 'rect',
                shape: {
                  x: start[0], y: start[1] - h/2,
                  width: barW, height: h, r: 2
                },
                style: { fill: fill, stroke: stroke, lineWidth: 1 }
              }];
              if (ser === 'Y') {
                children.push({
                  type: 'rect',
                  shape: {
                    x: start[0], y: start[1] - h/2,
                    width: barW, height: 2
                  },
                  style: { fill: '#DC2626' }
                });
              }
              return { type: 'group', children: children };
            }
          ", sev_fill_js, sev_stroke_js)),
          data = ae_data,
          encode = list(x = list(0, 1), y = 2),
          tooltip = list(
            formatter = htmlwidgets::JS(sprintf("
              function(params) {
                var v = params.value;
                var s = v[6] || '';
                var e = v[7] || '';
                var term = v[3] || '';
                var sev  = '' + (v[4] == null ? '' : v[4]);
                // A bare CTCAE grade reads as noise in the badge.
                var sevDisp = /^[0-9]+$/.test(sev) ? 'Grade ' + sev : sev;
                var ser  = v[5] || '';
                var sevColors = %s;
                var col = sevColors[sev] || '#9ca3af';
                var html = '<div style=\"min-width:160px\">';
                html += '<div style=\"font-size:13px;font-weight:600;' +
                  'margin-bottom:2px\">' + term + '</div>';
                if (sev) {
                  html += '<span style=\"display:inline-block;background:' +
                    col + ';color:#fff;padding:1px 6px;border-radius:3px;' +
                    'font-size:10px;font-weight:600;margin-bottom:3px\">' +
                    sevDisp + '</span>';
                  if (ser === 'Y') {
                    html += ' <span style=\"color:#DC2626;font-size:10px;' +
                      'font-weight:600\">SERIOUS</span>';
                  }
                  html += '<br/>';
                }
                html += '<div style=\"font-size:12px;color:#6b7280\">' +
                  s + ' \\u2192 ' + e + '</div></div>';
                return html;
              }
            ", sev_hex_js))
          )
        )

        all_series <- c(all_series, list(ae_series))
      }

      # ---------------------------------------------------------------
      # Milestones lane
      # ---------------------------------------------------------------
      ms_lane <- lane_idx[["MS"]]
      milestone_data <- list()

      # Treatment start (green filled circle)
      milestone_data <- c(milestone_data, list(list(
        value = list(trt_start, ms_lane, "trt_start", start_str)
      )))

      # Treatment end (green hollow circle)
      milestone_data <- c(milestone_data, list(list(
        value = list(trt_end, ms_lane, "trt_end", end_str)
      )))

      # End of study (blue diamond) — RFENDT is canonical, EOSDT aliased
      if ("RFENDT" %in% colnames(sl) && !is.na(sl$RFENDT[1])) {
        milestone_data <- c(milestone_data, list(list(
          value = list(pp_xval(sl$RFENDT[1], ref_ms, mode), ms_lane,
                       "eos", pp_xlabel(sl$RFENDT[1], ref_ms, mode))
        )))
      }

      # Death (red X) — DTHDT is canonical, DTHDTC aliased
      if ("DTHDT" %in% colnames(sl) && !is.na(sl$DTHDT[1])) {
        milestone_data <- c(milestone_data, list(list(
          value = list(pp_xval(sl$DTHDT[1], ref_ms, mode), ms_lane,
                       "death", pp_xlabel(sl$DTHDT[1], ref_ms, mode))
        )))
      } else if ("DTHFL" %in% colnames(sl) &&
                  !is.na(sl$DTHFL[1]) && sl$DTHFL[1] == "Y") {
        milestone_data <- c(milestone_data, list(list(
          value = list(trt_end, ms_lane, "death", "Date unknown")
        )))
      }

      ms_series <- list(
        type = "custom",
        name = "Milestones",
        renderItem = htmlwidgets::JS("
          function(params, api) {
            var x = api.coord([api.value(0), api.value(1)])[0];
            var y = api.coord([api.value(0), api.value(1)])[1];
            var kind = api.value(2);
            var sz = 6;
            if (kind === 'trt_start') {
              return {
                type: 'circle',
                shape: { cx: x, cy: y, r: sz },
                style: { fill: '#059669', stroke: '#fff', lineWidth: 1.5 }
              };
            } else if (kind === 'trt_end') {
              return {
                type: 'circle',
                shape: { cx: x, cy: y, r: sz },
                style: { fill: '#fff', stroke: '#059669', lineWidth: 2 }
              };
            } else if (kind === 'eos') {
              return {
                type: 'polygon',
                shape: {
                  points: [
                    [x, y - sz], [x + sz, y],
                    [x, y + sz], [x - sz, y]
                  ]
                },
                style: { fill: '#2563EB', stroke: '#fff', lineWidth: 1.5 }
              };
            } else if (kind === 'death') {
              return {
                type: 'group',
                children: [{
                  type: 'line',
                  shape: { x1: x-sz, y1: y-sz, x2: x+sz, y2: y+sz },
                  style: { stroke: '#DC2626', lineWidth: 2.5 }
                }, {
                  type: 'line',
                  shape: { x1: x+sz, y1: y-sz, x2: x-sz, y2: y+sz },
                  style: { stroke: '#DC2626', lineWidth: 2.5 }
                }]
              };
            }
          }
        "),
        data = milestone_data,
        encode = list(x = 0, y = 1),
        tooltip = list(
          formatter = htmlwidgets::JS("
            function(params) {
              var v = params.value;
              var kind = v[2];
              var date = v[3] || '';
              var labels = {
                'trt_start': 'Treatment Start',
                'trt_end':   'Treatment End',
                'eos':       'End of Study',
                'death':     'Death'
              };
              var colors = {
                'trt_start': '#059669',
                'trt_end':   '#059669',
                'eos':       '#2563EB',
                'death':     '#DC2626'
              };
              var label = labels[kind] || kind;
              var col = colors[kind] || '#6b7280';
              return '<div style=\"min-width:120px\">' +
                '<div style=\"font-size:13px;font-weight:600;color:' +
                col + '\">' + label + '</div>' +
                '<div style=\"font-size:12px;color:#6b7280\">' +
                date + '</div></div>';
            }
          ")
        )
      )

      all_series <- c(all_series, list(ms_series))

      # ---------------------------------------------------------------
      # Exposure lane (omitted when adex missing)
      # ---------------------------------------------------------------
      if (has_adex) {
        adex <- adex_raw[!is.na(adex_raw[[ex_src]]), , drop = FALSE]
        ex_lane <- lane_idx[["EX"]]
        ex_has_end <- if (ex_use_day) {
          "AENDY" %in% colnames(adex)
        } else {
          "AENDT" %in% colnames(adex)
        }
        opt_chr <- function(df, col, i) {
          if (col %in% colnames(df)) {
            v <- df[[col]][i]
            if (is.na(v)) "" else as.character(v)
          } else {
            ""
          }
        }
        ex_end <- function(i) if (ex_use_day) adex$AENDY[i] else adex$AENDT[i]
        ex_x <- function(v) {
          pp_xval_pref_day(
            if (ex_use_day) NULL else v,
            if (ex_use_day) v else NULL,
            ref_ms, mode
          )
        }
        ex_lab <- function(v) {
          if (ex_use_day) pp_day_label(v) else pp_xlabel(v, ref_ms, mode)
        }
        day_unit <- if (identical(mode, "rday")) 1 else 86400000

        # ADaM adex is parameterized (one row per PARAMCD per period);
        # the lane wants each dosing period once.
        dose_of <- function(i) {
          trimws(paste(opt_chr(adex, "EXDOSE", i), opt_chr(adex, "EXDOSU", i)))
        }
        key <- vapply(seq_len(nrow(adex)), function(i) {
          paste(adex[[ex_src]][i], if (ex_has_end) ex_end(i) else "",
                dose_of(i))
        }, character(1L))
        adex <- adex[!duplicated(key), , drop = FALSE]

        ex_data <- lapply(seq_len(nrow(adex)), function(i) {
          x0 <- ex_x(adex[[ex_src]][i])
          x1 <- if (ex_has_end && !is.na(ex_end(i))) {
            ex_x(ex_end(i))
          } else {
            x0 + day_unit
          }
          s_lab <- ex_lab(adex[[ex_src]][i])
          e_lab <- if (ex_has_end && !is.na(ex_end(i))) {
            ex_lab(ex_end(i))
          } else {
            s_lab
          }
          list(value = list(
            x0, x1, ex_lane, dose_of(i), opt_chr(adex, "EXTRT", i),
            s_lab, e_lab
          ))
        })

        ex_series <- list(
          type = "custom",
          name = "Exposure",
          renderItem = htmlwidgets::JS("
            function(params, api) {
              var start = api.coord([api.value(0), api.value(2)]);
              var end   = api.coord([api.value(1), api.value(2)]);
              var h     = api.size([0, 1])[1] * 0.45;
              var barW  = Math.max(end[0] - start[0], 4);
              var children = [{
                type: 'rect',
                shape: {
                  x: start[0], y: start[1] - h/2,
                  width: barW, height: h, r: 2
                },
                style: {
                  fill: 'rgba(37,99,235,0.25)',
                  stroke: 'rgba(37,99,235,0.55)',
                  lineWidth: 1
                }
              }];
              // Dose text when the bar has room for it. echarts coerces
              // numeric-looking dims, so stringify before use.
              var dose = '' + (api.value(3) == null ? '' : api.value(3));
              if (dose && barW > 46) {
                children.push({
                  type: 'text',
                  style: {
                    text: dose,
                    x: start[0] + barW / 2,
                    y: start[1],
                    fill: '#1e40af',
                    fontSize: 10,
                    fontWeight: 600,
                    fontFamily: 'system-ui, -apple-system, sans-serif',
                    textAlign: 'center',
                    textVerticalAlign: 'middle',
                    truncate: { outerWidth: barW - 8 }
                  }
                });
              }
              return { type: 'group', children: children };
            }
          "),
          data = ex_data,
          encode = list(x = list(0, 1), y = 2),
          tooltip = list(
            formatter = htmlwidgets::JS("
              function(params) {
                var v = params.value;
                var dose = '' + (v[3] == null ? '' : v[3]);
                var trt = v[4] || '';
                var s = v[5] || '';
                var e = v[6] || '';
                var html = '<div style=\"min-width:160px\">';
                html += '<div style=\"font-size:13px;font-weight:600;' +
                  'margin-bottom:2px\">' + (trt || 'Exposure') + '</div>';
                if (dose) {
                  html += '<div style=\"font-size:12px\">Dose: <b>' +
                    dose + '</b></div>';
                }
                html += '<div style=\"font-size:12px;color:#6b7280\">' +
                  s + ' \u2192 ' + e + '</div></div>';
                return html;
              }
            ")
          )
        )

        all_series <- c(all_series, list(ex_series))
      }

      # ---------------------------------------------------------------
      # Visit ruler (omitted when nothing carries visits)
      # ---------------------------------------------------------------
      if (has_vis) {
        vis_lane <- lane_idx[["VIS"]]
        vis_data <- lapply(seq_len(nrow(visits)), function(i) {
          x <- pp_xval_pref_day(
            if (is.na(visits$date[i])) NULL else visits$date[i],
            if (is.na(visits$day[i])) NULL else visits$day[i],
            ref_ms, mode
          )
          x_lab <- if (identical(mode, "rday") && !is.na(visits$day[i])) {
            pp_day_label(visits$day[i])
          } else {
            pp_xlabel(visits$date[i], ref_ms, mode)
          }
          list(value = list(x, vis_lane, visits$visit[i], x_lab))
        })

        vis_series <- list(
          type = "custom",
          name = "Visits",
          renderItem = htmlwidgets::JS("
            function(params, api) {
              var p = api.coord([api.value(0), api.value(1)]);
              var h = api.size([0, 1])[1] * 0.4;
              return {
                type: 'rect',
                shape: { x: p[0] - 1, y: p[1] - h / 2, width: 2, height: h },
                style: { fill: '#9ca3af' }
              };
            }
          "),
          data = vis_data,
          encode = list(x = 0, y = 1),
          tooltip = list(
            formatter = htmlwidgets::JS("
              function(params) {
                var v = params.value;
                return '<div style=\"min-width:120px\">' +
                  '<div style=\"font-size:13px;font-weight:600\">' +
                  (v[2] || 'Visit') + '</div>' +
                  '<div style=\"font-size:12px;color:#6b7280\">' +
                  (v[3] || '') + '</div></div>';
              }
            ")
          )
        )

        all_series <- c(all_series, list(vis_series))
      }

      # ---------------------------------------------------------------
      # Assemble chart
      # ---------------------------------------------------------------
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
            data = lanes,
            inverse = TRUE,
            axisLine = list(show = FALSE),
            axisTick = list(show = FALSE),
            axisLabel = list(
              color = PP_AXIS_LABEL_COLOR, fontSize = 11, fontWeight = 500
            ),
            splitLine = list(show = FALSE)
          ),
          series = all_series
        )) |>
        echarts4r::e_text_style(fontFamily = "system-ui, -apple-system, sans-serif")
    }
)
