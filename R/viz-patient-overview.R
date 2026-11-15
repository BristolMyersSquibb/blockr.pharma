# Patient Profile Viz: Patient Overview
#
# Multi-lane overview chart (~180px) combining treatment timeline, adverse
# events, and key milestones in a single compact chart.
#
# Lanes (each data-gated; absent tables simply drop their lane):
#   1. Treatment — green bar from TRTSDT to TRTEDT with arm label
#   2. Exposure — a thin line per drug in its colour, the drug's name above
#      it and its dose in the tooltip, as the patient timeline draws
#      treatment (pp-timeline-look.R)
#   3. Adverse Events — compact overlapping bars colored by severity, the
#      worst on top
#   4. Con meds — compact bars, started before or on treatment, as the
#      patient timeline colours them
#   5. Milestones — point markers: end of study, death
#   6. Visits — one tick per visit the subject attended (reconstructed from
#      the findings tables; see pp_visit_schedule())
#
# Data requirements (declared via new_pp_viz()):
#   adsl: required TRTSDT, TRTEDT; optional RFENDT, DTHDT, DTHFL
#   adae (optional table): ASTDT, AENDT, ASTDY, AENDY, AEDECOD, AESER
#   adex (optional table): ASTDT, AENDT, ASTDY, AENDY, EXDOSE, EXDOSU, EXTRT,
#         AVISIT
#   roles: arm (the ADSL arm column, settings$roles$arm) and severity
#          (the ADAE severity column, settings$roles$severity)
#
# All names are canonical: pp_normalize_dm() reconciles a study's spellings
# (EOSDT, DTHDTC, ASTDTC, ...) dm-wide before anything renders.

#' Patient Overview visualization definition
#' @noRd
patient_overview_viz <- new_pp_viz(
  id = "patient_overview",
  label = "Patient overview",
  domain = "Treatment",
  icon = "capsule",
  color = "#059669",
  description = "Treatment, adverse events, con meds, visits & milestones",
  tables = "adsl",
  requires = list(adsl = c("TRTSDT", "TRTEDT")),
  uses = c("arm", "severity"),
  optional = list(
    adsl = c("RFENDT", "DTHDT", "DTHFL"),
    adae = c(
      "ASTDT", "AENDT", "ASTDY", "AENDY", "AEDECOD", "AESER", "AEBODSYS",
      "AEOUT"
    ),
    adex = c(
      "ASTDT", "AENDT", "ASTDY", "AENDY", "EXDOSE", "EXDOSU", "EXTRT",
      "AVISIT"
    ),
    adcm = c("ASTDT", "AENDT", "ASTDY", "AENDY", "CMTRT", "CMDECOD", "CMCLAS",
             "CMINDC")
  ),
  exhibit = function(dm_obj, time_range, settings = list(),
                     ref_ms = NA_real_, mode = "date") {
    pp_static_overview(dm_obj, time_range, settings, ref_ms, mode)
  },
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
      # Nothing here derives a timepoint (see pp-cycle.R). A dose bar prints
      # the visit label on its own row, and a visit tick IS a visit label. AE
      # bars print neither: an event has a start date of its own, and where an
      # occurrence dataset does carry AVISIT it names the visit the event was
      # COLLECTED at, which can be a fortnight after onset. Printing that
      # beside the onset date would read as the visit the AE started in.
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
      # collapse by (drug, start, end) -- see the lane itself, below.
      has_adex <- "adex" %in% names(tbls)
      ex_use_day <- FALSE
      ex_drugs <- character()
      if (has_adex) {
        adex_raw <- as.data.frame(tbls[["adex"]])
        ex_use_day <- identical(mode, "rday") &&
          "ASTDY" %in% colnames(adex_raw)
        ex_src <- if (ex_use_day) "ASTDY" else "ASTDT"
        has_adex <- ex_src %in% colnames(adex_raw) &&
          nrow(adex_raw[!is.na(adex_raw[[ex_src]]), , drop = FALSE]) > 0
        # A combination regimen doses two drugs on the same day, and one lane
        # would draw them on top of each other -- the reported symptom was a
        # Day 1 tooltip that showed one of the two. Each drug gets a slot,
        # sorted so the same drug holds the same slot on every patient.
        if (has_adex && "EXTRT" %in% colnames(adex_raw)) {
          ex_drugs <- sort(unique(trimws(stats::na.omit(
            as.character(adex_raw$EXTRT)
          ))))
          ex_drugs <- ex_drugs[nzchar(ex_drugs)]
        }
      }

      # Con-med lane, same date-or-day gating.
      has_adcm <- "adcm" %in% names(tbls)
      cm_use_day <- FALSE
      if (has_adcm) {
        adcm_raw <- as.data.frame(tbls[["adcm"]])
        cm_use_day <- identical(mode, "rday") &&
          "ASTDY" %in% colnames(adcm_raw)
        cm_src <- if (cm_use_day) "ASTDY" else "ASTDT"
        has_adcm <- cm_src %in% colnames(adcm_raw) &&
          any(!is.na(adcm_raw[[cm_src]]))
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

      # ONE treatment lane, not three. ADSL's TRTSDT/TRTEDT are the min/max
      # of the exposure records -- on pharmaverseadam, TRTSDT equals the first
      # dose date for 100% of subjects and TRTEDT the last for 98.4% (the six
      # exceptions off by a day). So a Treatment bar beside an Exposure lane
      # draws the same fact twice, and draws it worse: an envelope from first
      # to last dose asserts continuous treatment, where the exposure records
      # show the holds. Exposure fills the lane where it exists; the envelope
      # is the fallback for a study shipping no adex.
      #
      # Milestones fold in for the same reason. They were a lane whose first
      # two items were the treatment bar's own endpoints redrawn as circles --
      # the same two dates a third time. What is left (end of study, death)
      # are POINTS, and a point does not need a category of its own.
      #
      # Short lane labels are what let the shared gutter (PP_GRID_LEFT) stay
      # narrow. Tooltips on each item still carry the full context.
      lanes <- c("TRT", if (has_adae) "AE", if (has_adcm) "CM",
                 if (has_vis) "VIS")
      lane_full <- c(TRT = "Treatment", AE = "Adverse Events",
                     CM = "Con meds", VIS = "Visits")
      lane_idx <- stats::setNames(seq_along(lanes) - 1L, lanes)
      n_lanes <- length(lanes)
      # Chrome plus an exact 40px per lane, so lane geometry does not shift
      # with the number of lanes a subject happens to have. A category axis
      # divides the grid evenly, so a treatment lane carrying three or more
      # drug slots can only get taller by making every lane taller -- which
      # is the right trade at that point: a 13px slot is not a bar.
      #
      # Bounded, because the drug count is the study's, not ours: a regimen
      # is two or three drugs, but a basket protocol can name a dozen, and an
      # overview that grows to 500px stops being an overview. Past the cap the
      # slots thin out instead. Everything is still drawn and still hoverable.
      #
      # A drug is a thin line with its name above it, so each takes about
      # 18px of the lane; past four drugs the names drop and the lines stay.
      lane_px <- 40 + min(max(0L, length(ex_drugs) - 1L), 3L) * 18
      chart_height <- PP_PLOT_TOP + 30 + n_lanes * lane_px

      # A study's arm label is data, not a literal: encode it, never paste it.
      arm_js <- pp_js_str(arm_label)

      # ---------------------------------------------------------------
      # Treatment envelope — FALLBACK ONLY (no adex). Carries the arm label,
      # which is why it is worth drawing at all when it is all we have; with
      # exposure present the arm is a click away in the subject picker header
      # and each drug line carries its name.
      # ---------------------------------------------------------------
      trt_lane <- lane_idx[["TRT"]]
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
                  width: barW, height: h,
                  r: Math.min(2, h / 4, barW / 2)
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
                  fontFamily: PatientProfile.ink('--bs-body-font-family'),
                  textVerticalAlign: 'middle',
                  truncate: { outerWidth: barW - 16 }
                }
              }]
            };
          }
        ", arm_js)),
        data = list(list(
          value = list(trt_start, trt_end, trt_lane),
          tip = pp_tip_event(
            arm_label, color = "#059669", type = "Treatment",
            days = pp_tip_span(
              pp_tip_when(sl$TRTSDT[1], NA, ref_ms, mode),
              pp_tip_when(sl$TRTEDT[1], NA, ref_ms, mode)
            )
          )
        )),
        encode = list(x = list(0, 1), y = 2),
        tooltip = list(formatter = PP_TIP_FORMATTER)
      )

      all_series <- if (has_adex) list() else list(trt_series)

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
        # Solid, as on the AE panel and the timeline.
        sev_fill_js <- js_color_map(vapply(sev_hex, rgba, "", alpha = "1"))
        sev_stroke_js <- js_color_map(vapply(sev_hex, rgba, "", alpha = "1"))

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

        # The worst drawn last, so it shows where AEs overlap, as on the
        # patient timeline's stripe.
        sev_rank <- function(v) {
          v <- toupper(as.character(v))
          r <- match(v, c("MILD", "MODERATE", "SEVERE"))
          g <- suppressWarnings(as.integer(v))
          ifelse(is.na(r), ifelse(is.na(g), 0L, g), r)
        }
        if (has_sev) {
          adae <- adae[order(sev_rank(adae[[sev_col]])), , drop = FALSE]
        }
        ae_data <- lapply(seq_len(nrow(adae)), function(i) {
          s <- ae_x(adae[[ae_src]][i])
          # A missing AENDT means the AE was still running at data cut --
          # the state a safety reviewer scans for. Run it to the window edge
          # rather than drawing a resolved same-day blip.
          e <- if (has_end && !is.na(ae_end(i))) {
            ae_x(ae_end(i))
          } else {
            pp_gantt_open_end(s, time_range, ref_ms, mode, day_unit)
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
            PP_ONGOING_LABEL
          }
          at <- function(nm) if (nm %in% colnames(adae)) adae[[nm]][i] else NA
          # The AE panel's tooltip, as the timeline's AE segments read.
          list(
            value = list(s, e, ae_lane, term, sev, ser, s_lab, e_lab,
                         !(has_end && !is.na(ae_end(i)))),
            tip = pp_tip_ae(
              term,
              color = if (sev %in% names(sev_hex)) sev_hex[[sev]] else "#9ca3af",
              sev = if (has_sev) adae[[sev_col]][i],
              serious = identical(ser, "Y"),
              bodsys = at("AEBODSYS"), outcome = at("AEOUT"),
              days = pp_tip_span(
                pp_tip_when(at("ASTDT"), at("ASTDY"), ref_ms, mode),
                pp_tip_when(at("AENDT"), at("AENDY"), ref_ms, mode),
                open = !(has_end && !is.na(ae_end(i)))
              )
            )
          )
        })

        ae_series <- list(
          type = "custom",
          name = "Adverse Events",
          renderItem = htmlwidgets::JS(sprintf("
            function(params, api) {
              var start = api.coord([api.value(0), api.value(2)]);
              var end   = api.coord([api.value(1), api.value(2)]);
              var h     = %d;
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
                  width: barW, height: h,
                  r: Math.min(2, h / 4, barW / 2)
                },
                // Serious: the dark outline of the AE panel and the
                // timeline; severity keeps the fill.
                style: ser === 'Y'
                  ? { fill: fill, stroke: '#111827', lineWidth: 1.5 }
                  : { fill: fill }
              }];
              if (api.value(8)) {
                var cy = start[1], a = Math.max(h * 0.42, 3);
                var ax = start[0] + barW + 2;
                children.push({
                  type: 'polyline',
                  shape: { points: [[ax, cy - a], [ax + a, cy], [ax, cy + a]] },
                  style: { fill: 'none', stroke: fill, lineWidth: 1.6,
                           lineCap: 'round', lineJoin: 'round' }
                });
              }
              return { type: 'group', children: children };
            }
          ", PP_GANTT_BAR_H, sev_fill_js, sev_stroke_js)),
          data = ae_data,
          encode = list(x = list(0, 1), y = 2),
          tooltip = list(formatter = PP_TIP_FORMATTER)
        )

        all_series <- c(all_series, list(ae_series))
      }

      # ---------------------------------------------------------------
      # Con meds lane (omitted when adcm missing): compact bars, started
      # before or on treatment, as the patient timeline colours them.
      # ---------------------------------------------------------------
      if (has_adcm) {
        adcm <- adcm_raw[!is.na(adcm_raw[[cm_src]]), , drop = FALSE]
        cm_lane <- lane_idx[["CM"]]
        cm_has_end <- if (cm_use_day) {
          "AENDY" %in% colnames(adcm)
        } else {
          "AENDT" %in% colnames(adcm)
        }
        cm_end <- function(i) if (cm_use_day) adcm$AENDY[i] else adcm$AENDT[i]
        cm_x <- function(v) {
          pp_xval_pref_day(
            if (cm_use_day) NULL else v,
            if (cm_use_day) v else NULL,
            ref_ms, mode
          )
        }
        day_unit <- if (identical(mode, "rday")) 1 else 86400000
        cm_on <- pp_cm_started_on(adcm, sl$TRTSDT[1])
        cm_name <- pp_cm_lane_values(
          adcm, if ("CMDECOD" %in% colnames(adcm)) "CMDECOD" else "CMTRT"
        )
        cm_data <- lapply(seq_len(nrow(adcm)), function(i) {
          s <- cm_x(adcm[[cm_src]][i])
          open <- !(cm_has_end && !is.na(cm_end(i)))
          e <- if (open) {
            pp_gantt_open_end(s, time_range, ref_ms, mode, day_unit)
          } else {
            cm_x(cm_end(i))
          }
          col <- pp_cm_started_colour(cm_on[[i]])
          at <- function(nm) if (nm %in% colnames(adcm)) adcm[[nm]][i] else NA
          list(
            value = list(s, e, cm_lane, col, open),
            # The con-med panel's tooltip, as the timeline's con meds read.
            tip = pp_tip_cm(
              swimmer_drug_name(cm_name[[i]]), color = col, on = cm_on[[i]],
              indication = at(settings$roles$indication %||% "CMINDC"),
              klass = if (pp_cm_uncoded(at("CMCLAS"))) NULL else at("CMCLAS"),
              days = pp_tip_span(
                pp_tip_when(at("ASTDT"), at("ASTDY"), ref_ms, mode),
                pp_tip_when(at("AENDT"), at("AENDY"), ref_ms, mode),
                open = open
              )
            )
          )
        })
        # Started on treatment drawn last, over the long prior meds.
        cm_data <- cm_data[order(cm_on)]

        cm_series <- list(
          type = "custom",
          name = "Con meds",
          renderItem = htmlwidgets::JS(sprintf("
            function(params, api) {
              var start = api.coord([api.value(0), api.value(2)]);
              var end   = api.coord([api.value(1), api.value(2)]);
              var h     = %d;
              var barW  = Math.max(end[0] - start[0], 4);
              var cs    = params.coordSys;
              var rect  = echarts.graphic.clipRectByRect(
                { x: start[0], y: start[1] - h / 2, width: barW, height: h },
                { x: cs.x, y: cs.y, width: cs.width, height: cs.height }
              );
              if (!rect) return;
              var fill = api.value(3);
              var children = [{
                type: 'rect',
                shape: Object.assign({}, rect, { r: %s }),
                style: { fill: fill }
              }];
              if (api.value(4)) {
                var cy = rect.y + rect.height / 2, a = Math.max(h * 0.42, 3);
                var ax = rect.x + rect.width + 2;
                children.push({
                  type: 'polyline',
                  shape: { points: [[ax, cy - a], [ax + a, cy], [ax, cy + a]] },
                  style: { fill: 'none', stroke: fill, lineWidth: 1.6,
                           lineCap: 'round', lineJoin: 'round' }
                });
              }
              return { type: 'group', children: children };
            }
          ", PP_GANTT_BAR_H, pp_mark_radius_js("rect.height", "rect.width"))),
          data = cm_data,
          encode = list(x = list(0, 1), y = 2),
          tooltip = list(formatter = PP_TIP_FORMATTER)
        )

        all_series <- c(all_series, list(cm_series))
      }

      # ---------------------------------------------------------------
      # Milestones — markers ON the treatment lane. Treatment start/end are
      # deliberately NOT among them any more: they were circles sitting under
      # the very bar whose ends they marked. What remains genuinely happens
      # elsewhere in time, usually after treatment stops.
      # ---------------------------------------------------------------
      ms_lane <- trt_lane
      milestone_data <- list()

      # End of study (blue diamond) — RFENDT is canonical, EOSDT aliased
      if ("RFENDT" %in% colnames(sl) && !is.na(sl$RFENDT[1])) {
        eos <- pp_tip_when(sl$RFENDT[1], NA, ref_ms, mode)
        milestone_data <- c(milestone_data, list(list(
          value = list(pp_xval(sl$RFENDT[1], ref_ms, mode), ms_lane,
                       "eos", pp_xlabel(sl$RFENDT[1], ref_ms, mode)),
          tip = pp_tip_event("End of study", color = "#2563EB",
                             days = pp_tip_day(eos))
        )))
      }

      # Death (red X) — DTHDT is canonical, DTHDTC aliased
      if ("DTHDT" %in% colnames(sl) && !is.na(sl$DTHDT[1])) {
        dth <- pp_tip_when(sl$DTHDT[1], NA, ref_ms, mode)
        milestone_data <- c(milestone_data, list(list(
          value = list(pp_xval(sl$DTHDT[1], ref_ms, mode), ms_lane,
                       "death", pp_xlabel(sl$DTHDT[1], ref_ms, mode)),
          tip = pp_tip_event("Death", color = "#DC2626",
                             days = pp_tip_day(dth))
        )))
      } else if ("DTHFL" %in% colnames(sl) &&
                  !is.na(sl$DTHFL[1]) && sl$DTHFL[1] == "Y") {
        # Flagged without a date, so the marker sits at the end of
        # treatment; the tooltip says so.
        milestone_data <- c(milestone_data, list(list(
          value = list(trt_end, ms_lane, "death", "Date unknown"),
          tip = pp_tip("Death", color = "#DC2626",
                       note = "Date not recorded; drawn at the end of treatment")
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
            if (kind === 'eos') {
              return {
                type: 'polygon',
                shape: {
                  points: [
                    [x, y - sz], [x + sz, y],
                    [x, y + sz], [x - sz, y]
                  ]
                },
                style: { fill: '#2563EB', stroke: PatientProfile.ink('--blockr-color-bg-surface'), lineWidth: 1.5 }
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
        tooltip = list(formatter = PP_TIP_FORMATTER)
      )

      # Appended AFTER exposure, below -- the milestones now share a lane with
      # the dose bars, and echarts paints in series order, so adding them here
      # would hide a death marker under the bar it lands on (the DTHFL branch
      # above puts one exactly there).

      # ---------------------------------------------------------------
      # Exposure — the treatment lane's real content when adex exists. Drug
      # lines say everything the envelope did and the holds besides.
      # ---------------------------------------------------------------
      if (has_adex) {
        adex <- adex_raw[!is.na(adex_raw[[ex_src]]), , drop = FALSE]
        ex_lane <- trt_lane
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
        day_unit <- if (identical(mode, "rday")) 1 else 86400000

        # The dose, in the study's own words. SDTM/ADaM keeps the amount and
        # its unit in two columns, and a study dosing on a per-weight
        # regimen ships that unit here -- so a record carrying "10 mg/kg"
        # reaches the tooltip by being read, never by this viz dividing a
        # dose by a weight it was not given.
        # A zero dose (placebo) is no dose to show.
        dose_of <- function(i) {
          amt <- suppressWarnings(as.numeric(opt_chr(adex, "EXDOSE", i)))
          if (!is.na(amt) && amt == 0) return("")
          trimws(paste(opt_chr(adex, "EXDOSE", i), opt_chr(adex, "EXDOSU", i)))
        }
        drug_of <- function(i) trimws(opt_chr(adex, "EXTRT", i))

        # ADaM adex is parameterized (one row per PARAMCD per period), so the
        # lane wants each administration once -- but ONCE PER DRUG. Keying on
        # (start, end, dose) alone, as this did, made a combination regimen's
        # two same-day infusions one bar whenever their dose strings matched,
        # and stacked them where they did not.
        key <- vapply(seq_len(nrow(adex)), function(i) {
          paste(drug_of(i), adex[[ex_src]][i],
                if (ex_has_end) ex_end(i) else "")
        }, character(1L))
        groups <- split(seq_len(nrow(adex)), factor(key, levels = unique(key)))

        # Which slot of the treatment lane a drug draws in. Unnamed doses
        # (no EXTRT) all share the single slot they had before.
        slot_of <- function(drug) {
          if (!length(ex_drugs) || !nzchar(drug)) return(0L)
          match(drug, ex_drugs) - 1L
        }
        n_slot <- max(length(ex_drugs), 1L)

        # Each drug in its colour, in its slot, as the patient timeline draws
        # treatment: the timeline's drug palette (pt_drug_palette()) and its
        # names (swimmer_drug_name()).
        drug_col <- pt_drug_palette(ex_drugs)
        # Its dose before, for the tooltip, where the dose changed.
        g_drug <- vapply(groups, function(r) drug_of(r[[1L]]), "")
        g_start <- vapply(groups, function(r) {
          as.numeric(ex_x(adex[[ex_src]][r[[1L]]]))
        }, 0)
        g_dose <- vapply(groups, function(r) {
          d <- unique(vapply(r, dose_of, character(1L)))
          paste(d[nzchar(d)], collapse = " · ")
        }, "")
        prev_dose <- rep("", length(groups))
        first_of_drug <- rep(FALSE, length(groups))
        for (d in unique(g_drug)) {
          k <- which(g_drug == d)
          k <- k[order(g_start[k])]
          first_of_drug[k[[1L]]] <- TRUE
          if (length(k) > 1L) {
            prev <- g_dose[k[-length(k)]]
            chg <- prev != g_dose[k[-1L]]
            prev_dose[k[-1L]][chg] <- prev[chg]
          }
        }

        ex_data <- lapply(seq_along(groups), function(g) {
          rows <- groups[[g]]
          i <- rows[[1L]]
          x0 <- ex_x(adex[[ex_src]][i])
          x1 <- if (ex_has_end && !is.na(ex_end(i))) {
            ex_x(ex_end(i))
          } else {
            pp_gantt_open_end(x0, time_range, ref_ms, mode, day_unit)
          }
          at <- function(nm) if (nm %in% colnames(adex)) adex[[nm]][i] else NA
          ex_open <- !(ex_has_end && !is.na(ex_end(i)))
          drug <- drug_of(i)
          name <- if (nzchar(drug)) swimmer_drug_name(drug) else "Exposure"
          col <- if (nzchar(drug) && drug %in% names(drug_col)) {
            unname(drug_col[[drug]])
          } else {
            SWIMMER_DRUGS[[1L]]
          }
          list(
            value = list(
              x0, x1, ex_lane, n_slot, slot_of(drug), col,
              if (first_of_drug[[g]]) name else "", ex_open
            ),
            # As the timeline's drug lines read: the drug and its dose, the
            # days with the dose before where it changed.
            tip = pp_tip_event(
              name, color = col, type = "Treatment",
              details = c(name, g_dose[[g]]),
              days = pp_tip_span(
                pp_tip_when(at("ASTDT"), at("ASTDY"), ref_ms, mode),
                pp_tip_when(at("AENDT"), at("AENDY"), ref_ms, mode),
                open = ex_open,
                after = if (nzchar(prev_dose[[g]])) {
                  paste("from", prev_dose[[g]])
                }
              )
            )
          )
        })

        ex_series <- list(
          type = "custom",
          name = "Exposure",
          renderItem = htmlwidgets::JS(sprintf("
            function(params, api) {
              var start = api.coord([api.value(0), api.value(2)]);
              var end   = api.coord([api.value(1), api.value(2)]);
              var laneH = api.size([0, 1])[1];
              var n     = Math.max(+api.value(3) || 1, 1);
              var k     = +api.value(4) || 0;
              var col   = api.value(5);
              var name  = api.value(6);
              var open  = !!api.value(7);
              var cs    = params.coordSys;
              // A slot per drug; the line sits low in it, the drug's name
              // above it, at the left of its first line.
              var band  = laneH * 0.9;
              var slot  = band / n;
              var yc    = start[1] - band / 2 + slot * (k + 0.5);
              var named = slot >= 16;
              var h     = %d;
              var y     = named ? yc + 4 : yc;
              var barW  = Math.max(end[0] - start[0], 2);
              var children = [{
                type: 'rect',
                shape: { x: start[0], y: y - h / 2, width: barW, height: h },
                style: { fill: col }
              }];
              if (open) {
                var a  = 3;
                var ax = start[0] + barW + 2;
                children.push({
                  type: 'polyline',
                  shape: { points: [[ax, y - a], [ax + a, y], [ax, y + a]] },
                  style: { fill: 'none', stroke: col, lineWidth: 1.6,
                           lineCap: 'round', lineJoin: 'round' }
                });
              }
              if (named && name) {
                var tx = Math.max(start[0], cs.x + 2);
                children.push({
                  type: 'text',
                  style: {
                    text: name,
                    x: tx,
                    y: y - 9,
                    fill: PatientProfile.ink('--blockr-color-text-muted'),
                    fontSize: 11,
                    fontFamily: PatientProfile.ink('--bs-body-font-family'),
                    textVerticalAlign: 'middle',
                    truncate: { outerWidth: cs.x + cs.width - tx }
                  }
                });
              }
              return { type: 'group', children: children };
            }
          ", PP_DRUG_LINE_H)),
          data = ex_data,
          encode = list(x = list(0, 1), y = 2),
          tooltip = list(formatter = PP_TIP_FORMATTER)
        )

        all_series <- c(all_series, list(ex_series))
      }

      # Milestones last: they share the treatment lane with the dose bars now,
      # and echarts paints in series order. Most patients reach neither, so
      # the series is legitimately empty -- it never was while it carried the
      # treatment endpoints.
      if (length(milestone_data)) {
        all_series <- c(all_series, list(ms_series))
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
          when <- pp_tip_when(visits$date[i], visits$day[i], ref_ms, mode)
          list(
            value = list(x, vis_lane, visits$visit[i], x_lab),
            tip = pp_tip_event(
              if (nzchar(pp_tip_str(visits$visit[i]))) {
                pp_tip_case(visits$visit[i])
              } else {
                "Visit"
              },
              color = "#6b7280", type = "Visit", days = pp_tip_day(when)
            )
          )
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
                style: { fill: PatientProfile.ink('--blockr-color-text-muted') }
              };
            }
          "),
          data = vis_data,
          encode = list(x = 0, y = 1),
          tooltip = list(formatter = PP_TIP_FORMATTER)
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
          grid = list(
            left = PP_GRID_LEFT, right = 20, top = PP_PLOT_TOP, bottom = 30,
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
        echarts4r::e_text_style(fontFamily = "var(--bs-body-font-family)")
    }
)
