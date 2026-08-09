# Static (ggplot) twins of the patient-profile visualizations.
#
# The interactive panels are echarts htmlwidgets, which exist only in a
# browser: the export pipeline (blockr.viz exhibits, blockr.outline decks)
# re-derives every picture server-side from the same state instead of
# screenshotting the canvas -- the same split the chart block draws between
# its live ECharts view and static_chart(). Each viz that can be exported
# declares an `exhibit` function with the SAME signature as its `render`
# (see new_pp_viz()), returning a ggplot instead of a widget. This file
# holds those twins plus the shared scaffolding (axis scales, theme, lane
# geometry) that keeps them visually consistent with the live panels.
#
# The twins reuse the interactive helpers verbatim -- pp_xval*(),
# pp_x_bounds(), pp_gantt_in_window(), pp_gantt_open_end() -- so both
# renderings agree about axis units (ms timestamps in date mode, continuous
# relative days in rday mode), window clipping and open-ended events. What
# they deliberately do not carry over: tooltips, hover states and the
# echarts toolbox, which have no meaning on paper.
#
# ggplot2 is a Suggests: everything here is behind pp_gg_require().

# ggplot2 aes() columns -- quasiquotation R CMD check cannot see through.
utils::globalVariables(c(
  "..x", "..panel", "..series", "..total", "..derived", "..pt_col",
  ".data", "AVAL", "xmin", "xmax", "ymin", "ymax", "y", "lo", "hi",
  "outlined", "position", "value", "visit", "param", "item", "x",
  "color", "fill", "label", "lwd", "series"
))

#' Assert ggplot2 (and stats helpers) are available for static rendering
#' @noRd
pp_gg_require <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Static patient-profile exhibits need the 'ggplot2' package.",
      call. = FALSE
    )
  }
}

#' @noRd
pp_gg_available <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}

#' Millisecond timestamp back to Date (inverse of pp_ms_ts())
#' @noRd
pp_ms_to_date <- function(ms) {
  as.Date(as.POSIXct(ms / 1000, origin = "1970-01-01", tz = "UTC"))
}

#' Shared x scale for the static panels, in the axis units the interactive
#' charts use (ms in date mode, continuous relative day in rday mode), so
#' every span computed by the pp_xval*() family plots unchanged.
#' @noRd
pp_static_x_scale <- function(time_range, ref_ms = NA_real_, mode = "date") {
  b <- pp_x_bounds(time_range, ref_ms, mode)
  limits <- if (anyNA(b)) NULL else b
  if (identical(mode, "rday") && !is.na(ref_ms)) {
    ggplot2::scale_x_continuous(
      limits = limits,
      breaks = function(lims) pretty(lims, n = 6),
      # Same skip-zero rule as the interactive axis: the day before D1 is
      # D-1, matching ADaM's *DY convention.
      labels = function(v) {
        ifelse(v > 0, paste0("D", round(v)), paste0("D", round(v) - 1))
      },
      expand = ggplot2::expansion(mult = c(0.01, 0.02))
    )
  } else {
    ggplot2::scale_x_continuous(
      limits = limits,
      breaks = function(lims) {
        pp_ms_ts(pretty(pp_ms_to_date(lims), n = 6))
      },
      labels = function(v) format(pp_ms_to_date(v)),
      expand = ggplot2::expansion(mult = c(0.01, 0.02))
    )
  }
}

#' Shared theme: the static face of the panel styling (muted axis text,
#' dashed hairline grid, no chart junk).
#' @noRd
pp_static_theme <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = PP_SPLIT_LINE_COLOR, linewidth = 0.4, linetype = "dashed"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        color = PP_AXIS_LABEL_COLOR, size = base_size * 0.85
      ),
      axis.title = ggplot2::element_text(
        color = PP_AXIS_LABEL_COLOR, size = base_size * 0.85
      ),
      strip.text = ggplot2::element_text(
        color = "#6b7280", size = base_size * 0.9, hjust = 0
      ),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(
        color = PP_AXIS_LABEL_COLOR, size = base_size * 0.85
      ),
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size = base_size * 1.1, face = "plain", color = "#374151"
      ),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

#' Stamp the slide-box size onto a plot, in the attribute contract
#' blockr.viz's gg exhibit methods read (`pptx_width` / `pptx_height`).
#' Heights are derived from the interactive panels' pixel heights at 96
#' px/in, so a ten-lane gantt gets a taller box than a three-lane one and
#' the deck's fit-to-slide scaling preserves that aspect.
#' @noRd
pp_static_sized <- function(p, height_px, width_in = 9) {
  attr(p, "pptx_width") <- width_in
  attr(p, "pptx_height") <- max(1.5, height_px / 96)
  p
}

#' Alpha-composited hex color (the static stand-in for the rgba() fills the
#' echarts renderers use).
#' @noRd
pp_alpha <- function(hex, alpha) {
  grDevices::adjustcolor(hex, alpha.f = alpha)
}

# ---------------------------------------------------------------------------
# Gantt twin (AE + CM)
# ---------------------------------------------------------------------------

#' Static gantt: one labelled lane per term, bars spanning start..end
#'
#' The shared twin of the AE / CM gantt renderItem. `bars` rows carry the
#' spans ALREADY in axis units and already window-filtered (the callers run
#' the same pp_gantt_in_window() pass as their interactive siblings).
#'
#' @param bars Data frame with `start`, `end`, `lane` (0-based, top first),
#'   `fill` (hex), `outlined` (logical; the AE gantt's serious outline),
#'   `ongoing` (logical) and `label` (lane label on the lane's first bar,
#'   `""` elsewhere).
#' @param time_range,ref_ms,mode The shared axis definition.
#' @return A ggplot, sized for the lane count.
#' @noRd
pp_static_gantt <- function(bars, time_range, ref_ms = NA_real_,
                            mode = "date") {
  pp_gg_require()
  n_lanes <- max(bars$lane) + 1L

  b <- pp_x_bounds(time_range, ref_ms, mode)
  clamp_lo <- function(x) if (is.na(b[1L])) x else pmax(x, b[1L])
  clamp_hi <- function(x) if (is.na(b[2L])) x else pmin(x, b[2L])

  # Lane geometry in lane units, mirroring the pixel constants: the label
  # row sits above the bar inside its own lane (PP_GANTT_LABEL_DY /
  # PP_GANTT_BAR_DY at a 38px lane).
  bars$y <- -bars$lane
  bars$xmin <- clamp_lo(bars$start)
  bars$xmax <- clamp_hi(bars$end)
  bar_c <- PP_GANTT_BAR_DY / PP_GANTT_LANE_H     # bar centre offset (down)
  bar_h <- PP_GANTT_BAR_H / PP_GANTT_LANE_H / 2  # half height
  lab_c <- -PP_GANTT_LABEL_DY / PP_GANTT_LANE_H  # label offset (up)

  labs_df <- bars[nzchar(bars$label), , drop = FALSE]
  ongoing_df <- bars[bars$ongoing, , drop = FALSE]

  p <- ggplot2::ggplot(bars) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = xmin, xmax = xmax,
        ymin = y - bar_c - bar_h, ymax = y - bar_c + bar_h,
        fill = I(fill),
        color = I(ifelse(outlined, "#111827", NA))
      ),
      linewidth = 0.4
    ) +
    pp_static_x_scale(time_range, ref_ms, mode) +
    ggplot2::scale_y_continuous(
      limits = c(-(n_lanes - 1L) - 0.55, 0.55),
      breaks = NULL, expand = ggplot2::expansion()
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    pp_static_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "none"
    )

  if (nrow(labs_df)) {
    p <- p + ggplot2::geom_text(
      data = labs_df,
      ggplot2::aes(x = xmin, y = y + lab_c, label = label),
      hjust = 0, size = 2.7, color = "#4b5563"
    )
  }
  if (nrow(ongoing_df)) {
    # The open-end caret: an event with no end date runs past the window
    # edge and says so, same as the interactive bars.
    p <- p + ggplot2::geom_text(
      data = ongoing_df,
      ggplot2::aes(x = xmax, y = y - bar_c, label = "\u203a",
                   color = I(fill)),
      hjust = 0.1, size = 3.6, fontface = "bold"
    )
  }

  pp_static_sized(p, pp_gantt_height(n_lanes))
}

#' Static twin of the AE gantt render
#' @noRd
pp_static_ae_gantt <- function(dm_obj, time_range, settings = list(),
                               ref_ms = NA_real_, mode = "date") {
  pp_gg_require()
  tbl <- as.data.frame(dm::dm_get_tables(dm_obj)[["adae"]])

  has_day <- "ASTDY" %in% colnames(tbl)
  use_day <- identical(mode, "rday") && has_day
  if (!use_day && !"ASTDT" %in% colnames(tbl)) return(NULL)
  tbl <- tbl[!is.na(if (use_day) tbl$ASTDY else tbl$ASTDT), , drop = FALSE]
  if (nrow(tbl) == 0) return(NULL)

  sev_color <- function(sev) {
    s <- as.character(sev)
    fixed <- settings$sev_colors
    if (!is.null(fixed)) {
      if (s %in% names(fixed)) return(unname(fixed[[s]]))
      if (toupper(s) %in% names(fixed)) return(unname(fixed[[toupper(s)]]))
    }
    pp_sev_fallback_color(s)
  }
  sev_col <- settings$roles$severity
  has_sev <- !is.null(sev_col) && sev_col %in% colnames(tbl)
  has_end <- if (use_day) "AENDY" %in% colnames(tbl) else
    "AENDT" %in% colnames(tbl)
  has_ser <- "AESER" %in% colnames(tbl)
  day_unit <- if (identical(mode, "rday")) 1 else 86400000
  end_at <- function(i) if (use_day) tbl$AENDY[i] else tbl$AENDT[i]

  bar_span <- function(i) {
    s <- pp_xval_pref_day(
      if (use_day) NULL else tbl$ASTDT[i],
      if (use_day) tbl$ASTDY[i] else NULL, ref_ms, mode
    )
    e <- if (has_end && !is.na(end_at(i))) {
      pp_xval_pref_day(
        if (use_day) NULL else tbl$AENDT[i],
        if (use_day) tbl$AENDY[i] else NULL, ref_ms, mode
      )
    } else {
      pp_gantt_open_end(s, time_range, ref_ms, mode, day_unit)
    }
    c(s, e)
  }

  spans <- vapply(seq_len(nrow(tbl)), bar_span, numeric(2L))
  keep <- pp_gantt_in_window(spans[1L, ], spans[2L, ], time_range,
                             ref_ms, mode)
  tbl <- tbl[keep, , drop = FALSE]
  spans <- spans[, keep, drop = FALSE]
  if (nrow(tbl) == 0) return(NULL)

  # Lane granularity follows the panel's own "Lanes" setting (see
  # pp-lanes.R): the printed twin groups its rows the way the screen does.
  lane_col <- pp_lane_column(tbl, PP_AE_LANES, settings$lanes, "AEDECOD")
  lane_lab <- pp_lane_values(tbl, lane_col %||% "AEDECOD", "AEDECOD")
  terms <- sort(unique(lane_lab))
  lane <- match(lane_lab, terms) - 1L
  first_of_lane <- !duplicated(lane[order(lane, spans[1L, ])])[
    order(order(lane, spans[1L, ]))
  ]

  sev <- if (has_sev) as.character(tbl[[sev_col]]) else
    rep("UNKNOWN", nrow(tbl))
  bars <- data.frame(
    start = spans[1L, ], end = spans[2L, ], lane = lane,
    fill = vapply(sev, sev_color, character(1L), USE.NAMES = FALSE),
    outlined = has_ser & toupper(as.character(
      if (has_ser) tbl$AESER else ""
    )) == "Y",
    ongoing = vapply(seq_len(nrow(tbl)), function(i) {
      !(has_end && !is.na(end_at(i)))
    }, logical(1L)),
    label = ifelse(first_of_lane, pp_term_label(lane_lab), ""),
    stringsAsFactors = FALSE
  )
  pp_static_gantt(bars, time_range, ref_ms, mode)
}

#' Static twin of the CM gantt render
#' @noRd
pp_static_cm_gantt <- function(dm_obj, time_range, settings = list(),
                               ref_ms = NA_real_, mode = "date") {
  pp_gg_require()
  tbl <- as.data.frame(dm::dm_get_tables(dm_obj)[["adcm"]])

  has_day <- "ASTDY" %in% colnames(tbl)
  use_day <- identical(mode, "rday") && has_day
  if (!use_day && !"ASTDT" %in% colnames(tbl)) return(NULL)
  tbl <- tbl[!is.na(if (use_day) tbl$ASTDY else tbl$ASTDT), , drop = FALSE]
  if (nrow(tbl) == 0) return(NULL)

  has_end <- if (use_day) "AENDY" %in% colnames(tbl) else
    "AENDT" %in% colnames(tbl)
  day_unit <- if (identical(mode, "rday")) 1 else 86400000
  end_at <- function(i) if (use_day) tbl$AENDY[i] else tbl$AENDT[i]

  bar_span <- function(i) {
    s <- pp_xval_pref_day(
      if (use_day) NULL else tbl$ASTDT[i],
      if (use_day) tbl$ASTDY[i] else NULL, ref_ms, mode
    )
    e <- if (has_end && !is.na(end_at(i))) {
      pp_xval_pref_day(
        if (use_day) NULL else tbl$AENDT[i],
        if (use_day) tbl$AENDY[i] else NULL, ref_ms, mode
      )
    } else {
      pp_gantt_open_end(s, time_range, ref_ms, mode, day_unit)
    }
    c(s, e)
  }

  spans <- vapply(seq_len(nrow(tbl)), bar_span, numeric(2L))
  keep <- pp_gantt_in_window(spans[1L, ], spans[2L, ], time_range,
                             ref_ms, mode)
  tbl <- tbl[keep, , drop = FALSE]
  spans <- spans[, keep, drop = FALSE]
  if (nrow(tbl) == 0) return(NULL)

  # Lane granularity follows the panel's own "Lanes" setting (see
  # pp-lanes.R): the printed twin groups its rows the way the screen does.
  lane_col <- pp_lane_column(tbl, PP_CM_LANES, settings$lanes, "CMDECOD")
  lane_lab <- pp_lane_values(tbl, lane_col %||% "CMTRT", "CMTRT")
  meds <- sort(unique(lane_lab))
  lane <- match(lane_lab, meds) - 1L
  first_of_lane <- !duplicated(lane[order(lane, spans[1L, ])])[
    order(order(lane, spans[1L, ]))
  ]

  # Indication colors, same precedence as the interactive bars: the injected
  # scale-map colors when they resolve, one medication color otherwise, grey
  # for rows carrying no indication while others do.
  default_color <- "#0891B2"
  indc_col <- settings$roles$indication
  has_indc <- !is.null(indc_col) && indc_col %in% colnames(tbl)
  indc_hex <- if (has_indc) settings$indc_colors else NULL
  fill <- if (is.null(indc_hex) || !length(indc_hex)) {
    rep(default_color, nrow(tbl))
  } else {
    indc <- as.character(tbl[[indc_col]])
    ifelse(indc %in% names(indc_hex),
           unname(unlist(indc_hex)[indc]), "#9ca3af")
  }

  bars <- data.frame(
    start = spans[1L, ], end = spans[2L, ], lane = lane,
    fill = fill, outlined = FALSE,
    ongoing = vapply(seq_len(nrow(tbl)), function(i) {
      !(has_end && !is.na(end_at(i)))
    }, logical(1L)),
    label = ifelse(first_of_lane, pp_term_label(lane_lab), ""),
    stringsAsFactors = FALSE
  )
  pp_static_gantt(bars, time_range, ref_ms, mode)
}

# ---------------------------------------------------------------------------
# Findings twin (labs / vitals / per-parameter cards, ADAS trajectory)
# ---------------------------------------------------------------------------

#' Static twin of pp_render_findings(): one panel per parameter, value line
#' with ANRIND-colored measurements and the reference band.
#'
#' Lines are straight segments (ggplot has no monotone interpolator and the
#' spline smoothers overshoot through values never measured -- the exact
#' reason the interactive chart uses monotone).
#' @noRd
pp_static_findings <- function(dm_obj, time_range, table_name, label,
                               paramcds = NULL, ref_ms = NA_real_,
                               mode = "date", smooth = "auto") {
  pp_gg_require()
  tbl <- pp_prepare_findings(dm_obj, table_name)
  if (is.null(tbl)) return(NULL)
  tbl <- tbl[!is.na(tbl$ADT) & !is.na(tbl$AVAL), , drop = FALSE]
  if (!is.null(paramcds)) {
    tbl <- tbl[tbl$PARAMCD %in% paramcds, , drop = FALSE]
  }
  if (nrow(tbl) == 0) return(NULL)

  anrind_colors <- c(H = "#dc2626", L = "#2563eb", N = "#059669")
  line_color <- blockr.theme::theme_palette("categorical", 1)

  params <- sort(unique(as.character(tbl$PARAMCD)))
  has_anrind <- "ANRIND" %in% colnames(tbl)
  has_ref <- all(c("A1LO", "A1HI") %in% colnames(tbl))
  has_dtype <- "DTYPE" %in% colnames(tbl)
  has_param <- "PARAM" %in% colnames(tbl)

  panel_label <- vapply(params, function(p) {
    if (!has_param) return(p)
    full <- as.character(tbl$PARAM[tbl$PARAMCD == p][1])
    if (is.na(full) || !nzchar(full)) return(p)
    if (nchar(full) > 40) full <- paste0(substr(full, 1, 37), "...")
    paste0(p, " \u2014 ", full)
  }, character(1L))

  tbl$..x <- pp_xval(tbl$ADT, ref_ms, mode)
  tbl$..panel <- factor(panel_label[as.character(tbl$PARAMCD)],
                        levels = unname(panel_label))
  tbl$..derived <- if (has_dtype) {
    !is.na(tbl$DTYPE) & nzchar(trimws(as.character(tbl$DTYPE)))
  } else {
    FALSE
  }
  tbl$..pt_col <- line_color
  if (has_anrind) {
    anr <- as.character(tbl$ANRIND)
    hit <- !is.na(anr) & anr %in% names(anrind_colors)
    tbl$..pt_col[hit] <- anrind_colors[anr[hit]]
  }
  tbl <- tbl[order(tbl$..panel, tbl$..x), , drop = FALSE]

  bands <- NULL
  if (has_ref) {
    bands <- do.call(rbind, lapply(params, function(p) {
      p_data <- tbl[tbl$PARAMCD == p, , drop = FALSE]
      lo <- stats::median(p_data$A1LO, na.rm = TRUE)
      hi <- stats::median(p_data$A1HI, na.rm = TRUE)
      if (is.na(lo) || is.na(hi)) return(NULL)
      data.frame(..panel = factor(panel_label[[p]],
                                  levels = unname(panel_label)),
                 lo = lo, hi = hi)
    }))
  }

  p <- ggplot2::ggplot(tbl, ggplot2::aes(x = ..x, y = AVAL))
  if (!is.null(bands) && nrow(bands)) {
    p <- p + ggplot2::geom_rect(
      data = bands,
      ggplot2::aes(ymin = lo, ymax = hi),
      xmin = -Inf, xmax = Inf,
      fill = pp_alpha("#059669", 0.06),
      color = pp_alpha("#059669", 0.15),
      linetype = "dashed", linewidth = 0.3,
      inherit.aes = FALSE
    )
  }
  p <- p +
    ggplot2::geom_line(color = line_color, linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_point(
      data = tbl[!tbl$..derived, , drop = FALSE],
      ggplot2::aes(color = I(..pt_col)), size = 1.8, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = tbl[tbl$..derived, , drop = FALSE],
      ggplot2::aes(color = I(..pt_col)),
      shape = 21, fill = "white", size = 1.8, stroke = 0.9, na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~..panel, ncol = 1L, scales = "free_y") +
    pp_static_x_scale(time_range, ref_ms, mode) +
    ggplot2::labs(x = NULL, y = NULL) +
    pp_static_theme() +
    ggplot2::theme(legend.position = "none")

  pp_static_sized(p, 40 + length(params) * 190)
}

#' Static twin of the ADAS-Cog trajectory render
#' @noRd
pp_static_adas <- function(dm_obj, time_range, settings = list(),
                           ref_ms = NA_real_, mode = "date") {
  pp_gg_require()
  tbl <- pp_prepare_findings(dm_obj, "adqsadas", num_cols = c("AVAL", "CHG"))
  if (is.null(tbl)) return(NULL)
  tbl <- tbl[!is.na(tbl$ADT) & !is.na(tbl$AVAL), , drop = FALSE]
  if (nrow(tbl) == 0) return(NULL)

  sel_items <- settings$items %||% "ACTOT"
  use_chg <- isTRUE(settings$chg)
  y_col <- if (use_chg && "CHG" %in% colnames(tbl)) "CHG" else "AVAL"
  tbl <- tbl[tbl$PARAMCD %in% sel_items & !is.na(tbl[[y_col]]), ,
             drop = FALSE]
  if (nrow(tbl) == 0) return(NULL)

  colors <- c(
    ACTOT = "#7C3AED",
    ACITM01 = "#2563EB", ACITM02 = "#DC2626", ACITM03 = "#059669",
    ACITM04 = "#D97706", ACITM05 = "#0891B2", ACITM06 = "#EA580C",
    ACITM07 = "#374151", ACITM08 = "#BE123C", ACITM09 = "#0D9488",
    ACITM10 = "#6366F1", ACITM11 = "#CA8A04", ACITM12 = "#9333EA",
    ACITM13 = "#E11D48", ACITM14 = "#14B8A6"
  )
  has_param <- "PARAM" %in% colnames(tbl)
  params <- sort(unique(as.character(tbl$PARAMCD)))
  series_label <- vapply(params, function(pc) {
    if (!has_param) return(pc)
    lab <- as.character(tbl$PARAM[tbl$PARAMCD == pc][1])
    if (is.na(lab) || !nzchar(lab)) pc else lab
  }, character(1L))
  series_color <- vapply(params, function(pc) {
    colors[[pc]] %||% "#6b7280"
  }, character(1L))

  tbl$..x <- pp_xval(tbl$ADT, ref_ms, mode)
  tbl$..series <- factor(series_label[as.character(tbl$PARAMCD)],
                         levels = unname(series_label))
  tbl$..total <- as.character(tbl$PARAMCD) == "ACTOT"
  tbl <- tbl[order(tbl$..series, tbl$..x), , drop = FALSE]

  p <- ggplot2::ggplot(
    tbl,
    ggplot2::aes(x = ..x, y = .data[[y_col]], color = ..series)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(linewidth = ifelse(..total, 0.8, 0.5)), na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = ifelse(..total, 2, 1.4)), na.rm = TRUE
    ) +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::scale_color_manual(
      values = stats::setNames(unname(series_color), unname(series_label))
    ) +
    pp_static_x_scale(time_range, ref_ms, mode) +
    ggplot2::labs(
      x = NULL,
      y = if (use_chg) "Change from Baseline" else "Score"
    ) +
    pp_static_theme() +
    ggplot2::theme(
      legend.position = if (length(params) > 1) "bottom" else "none"
    )

  pp_static_sized(p, if (length(params) > 1) 390 else 350)
}

# ---------------------------------------------------------------------------
# Orthostatic BP twin
# ---------------------------------------------------------------------------

#' Static twin of the orthostatic BP render
#' @noRd
pp_static_ortho_bp <- function(dm_obj, time_range, settings = list(),
                               ref_ms = NA_real_, mode = "date") {
  pp_gg_require()
  tbl <- pp_prepare_findings(dm_obj, "advs")
  if (is.null(tbl)) return(NULL)

  bp <- tbl[tbl$PARAMCD %in% c("SYSBP", "DIABP") & !is.na(tbl$AVAL) &
              nzchar(trimws(tbl$ATPT)), , drop = FALSE]
  if (nrow(bp) == 0) return(NULL)

  has_avisit <- "AVISIT" %in% colnames(bp)
  pos_map <- c(
    "AFTER LYING DOWN FOR 5 MINUTES" = "Lying",
    "AFTER STANDING FOR 1 MINUTE" = "Standing 1m",
    "AFTER STANDING FOR 3 MINUTES" = "Standing 3m",
    "SUPINE" = "Lying",
    "SEMI-RECUMBENT" = "Semi-recumbent",
    "SITTING" = "Sitting",
    "STANDING" = "Standing"
  )
  bp$position <- pos_map[toupper(trimws(bp$ATPT))]
  bp <- bp[!is.na(bp$position), , drop = FALSE]
  if (nrow(bp) == 0) return(NULL)

  positions <- intersect(
    c("Lying", "Semi-recumbent", "Sitting", "Standing",
      "Standing 1m", "Standing 3m"),
    unique(bp$position)
  )
  all_visits <- if (has_avisit) pp_visit_levels(bp) else character(0)
  sel_visits <- settings$visits
  if (is.null(sel_visits) || length(sel_visits) == 0) {
    sel_visits <- utils::tail(all_visits, 2)
  }
  sel_visits <- intersect(sel_visits, all_visits)
  if (length(sel_visits) == 0 && length(all_visits) > 0) {
    sel_visits <- utils::tail(all_visits, 2)
  }

  visit_colors <- c(
    "#2563EB", "#DC2626", "#059669", "#D97706",
    "#7C3AED", "#0891B2", "#EA580C", "#374151",
    "#E11D48", "#14B8A6"
  )
  param_labels <- c(SYSBP = "Systolic", DIABP = "Diastolic")

  rows <- list()
  for (pc in c("SYSBP", "DIABP")) {
    pc_data <- bp[bp$PARAMCD == pc, , drop = FALSE]
    if (nrow(pc_data) == 0) next
    visits_here <- if (length(sel_visits)) sel_visits else NA_character_
    for (vi in seq_along(visits_here)) {
      visit <- visits_here[vi]
      v_data <- if (has_avisit && !is.na(visit)) {
        pc_data[trimws(pc_data$AVISIT) == visit, , drop = FALSE]
      } else {
        pc_data
      }
      if (nrow(v_data) == 0) next
      vals <- vapply(positions, function(pos) {
        r <- v_data[v_data$position == pos, , drop = FALSE]
        if (nrow(r) == 0) NA_real_ else mean(r$AVAL, na.rm = TRUE)
      }, numeric(1L))
      keep <- !is.na(vals)
      if (!any(keep)) next
      rows[[length(rows) + 1L]] <- data.frame(
        position = factor(positions[keep], levels = positions),
        value = unname(vals[keep]),
        visit = if (is.na(visit)) "All visits" else visit,
        param = param_labels[[pc]],
        color = visit_colors[((vi - 1L) %% length(visit_colors)) + 1L],
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(NULL)
  df <- do.call(rbind, rows)
  df$series <- paste(df$param, df$visit)
  visit_cols <- stats::setNames(df$color, df$visit)
  visit_cols <- visit_cols[!duplicated(names(visit_cols))]

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = position, y = value, group = series,
                 color = visit, linetype = param)
  ) +
    ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = visit_cols) +
    ggplot2::scale_linetype_manual(
      values = c(Systolic = "solid", Diastolic = "dashed")
    ) +
    ggplot2::labs(x = NULL, y = "mmHg") +
    pp_static_theme()

  pp_static_sized(p, 340)
}

# ---------------------------------------------------------------------------
# Questionnaire heatmap twin
# ---------------------------------------------------------------------------

#' Static twin of the questionnaire heatmap render
#' @noRd
pp_static_heatmap <- function(dm_obj, time_range, settings = list(),
                              ref_ms = NA_real_, mode = "date") {
  pp_gg_require()
  domain <- settings$domain %||% "adqsadas"
  y_col <- settings$value %||% "AVAL"

  tbl <- pp_prepare_findings(dm_obj, domain)
  if (is.null(tbl) || !(y_col %in% colnames(tbl))) return(NULL)
  tbl <- tbl[!is.na(tbl[[y_col]]), , drop = FALSE]
  tbl <- tbl[!tbl$PARAMCD %in% c("NPTOT", "NPTOTMN"), , drop = FALSE]
  if (nrow(tbl) == 0) return(NULL)

  has_param <- "PARAM" %in% colnames(tbl)
  params <- sort(unique(tbl$PARAMCD))
  param_labels <- vapply(params, function(pc) {
    if (has_param) {
      lab <- as.character(tbl$PARAM[tbl$PARAMCD == pc][1])
      if (nchar(lab) > 30) lab <- paste0(substr(lab, 1, 27), "...")
      lab
    } else {
      pc
    }
  }, character(1L))

  if ("AVISITN" %in% colnames(tbl)) {
    visit_order <- unique(tbl[order(tbl$AVISITN), c("AVISIT", "AVISITN")])
    visits <- trimws(visit_order$AVISIT)
  } else {
    visits <- sort(unique(trimws(tbl$AVISIT)))
  }
  visits <- visits[nzchar(visits)]
  if (length(visits) == 0) return(NULL)

  cells <- list()
  for (vi in seq_along(visits)) {
    for (pi in seq_along(params)) {
      rows <- tbl[trimws(tbl$AVISIT) == visits[vi] &
                    tbl$PARAMCD == params[pi], , drop = FALSE]
      if (nrow(rows) == 0) next
      val <- mean(rows[[y_col]], na.rm = TRUE)
      if (is.na(val)) next
      cells[[length(cells) + 1L]] <- data.frame(
        visit = visits[vi], item = param_labels[[pi]], value = round(val, 2),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(cells)) return(NULL)
  df <- do.call(rbind, cells)
  df$visit <- factor(df$visit, levels = visits)
  # Top item first, matching the inverted interactive y axis.
  df$item <- factor(df$item, levels = rev(unname(param_labels)))

  fill_colors <- if (identical(y_col, "CHG")) {
    c("#059669", "#f9fafb", "#DC2626")
  } else {
    c("#dbeafe", "#ffffff", "#fecaca")
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = visit, y = item, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 1) +
    ggplot2::scale_fill_gradientn(colors = fill_colors) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = NULL, y = NULL, fill = y_col) +
    pp_static_theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(
        angle = if (length(visits) > 6) 30 else 0,
        hjust = if (length(visits) > 6) 0 else 0.5
      ),
      legend.title = ggplot2::element_text(
        color = PP_AXIS_LABEL_COLOR, size = 8
      )
    )

  pp_static_sized(p, 110 + length(params) * 28)
}

# ---------------------------------------------------------------------------
# Patient overview twin
# ---------------------------------------------------------------------------

#' Static twin of the patient overview render: the same lanes (treatment /
#' exposure, adverse events, visit ruler) with milestones on the treatment
#' lane.
#' @noRd
pp_static_overview <- function(dm_obj, time_range, settings = list(),
                               ref_ms = NA_real_, mode = "date") {
  pp_gg_require()
  tbls <- dm::dm_get_tables(dm_obj)
  adsl <- as.data.frame(tbls[["adsl"]])
  if (nrow(adsl) == 0) return(NULL)
  sl <- adsl[1, , drop = FALSE]
  if (is.na(sl$TRTSDT[1]) || is.na(sl$TRTEDT[1])) return(NULL)

  trt_start <- pp_xval(sl$TRTSDT[1], ref_ms, mode)
  trt_end <- pp_xval(sl$TRTEDT[1], ref_ms, mode)
  arm_col <- settings$roles$arm
  arm_label <- if (!is.null(arm_col) && arm_col %in% colnames(sl)) {
    trimws(gsub("[[:space:]]+", " ", as.character(sl[[arm_col]][1])))
  } else {
    "Treatment"
  }
  day_unit <- if (identical(mode, "rday")) 1 else 86400000

  # Lane availability, same gating as the interactive render.
  has_adae <- "adae" %in% names(tbls)
  ae_use_day <- FALSE
  adae_raw <- NULL
  if (has_adae) {
    adae_raw <- as.data.frame(tbls[["adae"]])
    ae_use_day <- identical(mode, "rday") && "ASTDY" %in% colnames(adae_raw)
    ae_src <- if (ae_use_day) "ASTDY" else "ASTDT"
    has_adae <- ae_src %in% colnames(adae_raw) &&
      nrow(adae_raw[!is.na(adae_raw[[ae_src]]), , drop = FALSE]) > 0
  }

  has_adex <- "adex" %in% names(tbls)
  ex_use_day <- FALSE
  ex_drugs <- character()
  adex_raw <- NULL
  if (has_adex) {
    adex_raw <- as.data.frame(tbls[["adex"]])
    ex_use_day <- identical(mode, "rday") && "ASTDY" %in% colnames(adex_raw)
    ex_src <- if (ex_use_day) "ASTDY" else "ASTDT"
    has_adex <- ex_src %in% colnames(adex_raw) &&
      nrow(adex_raw[!is.na(adex_raw[[ex_src]]), , drop = FALSE]) > 0
    if (has_adex && "EXTRT" %in% colnames(adex_raw)) {
      ex_drugs <- sort(unique(trimws(stats::na.omit(
        as.character(adex_raw$EXTRT)
      ))))
      ex_drugs <- ex_drugs[nzchar(ex_drugs)]
    }
  }

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

  lanes <- c("TRT", if (has_adae) "AE", if (has_vis) "VIS")
  lane_idx <- stats::setNames(seq_along(lanes) - 1L, lanes)
  lane_y <- function(nm) -lane_idx[[nm]]

  b <- pp_x_bounds(time_range, ref_ms, mode)
  clamp_lo <- function(x) if (is.na(b[1L])) x else pmax(x, b[1L])
  clamp_hi <- function(x) if (is.na(b[2L])) x else pmin(x, b[2L])

  # add_rect() collects from inside a closure, so the rect accumulator lives
  # in its own environment instead of reaching up the call stack with `<<-`.
  acc <- new.env(parent = emptyenv())
  acc$rects <- list()
  texts <- list()
  add_rect <- function(xmin, xmax, y, half_h, fill, color, lwd = 0.4) {
    acc$rects[[length(acc$rects) + 1L]] <- data.frame(
      xmin = clamp_lo(xmin), xmax = clamp_hi(xmax),
      ymin = y - half_h, ymax = y + half_h,
      fill = fill, color = color, lwd = lwd, stringsAsFactors = FALSE
    )
  }

  # Treatment envelope -- fallback only (no adex), carrying the arm label.
  if (!has_adex) {
    add_rect(trt_start, trt_end, lane_y("TRT"), 0.25,
             pp_alpha("#059669", 0.2), pp_alpha("#059669", 0.5))
    texts[[length(texts) + 1L]] <- data.frame(
      x = clamp_lo(trt_start), y = lane_y("TRT"), label = arm_label,
      color = "#059669", stringsAsFactors = FALSE
    )
  }

  # Exposure -- the treatment lane's real content when adex exists, one slot
  # per drug so a combination regimen's same-day infusions sit side by side.
  if (has_adex) {
    adex <- adex_raw[!is.na(adex_raw[[ex_src]]), , drop = FALSE]
    ex_has_end <- if (ex_use_day) "AENDY" %in% colnames(adex) else
      "AENDT" %in% colnames(adex)
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
        if (ex_use_day) v else NULL, ref_ms, mode
      )
    }
    drug_of <- function(i) trimws(opt_chr(adex, "EXTRT", i))
    key <- vapply(seq_len(nrow(adex)), function(i) {
      paste(drug_of(i), adex[[ex_src]][i],
            if (ex_has_end) ex_end(i) else "")
    }, character(1L))
    groups <- split(seq_len(nrow(adex)), factor(key, levels = unique(key)))
    n_slot <- max(length(ex_drugs), 1L)
    slot_of <- function(drug) {
      if (!length(ex_drugs) || !nzchar(drug)) return(0L)
      match(drug, ex_drugs) - 1L
    }
    band <- 0.9
    slot_h <- band / n_slot
    half_h <- if (n_slot == 1L) 0.225 else max(slot_h * 0.4, 0.04)
    for (rows in groups) {
      i <- rows[[1L]]
      x0 <- ex_x(adex[[ex_src]][i])
      x1 <- if (ex_has_end && !is.na(ex_end(i))) {
        ex_x(ex_end(i))
      } else {
        pp_gantt_open_end(x0, time_range, ref_ms, mode, day_unit)
      }
      k <- slot_of(drug_of(i))
      yc <- if (n_slot == 1L) {
        lane_y("TRT")
      } else {
        lane_y("TRT") + band / 2 - slot_h * (k + 0.5)
      }
      add_rect(x0, x1, yc, half_h,
               pp_alpha("#2563EB", 0.25), pp_alpha("#2563EB", 0.55))
    }
  }

  # Adverse events lane.
  if (has_adae) {
    adae <- adae_raw[!is.na(adae_raw[[ae_src]]), , drop = FALSE]
    ae_has_end <- if (ae_use_day) "AENDY" %in% colnames(adae) else
      "AENDT" %in% colnames(adae)
    sev_col <- settings$roles$severity
    has_sev <- !is.null(sev_col) && sev_col %in% colnames(adae)
    has_ser <- "AESER" %in% colnames(adae)

    sev_hex <- pp_sev_colors
    fixed <- settings$sev_colors
    if (!is.null(fixed)) {
      vals <- unname(unlist(fixed))
      names(vals) <- toupper(names(fixed))
      sev_hex[names(vals)] <- vals
    }
    ae_end <- function(i) if (ae_use_day) adae$AENDY[i] else adae$AENDT[i]
    ae_x <- function(v) {
      pp_xval_pref_day(
        if (ae_use_day) NULL else v,
        if (ae_use_day) v else NULL, ref_ms, mode
      )
    }
    for (i in seq_len(nrow(adae))) {
      s <- ae_x(adae[[ae_src]][i])
      e <- if (ae_has_end && !is.na(ae_end(i))) {
        ae_x(ae_end(i))
      } else {
        pp_gantt_open_end(s, time_range, ref_ms, mode, day_unit)
      }
      sev <- if (has_sev) toupper(as.character(adae[[sev_col]][i])) else ""
      hex <- if (nzchar(sev) && sev %in% names(sev_hex)) {
        sev_hex[[sev]]
      } else {
        "#9ca3af"
      }
      add_rect(s, e, lane_y("AE"), 0.225,
               pp_alpha(hex, 0.7), pp_alpha(hex, 0.9))
      if (has_ser && identical(toupper(as.character(adae$AESER[i])), "Y")) {
        # The serious strip: a thin red bar along the top edge, same mark as
        # the interactive lane.
        add_rect(s, e, lane_y("AE") + 0.2, 0.025, "#DC2626", NA, 0)
      }
    }
  }

  rect_df <- do.call(rbind, acc$rects)

  # Points are clipped to the window explicitly, the way echarts clips them
  # silently: a milestone past the axis (RFENDT is not among the columns
  # the time range scans) must drop without a ggplot warning.
  in_window <- function(x) {
    (is.na(b[1L]) | x >= b[1L]) & (is.na(b[2L]) | x <= b[2L])
  }

  # Milestones (end of study, death) on the treatment lane.
  ms <- list()
  if ("RFENDT" %in% colnames(sl) && !is.na(sl$RFENDT[1])) {
    ms[[length(ms) + 1L]] <- data.frame(
      x = pp_xval(sl$RFENDT[1], ref_ms, mode), y = lane_y("TRT"),
      kind = "eos", stringsAsFactors = FALSE
    )
  }
  if ("DTHDT" %in% colnames(sl) && !is.na(sl$DTHDT[1])) {
    ms[[length(ms) + 1L]] <- data.frame(
      x = pp_xval(sl$DTHDT[1], ref_ms, mode), y = lane_y("TRT"),
      kind = "death", stringsAsFactors = FALSE
    )
  } else if ("DTHFL" %in% colnames(sl) &&
               !is.na(sl$DTHFL[1]) && sl$DTHFL[1] == "Y") {
    ms[[length(ms) + 1L]] <- data.frame(
      x = trt_end, y = lane_y("TRT"), kind = "death",
      stringsAsFactors = FALSE
    )
  }
  ms_df <- if (length(ms)) do.call(rbind, ms) else NULL
  if (!is.null(ms_df)) {
    ms_df <- ms_df[in_window(ms_df$x), , drop = FALSE]
  }

  vis_df <- NULL
  if (has_vis) {
    vx <- vapply(seq_len(nrow(visits)), function(i) {
      pp_xval_pref_day(
        if (is.na(visits$date[i])) NULL else visits$date[i],
        if (is.na(visits$day[i])) NULL else visits$day[i],
        ref_ms, mode
      )
    }, numeric(1L))
    vis_df <- data.frame(x = vx, y = lane_y("VIS"))
    vis_df <- vis_df[in_window(vis_df$x), , drop = FALSE]
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = rect_df,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                   fill = I(fill), color = I(color), linewidth = I(lwd))
    ) +
    pp_static_x_scale(time_range, ref_ms, mode) +
    ggplot2::scale_y_continuous(
      limits = c(-(length(lanes) - 1L) - 0.55, 0.55),
      breaks = -unname(lane_idx), labels = names(lane_idx),
      expand = ggplot2::expansion()
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    pp_static_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "none",
      axis.text.y = ggplot2::element_text(face = "bold", size = 8)
    )

  if (length(texts)) {
    txt_df <- do.call(rbind, texts)
    p <- p + ggplot2::geom_text(
      data = txt_df,
      ggplot2::aes(x = x, y = y, label = label, color = I(color)),
      hjust = -0.05, size = 2.8, fontface = "bold"
    )
  }
  if (!is.null(vis_df) && nrow(vis_df)) {
    p <- p + ggplot2::geom_segment(
      data = vis_df,
      ggplot2::aes(x = x, xend = x, y = y - 0.2, yend = y + 0.2),
      color = "#9ca3af", linewidth = 0.7
    )
  }
  if (!is.null(ms_df) && nrow(ms_df)) {
    eos <- ms_df[ms_df$kind == "eos", , drop = FALSE]
    death <- ms_df[ms_df$kind == "death", , drop = FALSE]
    if (nrow(eos)) {
      p <- p + ggplot2::geom_point(
        data = eos, ggplot2::aes(x = x, y = y),
        shape = 23, size = 2.6, fill = "#2563EB", color = "white",
        stroke = 0.6
      )
    }
    if (nrow(death)) {
      p <- p + ggplot2::geom_point(
        data = death, ggplot2::aes(x = x, y = y),
        shape = 4, size = 2.6, color = "#DC2626", stroke = 1.2
      )
    }
  }

  lane_px <- 40 + min(max(0L, length(ex_drugs) - 2L), 3L) * 10
  pp_static_sized(p, 50 + length(lanes) * lane_px + 60)
}
