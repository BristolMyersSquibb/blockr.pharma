# Patient Profile Viz: Tumour Response Lane
#
# One card per repeatedly-assessed adrs parameter (pp_resp_lane_params()),
# drawn as a single lane of coloured segments on the profile's shared time
# axis -- the per-patient reading of the cohort's response swimmer, where the
# swimmer's one lane per patient becomes this patient's one lane.
#
# Data requirements (declared via new_pp_viz()):
#   adrs: required — PARAMCD, AVALC, and a time source: ADT or ADY
#         optional — PARAM, AVISIT, ADY
#
# Colour is the whole reading, so it comes from the board (pp-response.R) and
# is stated again in a header legend built from the same resolved vector, the
# way the CM gantt states its indications. Nothing here knows a category or a
# hex.
#
# Bars are NOT labelled with their category. The shared renderItem draws a
# label at the bar's START and truncates it at the plot's right edge, which is
# why the other gantts write one label per LANE rather than one per bar: on a
# lane of eight assessments, per-bar labels would each run under their
# neighbours. The legend says the vocabulary once, and the tooltip says which
# category this bar is.

#' Response lane definitions for a dm
#'
#' One per parameter the study assesses more than once; none at all for a
#' study whose response table is subject-level throughout (its best overall
#' response is still reported, in the patient info table). Built from the
#' COHORT dm, like the findings cards: which cards exist is a property of the
#' study, not of the patient on screen.
#'
#' @param dm_obj A normalized `dm` object.
#' @param table The response table.
#' @return Named list of `pp_viz` definitions, possibly empty.
#' @noRd
pp_response_vizs <- function(dm_obj, table = "adrs") {
  tbls <- tryCatch(dm::dm_get_tables(dm_obj), error = function(e) NULL)
  if (is.null(tbls) || !table %in% names(tbls)) return(list())
  tbl <- as.data.frame(tbls[[table]])

  codes <- pp_resp_lane_params(tbl)
  if (!length(codes)) return(list())

  has_param <- "PARAM" %in% colnames(tbl)
  vizs <- lapply(codes, function(code) {
    rows <- tbl[as.character(tbl$PARAMCD) %in% code, , drop = FALSE]
    param <- if (has_param) {
      p <- trimws(as.character(rows$PARAM[[1L]]))
      if (is.na(p) || !nzchar(p)) code else p
    } else {
      code
    }
    pp_response_viz(code, param, table)
  })
  stats::setNames(vizs, vapply(vizs, `[[`, character(1L), "id"))
}

#' One response lane definition
#'
#' The id is `response_<PARAMCD>`. It cannot collide with a findings card,
#' whose ids are always `<group>__<PARAMCD>`, and it is a pure function of the
#' study's own code, so it survives a board round trip.
#'
#' @param code The PARAMCD.
#' @param param Its display name.
#' @param table The response table.
#' @return A `pp_viz`.
#' @noRd
pp_response_viz <- function(code, param, table = "adrs") {
  new_pp_viz(
    id = paste0("response_", code),
    # The house form the parameter cards use: the code, the full name muted
    # behind it.
    label = code,
    sublabel = param,
    domain = "Efficacy",
    icon = "clipboard-pulse",
    color = "#DB2777",
    description = paste0(param, ", assessment by assessment"),
    search = paste(code, param, "response"),
    params = stats::setNames(param, code),
    tables = table,
    requires = stats::setNames(list(c("PARAMCD", "AVALC")), table),
    requires_any = stats::setNames(list(list(c("ADT", "ADY"))), table),
    optional = stats::setNames(list(c("PARAM", "ADY", "AVISIT")), table),
    # The board scale map's colours for the response categories, resolved
    # once by the block and read here and by the legend (see
    # pp_viz_exhibit_settings()).
    uses = "response",
    # No cohort strip. A strip band is either intervals read off two COLUMNS
    # or one value series, and a response lane is neither: its intervals are
    # derived from the gaps BETWEEN records, which pp_band_spans() has no way
    # to declare. Declaring nothing means the strip is drawn by the next
    # selected card that can, which is the designed behaviour rather than a
    # gap (see pp_cohort_band_source()).
    band = NULL,
    legend_ui = function(dm_obj, settings) {
      pp_resp_legend_ui(settings$resp_colors)
    },
    exhibit = local({
      .code <- code
      .table <- table
      function(dm_obj, time_range, settings = list(),
               ref_ms = NA_real_, mode = "date") {
        pp_static_response_lane(dm_obj, time_range, settings, ref_ms, mode,
                                paramcd = .code, table = .table)
      }
    }),
    render = local({
      .code <- code
      .param <- param
      .table <- table
      function(dm_obj, time_range, settings = list(),
               ref_ms = NA_real_, mode = "date") {
        pp_render_response_lane(dm_obj, time_range, settings, ref_ms, mode,
                                paramcd = .code, param = .param,
                                table = .table)
      }
    })
  )
}

#' The response legend for the panel header
#'
#' Same component and same argument as the CM panel's indication legend: one
#' swatch per category, in the resolved order, from the very colours the bars
#' use.
#' @param resp_colors Resolved category -> colour vector, or `NULL`.
#' @return A `shiny::div`, or `NULL`.
#' @noRd
pp_resp_legend_ui <- function(resp_colors = NULL) {
  if (is.null(resp_colors) || !length(resp_colors)) return(NULL)
  items <- lapply(names(resp_colors), function(lv) {
    shiny::span(
      class = "pp-legend-item",
      shiny::span(
        class = "pp-legend-swatch",
        style = paste0("background:", unname(resp_colors[[lv]]), ";")
      ),
      lv
    )
  })
  shiny::div(class = "pp-chart-legend", items)
}

#' Render one response lane
#' @noRd
pp_render_response_lane <- function(dm_obj, time_range, settings = list(),
                                    ref_ms = NA_real_, mode = "date",
                                    paramcd = NULL, param = NULL,
                                    table = "adrs") {
  tbls <- dm::dm_get_tables(dm_obj)
  if (!table %in% names(tbls)) return(pp_empty_chart("No response records"))
  tbl <- as.data.frame(tbls[[table]])
  if (!is.null(paramcd)) {
    tbl <- tbl[as.character(tbl$PARAMCD) %in% paramcd, , drop = FALSE]
  }
  if (nrow(tbl) == 0L) return(pp_empty_chart("No response records"))

  # Date mode has no day-based fallback, exactly as on the medication gantt.
  if (!identical(mode, "rday") && !"ADT" %in% colnames(tbl)) {
    return(pp_empty_chart(paste0(
      "Calendar dates unavailable for response assessments; ",
      "switch the timeline to relative day"
    )))
  }

  seg <- pp_resp_segments(tbl, time_range, ref_ms, mode)
  if (nrow(seg) == 0L) return(pp_empty_chart("No response records"))

  keep <- pp_gantt_in_window(seg$start, seg$end, time_range, ref_ms, mode)
  seg <- seg[keep, , drop = FALSE]
  if (nrow(seg) == 0L) {
    return(pp_empty_chart("No response assessments in this time range"))
  }

  # The board's colours where they resolve, this package's RECIST constants
  # otherwise. Resolved per level rather than looked up wholesale, so a
  # category the map does not bind still gets its conventional colour instead
  # of dropping to grey.
  resp_colors <- settings$resp_colors
  bar_color <- function(lv) {
    if (!is.null(resp_colors) && lv %in% names(resp_colors)) {
      unname(resp_colors[[lv]])
    } else {
      pp_resp_fallback_color(lv)
    }
  }

  lane_label <- pp_term_label(param %||% paramcd %||% "Response")

  bar_data <- lapply(seq_len(nrow(seg)), function(i) {
    col <- bar_color(seg$resp[i])
    list(
      value = list(
        seg$start[i], seg$end[i], 0L,
        seg$resp[i], seg$s_lab[i], seg$e_lab[i],
        # No on-bar label. The other gantts write one per LANE because they
        # have many and the axis labels are hidden; this card has exactly one
        # lane and its header already reads "OVR  Overall Response by
        # Investigator", so the same label under it was the sentence twice.
        # The category is in the legend and in the tooltip.
        "",
        seg$ongoing[i], col
      ),
      itemStyle = list(color = col)
    )
  })

  series_list <- list(list(
    type = "custom",
    name = "Response",
    renderItem = pp_gantt_render_item(6, ongoing_idx = 7),
    encode = list(x = list(0, 1), y = 2),
    data = bar_data,
    tooltip = list(
      formatter = htmlwidgets::JS(sprintf("
        function(params) {
          var v = params.value;
          var resp = v[3] || '';
          var s = v[4] || '';
          var e = v[5] || '';
          var ongoing = !!v[7];
          var badge = v[8] || '';
          var html = '<div style=\"min-width:170px\">';
          html += '<div style=\"font-size:14px;font-weight:700;' +
            'margin-bottom:2px\">' + %s + '</div>';
          html += '<div style=\"font-size:11px;color:#888;' +
            'margin-bottom:4px\">' + %s + '</div>';
          if (resp) {
            html += '<span style=\"display:inline-block;background:' +
              badge + ';color:#fff;padding:1px 8px;border-radius:3px;' +
              'font-size:11px;font-weight:600;margin-bottom:4px\">' +
              resp + '</span><br/>';
          }
          html += '<span style=\"font-size:12px\">Assessed: ' + s +
            '</span><br/>';
          html += '<span style=\"font-size:12px\">' +
            (ongoing ? 'Held until: last assessment' : 'Held until: ' + e) +
            '</span>';
          html += '</div>';
          return html;
        }
      ", pp_js_str(paramcd %||% "Response"), pp_js_str(param %||% "")))
    )
  ))

  echarts4r::e_charts(height = pp_gantt_height(1L)) |>
    echarts4r::e_list(list(
      backgroundColor = "transparent",
      tooltip = pp_tooltip(),
      grid = list(
        left = PP_GRID_LEFT, right = 20,
        top = PP_GANTT_TOP, bottom = PP_GANTT_BOTTOM,
        borderColor = "transparent"
      ),
      xAxis = pp_time_axis(time_range, ref_ms, mode),
      yAxis = list(
        type = "category",
        data = list(lane_label),
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
