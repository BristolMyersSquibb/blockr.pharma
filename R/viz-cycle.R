# Patient Profile Viz: Treatment Cycles
#
# A ruler, not a chart. Every viz builds its axis from the same `time_range`,
# `ref_ms` and `mode`, so the lanes are x-aligned by construction and a band
# drawn here lines up with the AE bars and lab points above it for free. That
# is the whole design: this viz reaches into nothing and nothing reaches into
# it, so it can be deleted in one file if clinicians do not want it.
#
# Bands rather than tick lines, because the question underneath "can I see the
# cycle?" is usually about DELAYS -- a cycle held for toxicity is a visibly
# wider band next to a run of even ones, which reads without anyone computing
# a difference. Alternating fill separates neighbours; a dashed border marks
# a cycle whose D1 visit was missing and whose start is therefore inferred
# (see pp-cycle.R).
#
# Cycle NUMBER lives here. Cycle DAY cannot: it is a property of a point, not
# an interval, so it rides in each viz's own tooltip via pp_with_cycle().
#
# GENERATED, NOT REGISTERED. Every other static viz can be judged from the
# schema -- is the table there, are the columns there -- which is exactly what
# pp_coverage_report() asks and what fills the gear's Data coverage list. This
# one cannot: adlb, AVISIT and ADT are all present on a study that simply is
# not dosed in cycles. Registering it statically therefore parks a permanently
# empty card on every such study, because the render is the first code that
# can tell, and by then a message in the panel is all it can do.
#
# So it is generated like the findings vizs (pp_findings_vizs()), which have
# the same shape of question -- exists only if the data carries the values.
# Deliberately absent from Data coverage as well: "needs table adqsadas"
# describes a gap someone could close, while "this study is not dosed in
# cycles" is not a gap at all and would be noise on every non-oncology study.

#' The cycle lane, when the study speaks cycles
#'
#' Availability is COHORT-based (the sidebar must not change as you page
#' through patients), so this asks only whether the study uses the vocabulary
#' at all -- never whether the patient on screen has cycles. It reads the
#' DISTINCT visit labels rather than the column: a lab table runs to hundreds
#' of thousands of rows and carries a few dozen visit names.
#'
#' @param dm_obj A normalized `dm` (unscoped).
#' @return A named list holding the cycle viz, or an empty list.
#' @noRd
pp_cycle_vizs <- function(dm_obj) {
  none <- list()
  if (!inherits(dm_obj, "dm")) return(none)
  tbls <- dm::dm_get_tables(dm_obj)
  if (!"adlb" %in% names(tbls)) return(none)
  df <- as.data.frame(tbls[["adlb"]])
  if (!all(c("AVISIT", "ADT") %in% colnames(df))) return(none)
  visits <- unique(as.character(df$AVISIT))
  if (!any(!is.na(pp_parse_cycle_visits(visits)$cycle))) return(none)
  stats::setNames(list(cycle_viz), cycle_viz$id)
}

#' Treatment cycles visualization definition
#'
#' Not in [patient_profile_static_vizs()] on purpose -- reached through
#' [pp_cycle_vizs()]. See the note at the top of this file.
#' @noRd
cycle_viz <- new_pp_viz(
  id = "cycle_lane",
  label = "Treatment Cycles",
  domain = "Treatment",
  icon = "arrow-repeat",
  color = "#7C3AED",
  description = "Cycle boundaries read from the visit schedule",
  tables = "adlb",
  requires = list(adlb = c("AVISIT", "ADT")),
  uses = "cycle",
  render = function(dm_obj, time_range, settings = list(),
                    ref_ms = NA_real_, mode = "date") {
    anchors <- settings$cycle_anchors
    if (is.null(anchors) || !nrow(anchors)) {
      return(pp_empty_chart(
        "No CYCLE n DAY m visits in the lab data for this patient"
      ))
    }
    if (identical(mode, "rday") && is.na(ref_ms)) {
      return(pp_empty_chart("Treatment start unavailable"))
    }

    band_fill <- c("rgba(124,58,237,0.10)", "rgba(124,58,237,0.20)")

    band_data <- lapply(seq_len(nrow(anchors)), function(i) {
      # cycle_end is the day BEFORE the next cycle starts, so the band runs to
      # the following day to make neighbours abut rather than leave a seam.
      x0 <- pp_xval(anchors$cycle_start[i], ref_ms, mode)
      x1 <- pp_xval(anchors$cycle_end[i] + 1L, ref_ms, mode)
      est <- isTRUE(anchors$estimated[i])
      len <- as.integer(anchors$cycle_end[i] - anchors$cycle_start[i]) + 1L
      start_lab <- pp_xlabel(anchors$cycle_start[i], ref_ms, mode)
      list(
        value = list(x0, x1, paste0("C", anchors$cycle[i]),
                     start_lab, len, if (est) 1L else 0L),
        itemStyle = list(
          color = band_fill[[(anchors$cycle[i] %% 2L) + 1L]],
          borderColor = "rgba(124,58,237,0.55)",
          borderWidth = 1,
          borderType = if (est) "dashed" else "solid"
        )
      )
    })

    series_list <- list(list(
      type = "custom",
      name = "Treatment Cycles",
      renderItem = htmlwidgets::JS("
        function(params, api) {
          var start = api.coord([api.value(0), 0]);
          var end   = api.coord([api.value(1), 0]);
          var cs    = params.coordSys;
          var rect  = echarts.graphic.clipRectByRect(
            { x: start[0], y: cs.y + 8, width: end[0] - start[0],
              height: cs.height - 16 },
            { x: cs.x, y: cs.y, width: cs.width, height: cs.height }
          );
          if (!rect) return;

          var children = [{
            type: 'rect',
            shape: Object.assign({}, rect, { r: 2 }),
            style: api.style()
          }];

          // Drop the label rather than let it spill out of a narrow band --
          // a clipped cycle is still readable from its neighbours.
          var label = api.value(2);
          if (label && rect.width > 22) {
            children.push({
              type: 'text',
              style: {
                text: label,
                x: rect.x + rect.width / 2,
                y: rect.y + rect.height / 2,
                textAlign: 'center',
                textVerticalAlign: 'middle',
                fill: '#5B21B6',
                fontSize: 11,
                fontWeight: 600,
                fontFamily: 'system-ui, -apple-system, sans-serif'
              }
            });
          }
          return { type: 'group', children: children };
        }
      "),
      encode = list(x = list(0, 1), y = 2),
      data = band_data,
      tooltip = list(
        formatter = htmlwidgets::JS("
          function(params) {
            var v = params.value;
            var cyc = v[2] || '';
            var start = v[3] || '';
            var len = v[4];
            var est = v[5];
            var html = '<div style=\"min-width:170px\">';
            html += '<div style=\"font-size:14px;font-weight:700;' +
              'margin-bottom:4px\">Cycle ' + cyc.replace('C', '') + '</div>';
            html += '<span style=\"font-size:12px\">Starts ' + start +
              '</span><br/>';
            html += '<span style=\"font-size:12px\">Length: ' + len +
              ' days</span>';
            if (est) {
              html += '<br/><span style=\"color:#888;font-size:11px\">' +
                'Day 1 visit missing \\u2014 start inferred</span>';
            }
            html += '</div>';
            return html;
          }
        ")
      )
    ))

    echarts4r::e_charts(height = 110) |>
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
          data = list("cycles"),
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
