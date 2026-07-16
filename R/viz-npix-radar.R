# Patient Profile Viz: NPI-X Radar
#
# Radar/spider chart of NPI-X domain scores, one polygon per visit.
# Controls: visits (checkbox of available visits).
#
# Data requirements (declared via new_pp_viz()):
#   adqsnpix: required PARAMCD, AVAL, AVISIT

#' NPI-X Radar visualization definition
#' @noRd
npix_radar_viz <- new_pp_viz(
  id = "npix_radar",
  label = "NPI-X Radar",
  domain = "Questionnaires",
  icon = "clipboard-pulse",
  color = "#E11D48",
  description = "Radar chart of NPI-X domain scores by visit",
  tables = "adqsnpix",
  requires = list(adqsnpix = c("PARAMCD", "AVAL", "AVISIT")),
  optional = list(adqsnpix = "AVISITN"),
  controls = list(
    visits = list(
      type = "checkbox",
      label = "Visits",
      default = NULL,
      choices_from = "AVISIT"
    )
  ),
  render = function(dm_obj, time_range, settings = list(), ...) {
    tbls <- dm::dm_get_tables(dm_obj)
    tbl <- as.data.frame(tbls[["adqsnpix"]])

    tbl <- tbl[!is.na(tbl$AVAL), , drop = FALSE]
    if (nrow(tbl) == 0) return(pp_empty_chart("No NPI-X records"))

      # Domain items only (12 individual items, not totals)
      item_codes <- paste0("NPITM", sprintf("%02d", 1:12), "S")
      tbl <- tbl[tbl$PARAMCD %in% item_codes, , drop = FALSE]
      if (nrow(tbl) == 0) return(pp_empty_chart("No NPI-X item data"))

      # Short domain labels
      domain_labels <- c(
        NPITM01S = "Delusions", NPITM02S = "Hallucinations",
        NPITM03S = "Agitation", NPITM04S = "Depression",
        NPITM05S = "Anxiety", NPITM06S = "Euphoria",
        NPITM07S = "Apathy", NPITM08S = "Disinhibition",
        NPITM09S = "Irritability", NPITM10S = "Aberrant Motor",
        NPITM11S = "Sleep", NPITM12S = "Appetite"
      )

      all_visits <- pp_visit_levels(tbl)

      sel_visits <- settings$visits
      if (is.null(sel_visits) || length(sel_visits) == 0) {
        sel_visits <- all_visits
      }
      sel_visits <- intersect(sel_visits, all_visits)
      if (length(sel_visits) == 0) {
        return(pp_empty_chart("No data for selected visits"))
      }

      visit_colors <- c(
        "#2563EB", "#059669", "#D97706", "#DC2626",
        "#7C3AED", "#0891B2", "#EA580C", "#374151",
        "#E11D48", "#14B8A6", "#6366F1", "#CA8A04",
        "#9333EA", "#0D9488", "#BE123C"
      )

      # Radar indicators (axes)
      present_codes <- intersect(item_codes, unique(tbl$PARAMCD))
      max_val <- max(tbl$AVAL, na.rm = TRUE)
      indicators <- lapply(present_codes, function(pc) {
        list(
          name = domain_labels[[pc]] %||% pc,
          max = ceiling(max_val * 1.1)
        )
      })

      # Build series per visit
      radar_series <- lapply(seq_along(sel_visits), function(vi) {
        visit <- sel_visits[vi]
        v_data <- tbl[trimws(tbl$AVISIT) == visit, , drop = FALSE]
        # One value per domain
        values <- vapply(present_codes, function(pc) {
          rows <- v_data[v_data$PARAMCD == pc, , drop = FALSE]
          if (nrow(rows) == 0) return(NA_real_)
          mean(rows$AVAL, na.rm = TRUE)
        }, numeric(1))
        values[is.na(values)] <- 0

        color <- visit_colors[((vi - 1L) %% length(visit_colors)) + 1L]

        list(
          value = as.list(values),
          name = visit,
          lineStyle = list(color = color, width = 2),
          itemStyle = list(color = color),
          areaStyle = list(color = color, opacity = 0.08),
          symbol = "circle",
          symbolSize = 5
        )
      })

      echarts4r::e_charts(height = 400) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          tooltip = pp_tooltip(),
          toolbox = pp_toolbox(),
          legend = list(
            show = TRUE,
            bottom = 0, left = "center",
            textStyle = list(fontSize = 11, color = PP_AXIS_LABEL_COLOR),
            itemWidth = 14, itemHeight = 10
          ),
          radar = list(
            indicator = indicators,
            shape = "polygon",
            splitNumber = 4,
            center = list("50%", "48%"),
            radius = "65%",
            name = list(
              textStyle = list(
                fontSize = 11, color = PP_AXIS_LABEL_COLOR, fontWeight = 400
              )
            ),
            axisLine = list(lineStyle = list(color = PP_AXIS_LINE_COLOR)),
            splitLine = list(lineStyle = list(color = PP_SPLIT_LINE_COLOR)),
            splitArea = list(
              areaStyle = list(
                color = list("rgba(249,250,251,0.5)", "rgba(255,255,255,0.5)")
              )
            )
          ),
          series = list(list(
            type = "radar",
            data = radar_series,
            tooltip = list(
              formatter = htmlwidgets::JS("
                function(params) {
                  var d = params.data;
                  var html = '<div style=\"min-width:140px\">';
                  html += '<div style=\"font-size:13px;font-weight:600;' +
                    'margin-bottom:4px\">' + (d.name || '') + '</div>';
                  var vals = d.value || [];
                  var inds = params.radar && params.radar.indicator ?
                    params.radar.indicator : [];
                  for (var i = 0; i < vals.length; i++) {
                    var label = (inds[i] && inds[i].name) || ('Item ' + (i+1));
                    html += '<div style=\"font-size:11px;line-height:1.5\">' +
                      '<span style=\"color:#6b7280\">' + label +
                      ':</span> ' + (vals[i] != null ? vals[i].toFixed(1) : '-') +
                      '</div>';
                  }
                  html += '</div>';
                  return html;
                }
              ")
            )
          ))
        )) |>
        echarts4r::e_text_style(fontFamily = "system-ui, -apple-system, sans-serif")
    }
)
