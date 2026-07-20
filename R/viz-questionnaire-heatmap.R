# Patient Profile Viz: Questionnaire Heatmap
#
# Heatmap grid of scores (AVAL or CHG) across items and visits.
# Controls: domain (radio), value (radio).
#
# Data requirements (declared via new_pp_viz()):
#   adqsadas, adqsnpix: required PARAMCD, AVISIT, AVAL; optional CHG, PARAM,
#                       AVISITN. Both tables must be present.

#' Questionnaire Heatmap visualization definition
#' @noRd
questionnaire_heatmap_viz <- new_pp_viz(
  id = "questionnaire_heatmap",
  label = "Questionnaire Heatmap",
  domain = "Questionnaires",
  icon = "grid-3x3",
  color = "#6366F1",
  description = "Heatmap of questionnaire scores across items and visits",
  tables = c("adqsadas", "adqsnpix"),
  requires = list(
    adqsadas = c("PARAMCD", "AVISIT", "AVAL"),
    adqsnpix = c("PARAMCD", "AVISIT", "AVAL")
  ),
  optional = list(
    adqsadas = c("CHG", "PARAM", "AVISITN"),
    adqsnpix = c("CHG", "PARAM", "AVISITN")
  ),
  controls = list(
    domain = list(
      type = "radio",
      label = "Domain",
      default = "adqsadas",
      choices = c("ADAS-Cog" = "adqsadas", "NPI-X" = "adqsnpix")
    ),
    value = list(
      type = "radio",
      label = "Value",
      default = "AVAL",
      choices = c("Absolute" = "AVAL", "Change" = "CHG")
    )
  ),
  render = function(dm_obj, time_range, settings = list(), ...) {
    tbls <- dm::dm_get_tables(dm_obj)

    domain <- settings$domain %||% "adqsadas"
    y_col <- settings$value %||% "AVAL"

    tbl <- pp_prepare_findings(dm_obj, domain)
    if (is.null(tbl)) return(pp_empty_chart("No records"))
    if (!(y_col %in% colnames(tbl))) {
      return(pp_empty_chart(paste("No", y_col, "values in", domain)))
    }
    tbl <- tbl[!is.na(tbl[[y_col]]), , drop = FALSE]
    if (nrow(tbl) == 0) return(pp_empty_chart("No records"))

      # Item labels
      has_param <- "PARAM" %in% colnames(tbl)

      # Exclude total/summary codes
      exclude_codes <- c("NPTOT", "NPTOTMN")
      tbl <- tbl[!tbl$PARAMCD %in% exclude_codes, , drop = FALSE]

      params <- sort(unique(tbl$PARAMCD))
      if (length(params) == 0) return(pp_empty_chart("No item data"))

      # Build param labels
      param_labels <- vapply(params, function(pc) {
        if (has_param) {
          lab <- as.character(tbl$PARAM[tbl$PARAMCD == pc][1])
          if (nchar(lab) > 30) lab <- paste0(substr(lab, 1, 27), "...")
          lab
        } else {
          pc
        }
      }, character(1))

      # Visits: sort by AVISITN if available
      if ("AVISITN" %in% colnames(tbl)) {
        visit_order <- unique(tbl[order(tbl$AVISITN), c("AVISIT", "AVISITN")])
        visits <- trimws(visit_order$AVISIT)
        visits <- visits[nzchar(visits)]
      } else {
        visits <- sort(unique(trimws(tbl$AVISIT)))
        visits <- visits[nzchar(visits)]
      }
      if (length(visits) == 0) return(pp_empty_chart("No visits found"))

      # Heatmap data: [visit_idx, param_idx, value]
      heat_data <- list()
      for (vi in seq_along(visits)) {
        for (pi in seq_along(params)) {
          rows <- tbl[trimws(tbl$AVISIT) == visits[vi] &
            tbl$PARAMCD == params[pi], , drop = FALSE]
          val <- if (nrow(rows) > 0) mean(rows[[y_col]], na.rm = TRUE) else NA
          if (!is.na(val)) {
            heat_data <- c(heat_data, list(list(vi - 1L, pi - 1L, round(val, 2))))
          }
        }
      }

      if (length(heat_data) == 0) {
        return(pp_empty_chart("No heatmap data"))
      }

      all_vals <- vapply(heat_data, function(x) x[[3]], numeric(1))
      min_val <- min(all_vals, na.rm = TRUE)
      max_val <- max(all_vals, na.rm = TRUE)

      # Color scale
      if (y_col == "CHG") {
        visual_map <- list(
          min = min_val, max = max_val,
          calculable = TRUE,
          orient = "horizontal",
          left = "center", bottom = 0,
          itemWidth = 10, itemHeight = 120,
          textStyle = list(fontSize = 10, color = "#6b7280"),
          inRange = list(color = list("#059669", "#f9fafb", "#DC2626"))
        )
      } else {
        visual_map <- list(
          min = min_val, max = max_val,
          calculable = TRUE,
          orient = "horizontal",
          left = "center", bottom = 0,
          itemWidth = 10, itemHeight = 120,
          textStyle = list(fontSize = 10, color = "#6b7280"),
          inRange = list(color = list("#dbeafe", "#ffffff", "#fecaca"))
        )
      }

      # Tooltip needs access to axis data: embed visit and param labels as JS
      # arrays. Both are study data (visit names, questionnaire item labels),
      # so they are encoded, not pasted -- a quote or a line break in either
      # would break the literal and take the whole widget down with it.
      visits_js <- pp_js_arr(visits)
      params_js <- pp_js_arr(param_labels)

      # Chrome plus a fixed 28px row, same rule as the gantt lanes: a minimum
      # height would stretch a short questionnaire's rows apart instead of
      # just drawing a short chart.
      chart_height <- PP_PLOT_TOP + 50 + max(length(params), 1L) * 28

      echarts4r::e_charts(height = chart_height) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          toolbox = pp_toolbox(),
          tooltip = list(
            trigger = "item",
            confine = TRUE,
            backgroundColor = "rgba(255,255,255,0.98)",
            borderColor = "#d1d5db",
            borderWidth = 1,
            textStyle = list(color = "#1f2937", fontSize = 12),
            extraCssText = paste0(
              "box-shadow: 0 4px 12px rgba(0,0,0,0.08);",
              "border-radius: 6px; padding: 8px 12px;"
            ),
            formatter = htmlwidgets::JS(sprintf("
              function(params) {
                var visits = %s;
                var items = %s;
                var v = params.value;
                return '<div style=\"min-width:140px\">' +
                  '<div style=\"font-size:12px;font-weight:600;margin-bottom:2px\">' +
                  (items[v[1]] || '') + '</div>' +
                  '<div style=\"font-size:11px;color:#6b7280;margin-bottom:2px\">' +
                  (visits[v[0]] || '') + '</div>' +
                  '<div style=\"font-size:13px;font-weight:700\">' +
                  v[2] + '</div></div>';
              }
            ", visits_js, params_js))
          ),
          grid = list(
            left = 140, right = 20, top = PP_PLOT_TOP, bottom = 50,
            borderColor = "transparent"
          ),
          xAxis = list(
            type = "category",
            data = as.list(visits),
            position = "top",
            axisLine = list(show = FALSE),
            axisTick = list(show = FALSE),
            axisLabel = list(
              color = PP_AXIS_LABEL_COLOR, fontSize = 11,
              rotate = if (length(visits) > 6) 30 else 0
            ),
            splitLine = list(show = FALSE)
          ),
          yAxis = list(
            type = "category",
            data = as.list(unname(param_labels)),
            inverse = TRUE,
            axisLine = list(show = FALSE),
            axisTick = list(show = FALSE),
            axisLabel = list(
              color = PP_AXIS_LABEL_COLOR, fontSize = 11,
              width = 120, overflow = "truncate"
            ),
            splitLine = list(show = FALSE)
          ),
          visualMap = visual_map,
          series = list(list(
            type = "heatmap",
            data = heat_data,
            emphasis = list(
              itemStyle = list(
                borderColor = "#374151",
                borderWidth = 1
              )
            ),
            itemStyle = list(
              borderColor = "#ffffff",
              borderWidth = 2,
              borderRadius = 2
            )
          ))
        )) |>
        echarts4r::e_text_style(fontFamily = "system-ui, -apple-system, sans-serif")
    }
)
