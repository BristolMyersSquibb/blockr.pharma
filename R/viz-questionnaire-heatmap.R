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
  label = "Questionnaire heatmap",
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
      choices = c("Analysis value" = "AVAL", "Change from baseline" = "CHG")
    )
  ),
  exhibit = function(dm_obj, time_range, settings = list(),
                     ref_ms = NA_real_, mode = "date") {
    pp_static_heatmap(dm_obj, time_range, settings, ref_ms, mode)
  },
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

      # The tooltip's headline is the item's full name, where the axis label
      # above is cut to fit.
      tip_heads <- vapply(params, function(pc) {
        if (has_param) {
          pp_tip_param_words(tbl$PARAM[tbl$PARAMCD == pc][1])
        } else {
          pc
        }
      }, character(1))
      value_word <- if (y_col == "CHG") {
        "Change from baseline"
      } else {
        "Analysis value"
      }

      # Heatmap data: value = [visit_idx, param_idx, value]
      heat_data <- list()
      for (vi in seq_along(visits)) {
        for (pi in seq_along(params)) {
          rows <- tbl[trimws(tbl$AVISIT) == visits[vi] &
            tbl$PARAMCD == params[pi], , drop = FALSE]
          val <- if (nrow(rows) > 0) mean(rows[[y_col]], na.rm = TRUE) else NA
          if (!is.na(val)) {
            val <- round(val, 2)
            heat_data <- c(heat_data, list(list(
              value = list(vi - 1L, pi - 1L, val),
              tip = pp_tip(tip_heads[[pi]], rows = list(
                pp_tip_row(value_word,
                           pp_tip_num(val, signed = y_col == "CHG")),
                pp_tip_row("Visit", pp_tip_case(visits[vi]))
              ))
            )))
          }
        }
      }

      if (length(heat_data) == 0) {
        return(pp_empty_chart("No heatmap data"))
      }

      all_vals <- vapply(heat_data, function(x) x$value[[3]], numeric(1))
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
          textStyle = list(fontSize = 11, color = "var(--blockr-color-text-muted)"),
          inRange = list(color = list("#059669", "#f9fafb", "#DC2626"))
        )
      } else {
        visual_map <- list(
          min = min_val, max = max_val,
          calculable = TRUE,
          orient = "horizontal",
          left = "center", bottom = 0,
          itemWidth = 10, itemHeight = 120,
          textStyle = list(fontSize = 11, color = "var(--blockr-color-text-muted)"),
          inRange = list(color = list("#dbeafe", "#ffffff", "#fecaca"))
        )
      }

      # Chrome plus a fixed 28px row, same rule as the gantt lanes: a minimum
      # height would stretch a short questionnaire's rows apart instead of
      # just drawing a short chart.
      chart_height <- PP_PLOT_TOP + 50 + max(length(params), 1L) * 28

      echarts4r::e_charts(height = chart_height) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          tooltip = pp_tooltip(),
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
            tooltip = list(formatter = PP_TIP_FORMATTER),
            emphasis = list(
              itemStyle = list(
                borderColor = "#374151",
                borderWidth = 1
              )
            ),
            itemStyle = list(
              borderColor = "var(--blockr-color-bg-surface)",
              borderWidth = 2,
              borderRadius = 2
            )
          ))
        )) |>
        echarts4r::e_text_style(fontFamily = "var(--bs-body-font-family)")
    }
)
