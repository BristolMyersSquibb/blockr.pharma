# Patient Profile Viz: ADAS-Cog Trajectory
#
# Multi-line chart of ADAS-Cog scores over visits.
# Controls: items (checkbox of PARAMCDs), chg (toggle absolute vs change).
#
# Data requirements (declared via new_pp_viz()):
#   adqsadas: required PARAMCD, AVAL, ADT; optional AVISIT, PARAM, CHG
# (canonical names; pp_normalize_dm() maps ADTM -> ADT dm-wide)

#' ADAS-Cog Trajectory visualization definition
#' @noRd
adas_trajectory_viz <- new_pp_viz(
  id = "adas_trajectory",
  label = "ADAS-Cog trajectory",
  domain = "Questionnaires",
  icon = "clipboard-pulse",
  color = "#7C3AED",
  description = "ADAS-Cog scores over visits with item-level drill-down",
  tables = "adqsadas",
  requires = list(adqsadas = c("PARAMCD", "AVAL", "ADT")),
  optional = list(adqsadas = c("AVISIT", "PARAM", "CHG")),
  controls = list(
    items = list(
      type = "checkbox",
      label = "Items",
      phrase = "items: {}",
      all_word = "all",
      default = "ACTOT",
      choices_from = "PARAMCD"
    ),
    chg = list(
      type = "toggle",
      label = "Change from baseline",
      default = FALSE
    )
  ),
  exhibit = function(dm_obj, time_range, settings = list(),
                     ref_ms = NA_real_, mode = "date") {
    pp_static_adas(dm_obj, time_range, settings, ref_ms, mode)
  },
  render = function(dm_obj, time_range, settings = list(),
                   ref_ms = NA_real_, mode = "date") {
    tbl <- pp_prepare_findings(dm_obj, "adqsadas")
    if (is.null(tbl)) return(pp_empty_chart("No ADAS-Cog records"))

    tbl <- tbl[!is.na(tbl$ADT) & !is.na(tbl$AVAL), , drop = FALSE]
    if (nrow(tbl) == 0) return(pp_empty_chart("No ADAS-Cog records"))

      # Settings
      sel_items <- settings$items %||% "ACTOT"
      use_chg <- isTRUE(settings$chg)
      y_col <- if (use_chg && "CHG" %in% colnames(tbl)) "CHG" else "AVAL"

      # Filter to selected items
      tbl <- tbl[tbl$PARAMCD %in% sel_items, , drop = FALSE]
      if (nrow(tbl) == 0) return(pp_empty_chart("No data for selected items"))

      has_param <- "PARAM" %in% colnames(tbl)
      # as.character() before the colors[[pc]] lookup below: [[ on a factor
      # indexes by LEVEL CODE, so a factor PARAMCD silently mis-colors every
      # series, or errors out past the palette length.
      params <- sort(unique(as.character(tbl$PARAMCD)))

      colors <- c(
        ACTOT = "#7C3AED",
        ACITM01 = "#2563EB", ACITM02 = "#DC2626", ACITM03 = "#059669",
        ACITM04 = "#D97706", ACITM05 = "#0891B2", ACITM06 = "#EA580C",
        ACITM07 = "#374151", ACITM08 = "#BE123C", ACITM09 = "#0D9488",
        ACITM10 = "#6366F1", ACITM11 = "#CA8A04", ACITM12 = "#9333EA",
        ACITM13 = "#E11D48", ACITM14 = "#14B8A6"
      )

      all_series <- lapply(params, function(pc) {
        p_data <- tbl[tbl$PARAMCD == pc, , drop = FALSE]
        p_data <- p_data[order(p_data$ADT), , drop = FALSE]
        color <- colors[[pc]] %||% "#6b7280"
        is_total <- pc == "ACTOT"

        param_label <- pc
        if (has_param && nrow(p_data) > 0) {
          param_label <- as.character(p_data$PARAM[1])
        }

        tip_head <- if (is_total) {
          "ADAS-Cog total"
        } else {
          pp_tip_param_words(param_label)
        }
        col_at <- function(nm, i) {
          if (nm %in% colnames(p_data)) p_data[[nm]][i] else NA
        }

        data_points <- lapply(seq_len(nrow(p_data)), function(i) {
          val <- p_data[[y_col]][i]
          if (is.na(val)) return(NULL)
          when <- pp_tip_when(p_data$ADT[i], col_at("ADY", i), ref_ms, mode)
          list(
            value = list(pp_xval(p_data$ADT[i], ref_ms, mode), val),
            tip = pp_tip(tip_head, color = color, rows = list(
              pp_tip_row("Analysis value", pp_tip_num(p_data$AVAL[i])),
              pp_tip_row("Change from baseline",
                         pp_tip_num(col_at("CHG", i), signed = TRUE)),
              pp_tip_row("Visit", pp_tip_case(col_at("AVISIT", i))),
              pp_tip_row("Day", when$main, when$meta)
            ))
          )
        })
        data_points <- Filter(Negate(is.null), data_points)

        list(
          type = "line",
          name = param_label,
          data = data_points,
          lineStyle = list(
            color = color,
            width = if (is_total) 2.5 else 1.5,
            opacity = if (is_total) 1 else 0.7
          ),
          itemStyle = list(color = color),
          symbolSize = if (is_total) 8 else 5,
          z = if (is_total) 3 else 2,
          tooltip = list(formatter = PP_TIP_FORMATTER)
        )
      })

      y_label <- if (use_chg) "Change from Baseline" else "Score"

      echarts4r::e_charts(height = 350) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          tooltip = pp_tooltip(),
          legend = list(
            show = length(params) > 1,
            bottom = 0, left = "center",
            textStyle = list(fontSize = 11, color = PP_AXIS_LABEL_COLOR),
            itemWidth = 14, itemHeight = 10
          ),
          grid = list(
            left = PP_GRID_LEFT, right = 20, top = PP_PLOT_TOP,
            bottom = if (length(params) > 1) 40 else 30,
            borderColor = "transparent"
          ),
          xAxis = pp_time_axis(time_range, ref_ms, mode),
          yAxis = list(
            type = "value",
            name = y_label,
            nameTextStyle = list(fontSize = 11, color = PP_AXIS_LABEL_COLOR),
            axisLine = list(show = FALSE),
            axisTick = list(show = FALSE),
            axisLabel = list(color = PP_AXIS_LABEL_COLOR, fontSize = 11),
            splitLine = list(
              show = TRUE,
              lineStyle = list(color = PP_SPLIT_LINE_COLOR)
            )
          ),
          series = all_series
        )) |>
        echarts4r::e_text_style(fontFamily = "var(--bs-body-font-family)")
    }
)
