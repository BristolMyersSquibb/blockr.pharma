# Patient Profile Viz: Orthostatic Blood Pressure
#
# Connected dot-plot showing BP by body position. Which positions appear
# depends on the data: ADaM studies encode position in ATPT as a timepoint
# phrase, SDTM-shaped studies use the VSPOS position variable.
# Controls: visits (checkbox of available visits).
#
# Data requirements (declared via new_pp_viz()):
#   advs: required PARAMCD, AVAL, ATPT (or VSPOS); optional AVISIT

#' Orthostatic BP visualization definition
#' @noRd
ortho_bp_viz <- new_pp_viz(
  id = "ortho_bp",
  label = "Orthostatic BP",
  domain = "Vitals",
  icon = "arrows-vertical",
  color = "#0891B2",
  description = "Blood pressure by position (lying, standing 1min, 3min)",
  tables = "advs",
  requires = list(advs = list(
    PARAMCD = NULL,
    AVAL    = NULL,
    ATPT    = "VSPOS"
  )),
  optional = list(advs = list(AVISIT = NULL)),
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
    tbl <- as.data.frame(tbls[["advs"]])

    # Filter to BP params with ATPT
      bp <- tbl[tbl$PARAMCD %in% c("SYSBP", "DIABP") &
        !is.na(tbl$AVAL) &
        nzchar(trimws(tbl$ATPT)), , drop = FALSE]
      if (nrow(bp) == 0) return(pp_empty_chart("No orthostatic BP records"))

      has_avisit <- "AVISIT" %in% colnames(bp)

      # Position categories. ADaM studies spell the position out as an ATPT
      # timepoint phrase; SDTM studies use the VSPOS controlled terms, which
      # pp_resolve_requires() has already renamed to ATPT by this point.
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
      if (nrow(bp) == 0) return(pp_empty_chart("No recognized BP positions"))

      # Ordered by increasing orthostatic challenge, restricted to what the
      # study actually recorded: a study may carry two positions, or five.
      positions <- intersect(
        c("Lying", "Semi-recumbent", "Sitting", "Standing",
          "Standing 1m", "Standing 3m"),
        unique(bp$position)
      )

      # Available visits
      if (has_avisit) {
        all_visits <- sort(unique(trimws(bp$AVISIT)))
        all_visits <- all_visits[nzchar(all_visits)]
      } else {
        all_visits <- character(0)
      }

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

      all_series <- list()
      param_labels <- c(SYSBP = "Systolic", DIABP = "Diastolic")

      for (pc in c("SYSBP", "DIABP")) {
        pc_data <- bp[bp$PARAMCD == pc, , drop = FALSE]
        if (nrow(pc_data) == 0) next
        is_systolic <- pc == "SYSBP"

        for (vi in seq_along(sel_visits)) {
          visit <- sel_visits[vi]
          v_data <- if (has_avisit) {
            pc_data[trimws(pc_data$AVISIT) == visit, , drop = FALSE]
          } else {
            pc_data
          }
          if (nrow(v_data) == 0) next

          color <- visit_colors[((vi - 1L) %% length(visit_colors)) + 1L]

          # One value per position
          vals <- vapply(positions, function(pos) {
            rows <- v_data[v_data$position == pos, , drop = FALSE]
            if (nrow(rows) == 0) return(NA_real_)
            mean(rows$AVAL, na.rm = TRUE)
          }, numeric(1))

          # Carry the category index explicitly: dropping a missing position
          # from a scalar-valued series would slide every later value one
          # category to the left.
          data_points <- lapply(seq_along(positions), function(pi) {
            if (is.na(vals[pi])) return(NULL)
            val <- unname(vals[pi])
            tt <- paste0(
              '<div style="min-width:140px">',
              '<div style="font-size:13px;font-weight:600;margin-bottom:2px">',
              param_labels[[pc]], ' \u2014 ', visit, '</div>',
              '<div style="font-size:12px;line-height:1.6">',
              '<span style="color:#6b7280">Position:</span> ',
              positions[pi],
              '<br/><span style="color:#6b7280">Value:</span> <b>',
              round(val, 1), '</b> mmHg</div></div>'
            )
            list(value = list(pi - 1L, val), tooltip_text = tt)
          })
          data_points <- Filter(Negate(is.null), data_points)

          series_name <- paste(param_labels[[pc]], visit)

          all_series <- c(all_series, list(list(
            type = "line",
            name = series_name,
            data = data_points,
            lineStyle = list(
              color = color,
              width = if (is_systolic) 2 else 1.5,
              type = if (is_systolic) "solid" else "dashed"
            ),
            itemStyle = list(color = color),
            symbolSize = 8,
            tooltip = list(
              formatter = htmlwidgets::JS(
                "function(params) { return params.data.tooltip_text || ''; }"
              )
            )
          )))
        }
      }

      if (length(all_series) == 0) {
        return(pp_empty_chart("No orthostatic BP data for selected visits"))
      }

      echarts4r::e_charts(height = 300) |>
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
          grid = list(
            left = 60, right = 20, top = 20, bottom = 40,
            borderColor = "transparent"
          ),
          xAxis = list(
            type = "category",
            data = positions,
            axisLine = list(show = FALSE),
            axisTick = list(show = FALSE),
            axisLabel = list(color = PP_AXIS_LABEL_COLOR, fontSize = 11)
          ),
          yAxis = list(
            type = "value",
            name = "mmHg",
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
        echarts4r::e_text_style(fontFamily = "system-ui, -apple-system, sans-serif")
    }
)
