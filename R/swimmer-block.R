# The efficacy swimmer, built from blockr.viz's table block.
#
# One row per patient (grouped by arm), listing columns beside a swimlane:
# the response bar at full height with its events, a thin line per drug
# above it. The table block's prepare step, swimmer_frame(), reads the ADaM
# tables from the input dm and returns the long frame the spans column draws,
# so the board needs no prep blocks. The result is a table block and saves as
# one, like new_ae_heatmap_block().

#' Efficacy swimmer
#'
#' One row per patient: the overall response over time as a bar with first
#' progression, serious AEs and death on it, and a thin line per drug above
#' it, with an arrow while the drug is ongoing at the cutoff. Rows grow with
#' the number of drugs; past `max_lanes` drugs, study drugs keep their lanes
#' and the rest fold into a grey "Other" lane. Pointing at a drug lists its
#' cycles (number, dose, study day, delay). Time is weeks since the first
#' dose.
#'
#' The input is a dm of ADaM tables as they are: ADSL, ADEX, ADRS (overall
#' response per visit, `PARAMCD == "OVR"`) and, optional, ADAE. The block
#' computes study weeks, drugs ongoing to the cutoff, response periods and
#' the best overall response ([swimmer_frame()]). The doses setting sits
#' beside the title: "Exceptions" (dose cuts, late doses, skipped cycles),
#' "Dots" (one ring per administration) or "Plain".
#'
#' Returns a [blockr.viz::new_table_block()] in the groups layout.
#'
#' @param doses How a drug lane shows its doses: `"Exceptions"`, `"Dots"` or
#'   `"Plain"`.
#' @param con_meds Draw con meds from ADCM as lanes too.
#' @param max_lanes Lanes before drugs fold into "Other".
#' @param group Column grouping the rows. Default the arm.
#' @param title,subtitle Text above the table.
#' @param ... Forwarded to [blockr.viz::new_table_block()].
#' @return A table block (class `table_block`).
#' @examplesIf interactive()
#' new_swimmer_block()
#' @export
new_swimmer_block <- function(doses = "Exceptions",
                              con_meds = FALSE,
                              max_lanes = 4L,
                              group = "ARM",
                              title = "Efficacy swimmer",
                              subtitle = swimmer_subtitle(),
                              ...) {
  doses <- match.arg(doses, c("Exceptions", "Dots", "Plain"))
  args <- list(...)
  args[c("ctor", "ctor_pkg", "class")] <- NULL
  do.call(blockr.viz::new_table_block, c(
    list(
      layout = "groups",
      by = c(group, "USUBJID"),
      summaries = swimmer_summaries(),
      missing = "drop",
      sort_by = "Weeks",
      sort_dir = "desc",
      title = title,
      subtitle = subtitle,
      script = swimmer_script(max_lanes),
      script_values = list(doses = doses, con_meds = isTRUE(con_meds))
    ),
    args,
    list(ctor = "new_table_block", ctor_pkg = "blockr.viz")
  ))
}

#' The swimmer's prepare script
#'
#' Declares the doses and con meds settings and calls [swimmer_frame()] on
#' the input dm.
#'
#' @param max_lanes Lanes before drugs fold into "Other".
#' @return The script, one string.
#' @export
swimmer_script <- function(max_lanes = 4L) {
  paste(
    paste0("doses <- factor(\"Exceptions\", levels = c(\"Exceptions\", ",
           "\"Dots\", \"Plain\"))  #| select(label = \"Doses\")"),
    "con_meds <- FALSE  #| flag(label = \"Con meds\")",
    paste0("blockr.pharma::swimmer_frame(data, doses = doses, ",
           "con_meds = con_meds, max_lanes = ", as.integer(max_lanes), ")"),
    sep = "\n"
  )
}

#' The swimmer's sentence under the title
#'
#' @return A title template, see [blockr.viz::new_chart_block()].
#' @export
swimmer_subtitle <- function() {
  "Weeks since first dose, doses as {@doses}"
}

swimmer_summaries <- function() {
  list(
    list(type = "field", name = "BOR", col = "BOR"),
    list(type = "field", name = "Status", col = "status"),
    list(type = "simple", name = "Weeks", func = "max", col = "weeks",
         show = "number"),
    list(type = "spans", name = "Weeks since first dose", x = "start",
         xend = "end", color = "colour", label = "label",
         tooltip = c("days", "dose"), size = "lg",
         event_x = "event_wk", event_shape = "event", open = "ongoing",
         lane = "lane", seg_style = "mark", strip = "strip")
  )
}
