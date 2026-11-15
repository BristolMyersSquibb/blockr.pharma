# The swimmer and the old AE and con-med timeline are gone; the patient
# timeline draws both. A board saved with either calls its constructor with
# the saved state, so the two names stay and build the patient timeline with
# the closest settings. The block then saves as the patient timeline.
#
# The saved prepare script, summaries, sort and subtitle are dropped: they
# name the old frame's functions and columns.

# What both keep of a saved block: the texts, drill-down and reader tools.
retired_keep <- function(args) {
  keep <- c("caption", "drill", "drill_target", "drill_table", "search",
            "download", "open")
  args[intersect(names(args), keep)]
}

# A saved title or block name, unless it is the old default.
retired_text <- function(x, default) {
  x <- as.character(unlist(x))
  if (length(x) == 1L && nzchar(x) && !x %in% default) x
}

# A saved value as one string, or NULL.
retired_value <- function(x) {
  x <- as.character(unlist(x))
  if (length(x)) x[[1L]]
}

# The swimmer: the efficacy preset with its response, drug lines,
# discontinuation and death, con meds when it drew them, its grouping, doses
# and cutoff.
new_swimmer_block <- function(by = c("ARM", "USUBJID"),
                              script_values = list(), title = NULL, ...) {
  args <- list(...)
  sv <- script_values
  own <- intersect(names(args), c("doses", "dose_tags", "cutoff", "con_meds",
                                  "reasons", "events", "parameter", "group"))
  sv[own] <- args[own]
  events <- as.character(unlist(sv$events %||% c("Progression", "Death")))
  layers <- c(
    "Response", "Treatment",
    if (!isFALSE(as.logical(unlist(sv$reasons))) || "Death" %in% events) {
      "Discontinuation and death"
    },
    if (isTRUE(as.logical(unlist(sv$con_meds)))) "Con meds"
  )
  group <- if (!is.null(sv$group)) {
    retired_value(sv$group)
  } else {
    by <- as.character(unlist(by))
    if (length(by) > 1L) by[[1L]] else PT_NO_GROUP
  }
  # The swimmer's default arm column: the patient timeline finds the arm.
  if (identical(group, "ARM")) group <- NULL
  new <- list(layers = layers, group = group,
              response = retired_value(sv$parameter),
              doses = retired_value(sv$doses),
              dose_tags = as.logical(unlist(sv$dose_tags))[1L],
              cutoff = sv$cutoff)
  new <- Filter(function(v) length(v) && !all(is.na(v)), new)
  do.call(new_efficacy_timeline_block, c(
    list(script_values = new,
         title = retired_text(title, "Efficacy swimmer"),
         block_name = retired_text(args$block_name, "Efficacy swimmer")),
    retired_keep(args)
  ))
}

# The old timeline: the safety preset with the sources it drew, dose bars
# for treatment, death unless it was off, and its colours and record
# settings.
new_timeline_block <- function(script_values = list(), title = NULL, ...) {
  args <- list(...)
  shared <- c(setdiff(PT_SETTINGS, c("layers", "sort_col", "response",
                                     "treatment_as", "doses", "dose_tags")),
              PT_RETIRED)
  sv <- script_values
  own <- intersect(names(args), c(shared, "show", "death"))
  sv[own] <- args[own]
  show <- as.character(unlist(sv$show %||% c("Adverse events", "Treatment")))
  layers <- c(
    intersect(c("Treatment", "Adverse events", "Con meds"), show),
    if (!isFALSE(as.logical(unlist(sv$death)))) "Discontinuation and death"
  )
  new <- sv[intersect(names(sv), shared)]
  # The old timeline's default arm column: the patient timeline finds the arm.
  if (identical(retired_value(new$group), "TRT01A")) new$group <- NULL
  new$layers <- layers
  new$treatment_as <- "bars by dose"
  do.call(new_safety_timeline_block, c(
    list(script_values = new,
         title = retired_text(title, "Timeline"),
         block_name = retired_text(args$block_name, "Timeline")),
    retired_keep(args)
  ))
}
