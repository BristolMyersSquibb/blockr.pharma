# The patient profile as an exhibit.
#
# This file is the block's seam into the blockr.viz / blockr.outline export
# pipeline, mirroring what the chart block does one package over:
#
#   * pp_patient_exhibit() rebuilds the selected visuals server-side from
#     the block's serializable state (a ggplot per viz, via each viz's
#     `exhibit` twin) and returns them in a container classed
#     c("pp_exhibit", "blockr_exhibit") -- the marker blockr.outline's deck
#     routes through blockr.viz's exhibit generics. Handed a COHORT, it
#     renders every subject: one group of slides per patient, so a deck
#     built over a drilled-down cohort reads as a per-patient review.
#   * pptx_add_exhibit.pp_exhibit() / html_exhibit.pp_exhibit() render that
#     container: one slide (or one image) per visual per patient,
#     delegating each plot to blockr.viz's gg methods so a profile slide
#     and a chart slide are placed by the same rule.
#   * report_call.patient_profile_block() tells blockr.outline how the
#     block's result prints: a literal, self-qualified
#     blockr.pharma::pp_patient_exhibit(<var>, <state...>) call.
#
# The methods extend generics that live in blockr.viz (a Suggests), so they
# are registered at load time via pp_register_viz_s3() (see zzz.R), the
# soft-registration pattern -- never through NAMESPACE.

#' Which viz-level exports are possible right now
#'
#' Gates the download buttons and the exhibit builder: static rendering
#' needs ggplot2, the writers live in blockr.viz.
#' @noRd
pp_exhibit_ready <- function() {
  pp_gg_available() && requireNamespace("blockr.viz", quietly = TRUE)
}

#' Assemble the render-time settings for one viz
#'
#' The pure core of what the block's render_viz_slot() injects on top of the
#' persisted per-viz settings: resolved role columns for the viz's `uses`
#' declaration, scale-map colors for the severity / indication roles, cycle
#' anchors, and the block-level smoothing. Shared by the live render path
#' and the static exhibit path so the two cannot drift.
#'
#' @param viz A `pp_viz` definition.
#' @param viz_settings The persisted settings for this viz (may be empty).
#' @param roles Resolved roles (see [pp_resolve_roles()]).
#' @param dm_obj The subject-scoped dm.
#' @param scale_map The board scale map, or `NULL` (built-in constants).
#' @param cycle_anchors Cycle anchors, or `NULL`.
#' @param smooth Block-level line smoothing.
#' @return The merged settings list.
#' @noRd
pp_viz_exhibit_settings <- function(viz, viz_settings, roles, dm_obj,
                                    scale_map = NULL, cycle_anchors = NULL,
                                    smooth = "auto") {
  viz_settings <- viz_settings %||% list()
  uses <- viz$uses %||% character()
  if (length(uses)) {
    viz_settings$roles <- roles[intersect(uses, names(roles))]
  }
  if ("severity" %in% uses) {
    sev_colors <- pp_sev_scale_colors(scale_map, dm_obj,
                                      sev_col = roles$severity)
    if (!is.null(sev_colors)) {
      viz_settings$sev_colors <- sev_colors
    }
  }
  if ("indication" %in% uses) {
    indc_colors <- pp_indc_scale_colors(scale_map, dm_obj,
                                        indc_col = roles$indication)
    if (!is.null(indc_colors)) {
      viz_settings$indc_colors <- indc_colors
    }
  }
  if ("cycle" %in% uses) {
    viz_settings$cycle_anchors <- cycle_anchors
  }
  viz_settings$smooth <- smooth
  viz_settings
}

#' The Patient Profile, Rebuilt as Static Exhibits
#'
#' Re-derives the patient profile's selected visualizations server-side as
#' ggplots -- the printed form of the block, built from the same state the
#' block serializes (`selected`, `viz_settings`, `timeline_mode`, ...). This
#' is what the block's download buttons write and what a blockr.outline deck
#' places on slides: the export pipeline never captures the live echarts
#' canvases, it re-renders from state, exactly as the chart block's
#' [blockr.viz::static_chart()] path does.
#'
#' Handed a single-subject dm (the block's result once a patient is
#' picked), it renders that patient. Handed a cohort, it renders EVERY
#' subject -- one group of visuals per patient, so a deck over a
#' drilled-down cohort becomes a per-patient review -- up to
#' `max_subjects`, and says on the console what it dropped past the cap.
#'
#' Visuals without a static twin (currently the NPI-X radar and the cycle
#' band) are skipped with a message, as is any visual whose data
#' requirements the dm does not meet.
#'
#' @param data A `dm` of ADaM tables (the patient profile block's result):
#'   one subject, or a cohort.
#' @param selected Character vector of viz ids to render, in order. `NULL`
#'   falls back to the block's own default selection.
#' @param viz_settings Named list of per-viz settings, keyed by viz id --
#'   the block's `viz_settings` state.
#' @param timeline_mode `"rday"` (relative day) or `"date"`; falls back to
#'   `"date"` per patient when that subject has no timeline reference.
#' @param show_prestudy Show the full pre-treatment history instead of
#'   clipping to the screening window.
#' @param smooth Findings line smoothing, `"auto"` or `"off"` (static lines
#'   are straight segments either way; kept for state parity).
#' @param subject USUBJID to scope a cohort to one patient. `NULL` renders
#'   the whole cohort.
#' @param roles Declared study roles (the `"study_roles"` board option
#'   value), or `NULL` for the package conventions.
#' @param scale_map A blockr.theme board scale map for the severity /
#'   indication colors, or `NULL` for the built-in constants.
#' @param title Optional exhibit title (used by the slide methods).
#' @param max_subjects Cap on how many patients a cohort renders
#'   (default `getOption("blockr.pharma.exhibit_max_subjects", 25L)`): a
#'   deck over an unfiltered 300-patient study is a mistake, not a review.
#'
#' @return An object of class `c("pp_exhibit", "blockr_exhibit")` whose
#'   `patients` field holds, per rendered subject, the ggplots and their
#'   labels -- or `NULL` when nothing can be rendered.
#'
#' @export
pp_patient_exhibit <- function(data,
                               selected = NULL,
                               viz_settings = list(),
                               timeline_mode = c("rday", "date"),
                               show_prestudy = FALSE,
                               smooth = c("auto", "off"),
                               subject = NULL,
                               roles = NULL,
                               scale_map = NULL,
                               title = NULL,
                               max_subjects = getOption(
                                 "blockr.pharma.exhibit_max_subjects", 25L
                               )) {
  pp_gg_require()
  timeline_mode <- match.arg(timeline_mode)
  smooth <- match.arg(smooth)

  if (!inherits(data, "dm")) {
    message("[pp exhibit] not a dm; nothing to render")
    return(NULL)
  }

  # The block's result carries the unfiltered cohort as an attribute (see
  # pp_pick_subject()): a deck built over the block renders the COHORT --
  # the block's true view -- even though the block's downstream contract is
  # the picked patient. An explicit `subject` still narrows to one.
  cohort <- attr(data, "pp_cohort", exact = TRUE)
  if (inherits(cohort, "dm")) {
    data <- cohort
  }

  nd <- pp_normalize_dm(data)
  ids <- pp_subject_ids(nd)
  if (length(ids) == 0L) {
    message("[pp exhibit] no subjects in the dm; nothing to render")
    return(NULL)
  }

  subjects <- if (!is.null(subject)) {
    if (!subject %in% ids) {
      message("[pp exhibit] subject '", subject, "' is not in the cohort; ",
              "nothing to render")
      return(NULL)
    }
    subject
  } else {
    ids
  }
  if (length(subjects) > max_subjects) {
    message(
      "[pp exhibit] cohort holds ", length(subjects), " subjects; ",
      "rendering the first ", max_subjects, " (raise with ",
      "options(blockr.pharma.exhibit_max_subjects = ) or scope the ",
      "profile to a patient)"
    )
    subjects <- subjects[seq_len(max_subjects)]
  }

  # Study-level work happens once: roles, the viz catalog and the default
  # selection describe the study, not a patient.
  resolved <- pp_resolve_roles(nd, roles)
  catalog <- c(
    patient_profile_static_vizs(),
    pp_cycle_vizs(nd),
    pp_findings_vizs(nd)
  )
  tbl_names <- names(dm::dm_get_tables(nd))
  catalog <- Filter(function(v) all(v$tables %in% tbl_names), catalog)

  if (is.null(selected)) {
    # The block's own default selection (see its init observer): the
    # overview first, then the next two cards.
    avail_ids <- names(catalog)
    selected <- if ("patient_overview" %in% avail_ids) {
      others <- setdiff(avail_ids, c("patient_overview", "ae_gantt"))
      c("patient_overview", utils::head(others, 2L))
    } else {
      utils::head(avail_ids, 2L)
    }
  }

  patients <- list()
  for (subj in subjects) {
    scoped <- if (length(ids) == 1L) nd else pp_scope_subject(nd, subj)
    rendered <- pp_exhibit_one_patient(
      scoped, subj, catalog, selected, viz_settings, resolved,
      scale_map = scale_map, timeline_mode = timeline_mode,
      show_prestudy = show_prestudy, smooth = smooth
    )
    if (!is.null(rendered)) {
      patients[[subj]] <- rendered
    }
  }

  if (!length(patients)) {
    message("[pp exhibit] no exportable visualization rendered")
    return(NULL)
  }

  structure(
    list(patients = patients, title = title),
    class = c("pp_exhibit", "blockr_exhibit")
  )
}

#' Render one patient's selected visuals
#'
#' @return `list(subject, plots, labels)`, or `NULL` when nothing rendered
#'   for this patient. The timeline window, reference and cycle anchors are
#'   all per-patient, exactly as in the block's reactives.
#' @noRd
pp_exhibit_one_patient <- function(scoped, subj, catalog, selected,
                                   viz_settings, resolved,
                                   scale_map = NULL,
                                   timeline_mode = "rday",
                                   show_prestudy = FALSE,
                                   smooth = "auto") {
  time_range <- pp_compute_time_range(scoped, ref_col = resolved$timeline)
  if (is.null(time_range)) {
    message("[pp exhibit] ", subj, ": no dated records; skipped")
    return(NULL)
  }
  ref_ms <- pp_compute_ref_ms(scoped, resolved$timeline)
  if (!isTRUE(show_prestudy)) {
    time_range <- pp_clip_prestudy(time_range, ref_ms)
  }
  mode <- timeline_mode
  if (identical(mode, "rday") && is.na(ref_ms)) {
    mode <- "date"
  }
  cycle_anchors <- pp_cycle_anchors(scoped)

  plots <- list()
  labels <- character()
  for (viz_id in selected) {
    viz <- catalog[[viz_id]]
    if (is.null(viz)) {
      message("[pp exhibit] unknown viz '", viz_id, "'; skipped")
      next
    }
    if (!is.function(viz$exhibit)) {
      message("[pp exhibit] '", viz_id, "' has no static exhibit; skipped")
      next
    }
    if (!isTRUE(pp_resolve_requires(scoped, viz)$ok)) {
      message("[pp exhibit] '", viz_id, "' misses required columns; skipped")
      next
    }
    if (pp_no_patient_rows(scoped, viz$tables)) {
      message("[pp exhibit] '", viz_id, "' has no data for ", subj,
              "; skipped")
      next
    }
    settings <- pp_viz_exhibit_settings(
      viz, viz_settings[[viz_id]], resolved, scoped,
      scale_map = scale_map, cycle_anchors = cycle_anchors, smooth = smooth
    )
    p <- tryCatch(
      viz$exhibit(scoped, time_range, settings, ref_ms, mode),
      error = function(e) {
        message("[pp exhibit] '", viz_id, "' failed for ", subj, ": ",
                conditionMessage(e))
        NULL
      }
    )
    if (is.null(p)) next
    plots[[viz_id]] <- p
    labels[[viz_id]] <- viz$label
  }

  if (!length(plots)) {
    return(NULL)
  }
  list(subject = subj, plots = plots, labels = labels)
}

#' Slide title for one visual of a profile exhibit
#'
#' Single patient: the deck's title (or the exhibit's own, or "Patient
#' <id>") prefixes the visual's label. Multiple patients: the patient
#' identifies the slide -- a deck title repeated across thirty slides says
#' nothing, where the USUBJID is the one thing a reviewer scans for.
#' @noRd
pp_exhibit_slide_title <- function(x, patient, i, title = NULL) {
  label <- unname(patient$labels[[i]])
  if (length(x$patients) > 1L) {
    return(paste0("Patient ", patient$subject, ": ", label))
  }
  base <- title %||% x$title %||% paste("Patient", patient$subject)
  if (!nzchar(base)) {
    return(label)
  }
  paste0(base, ": ", label)
}

#' @noRd
pp_exhibit_pptx_add <- function(doc, x, title = NULL, subtitle = NULL,
                                caption = NULL, template = NULL,
                                layout = NULL, master = NULL, top = NULL,
                                # Accepted and ignored: the table
                                # paginator's knobs. Each visual is one
                                # slide by definition (the file writer
                                # passes them to whichever method it
                                # reaches).
                                max_rows = NULL, max_cols = NULL,
                                min_font_size = NULL, ...) {
  for (patient in x$patients) {
    for (i in seq_along(patient$plots)) {
      doc <- blockr.viz::pptx_add_exhibit(
        doc, patient$plots[[i]],
        title = pp_exhibit_slide_title(x, patient, i, title),
        template = template, layout = layout, master = master, top = top,
        ...
      )
    }
  }
  doc
}

#' @noRd
pp_exhibit_html <- function(x, title = NULL, caption = NULL,
                            max_height = NULL, default_expanded = NULL,
                            ...) {
  many <- length(x$patients) > 1L
  htmltools::tagList(lapply(x$patients, function(patient) {
    htmltools::tags$div(
      class = "pp-exhibit-patient",
      if (many) {
        htmltools::tags$div(
          style = paste0(
            "font-size:14px;font-weight:600;color:#111827;",
            "margin:16px 0 4px;"
          ),
          paste("Patient", patient$subject)
        )
      },
      lapply(seq_along(patient$plots), function(i) {
        htmltools::tags$div(
          class = "pp-exhibit-item",
          htmltools::tags$div(
            style = paste0(
              "font-size:12px;font-weight:500;color:#374151;",
              "margin:8px 0 4px;"
            ),
            unname(patient$labels[[i]])
          ),
          blockr.viz::html_exhibit(patient$plots[[i]])
        )
      })
    )
  }))
}

#' @export
print.pp_exhibit <- function(x, ...) {
  for (patient in x$patients) {
    for (p in patient$plots) print(p)
  }
  invisible(x)
}

#' How the patient profile block's result prints in a rendered document
#'
#' The blockr.viz `report_call()` method for the patient profile block: a
#' literal, self-qualified [pp_patient_exhibit()] call over the result
#' variable, carrying the block's committed state (read from the
#' constructor closure, the same values serialization reads). Arguments at
#' their defaults are dropped so the emitted call stays readable.
#'
#' A block with no picked subject passes the cohort through, and the
#' exhibit then renders every patient -- the deck becomes a per-patient
#' review of whatever cohort reaches the block.
#'
#' Study-level board options (declared roles, the scale map) are NOT baked
#' in: they are not block state, and the exhibit falls back to the same
#' package conventions the block itself uses when nothing is declared.
#' @noRd
pp_report_call <- function(x, var, ...) {
  env <- environment(x[["expr_server"]])
  state <- function(nm) {
    v <- get0(nm, envir = env, ifnotfound = NULL)
    if (is.function(v)) NULL else v
  }

  args <- list()

  sel <- state("selected")
  if (is.character(sel) && length(sel)) {
    args$selected <- as.character(sel)
  }

  vs <- state("viz_settings")
  if (is.list(vs) && length(vs)) {
    # Only the selected vizs' settings travel: the block initializes
    # defaults for EVERY available card, and a literal carrying all of them
    # would bury the call.
    if (length(args$selected)) {
      vs <- vs[intersect(names(vs), args$selected)]
    }
    vs <- vs[vapply(vs, function(v) is.list(v) && length(v) > 0L,
                    logical(1L))]
    if (length(vs)) {
      args$viz_settings <- vs
    }
  }

  tm <- state("timeline_mode")
  if (is.character(tm) && length(tm) == 1L && !identical(tm, "rday")) {
    args$timeline_mode <- tm
  }
  if (isTRUE(state("show_prestudy"))) {
    args$show_prestudy <- TRUE
  }
  sm <- state("smooth")
  if (is.character(sm) && length(sm) == 1L && !identical(sm, "auto")) {
    args$smooth <- sm
  }
  # The picked subject is deliberately NOT emitted: a deck renders the
  # block's cohort (recovered from the result's pp_cohort attribute), one
  # patient group per subject. The pick is an interactive convenience; the
  # cohort is the profile's true view in a document.

  as.call(c(
    list(
      call("::", as.name("blockr.pharma"), as.name("pp_patient_exhibit")),
      as.name(var)
    ),
    args
  ))
}

#' Register the S3 methods that extend blockr.viz's generics
#'
#' blockr.viz is a Suggests, so the methods cannot be declared in NAMESPACE.
#' Registered when blockr.viz is already loaded, and again from its onLoad
#' hook otherwise -- the standard soft-registration pattern.
#' @noRd
pp_register_viz_s3 <- function() {
  register <- function(...) {
    if (!requireNamespace("blockr.viz", quietly = TRUE)) {
      return(invisible(NULL))
    }
    ns <- asNamespace("blockr.viz")
    registerS3method("report_call", "patient_profile_block",
                     pp_report_call, envir = ns)
    registerS3method("pptx_add_exhibit", "pp_exhibit",
                     pp_exhibit_pptx_add, envir = ns)
    registerS3method("html_exhibit", "pp_exhibit",
                     pp_exhibit_html, envir = ns)
    invisible(NULL)
  }
  if (isNamespaceLoaded("blockr.viz")) {
    register()
  }
  setHook(packageEvent("blockr.viz", "onLoad"), register)
  invisible(NULL)
}
