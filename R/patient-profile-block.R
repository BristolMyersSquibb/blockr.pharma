#' Patient Profile Block
#'
#' Displays stacked clinical echarts visualizations (AE Gantt bars, lab line
#' charts, vitals, questionnaires) with a searchable sidebar for toggling
#' vizs on/off and per-viz controls.
#'
#' Input: a dm object. When it carries exactly one subject (an upstream
#' drill-down has committed to a patient) the profile renders that subject
#' straight away. When it carries a cohort, the header picker selects one
#' subject within it and the block filters the dm down to that patient, so
#' the charts and the block's own output never disagree. Until a patient is
#' picked the block is a pass-through and the chart area shows a placeholder:
#' whose data this is is never guessed.
#'
#' @param selected Initial viz IDs to show (default: patient_overview +
#'   first available)
#' @param viz_settings Named list of per-viz settings
#'   (e.g., `list(adas_trajectory = list(items = "ACTOT"))`)
#' @param timeline_mode Initial timeline x-axis mode: `"rday"` (relative day
#'   from treatment start, ADaM \*DY convention; the default) or `"date"`
#'   (calendar dates). Changeable at runtime via the gear popover in the
#'   chart area header.
#' @param subject USUBJID to display, as a length-1 character. Only meaningful
#'   when the incoming dm carries more than one subject; a single-subject dm
#'   always renders its one subject. Ignored (and cleared) when the value is
#'   absent from the incoming cohort. Defaults to `NULL`, i.e. no patient
#'   chosen.
#' @param arm_var ADSL column holding the treatment / arm label, as a length-1
#'   character. Studies that do not ship the ADaM arm variables can name their
#'   own column here (e.g. `"TRT"`); it is used by both the subject picker and
#'   the treatment lane, so the two cannot disagree. Defaults to `NULL`, which
#'   falls back to the first of `ARM`, `ACTARM`, `TRT01P`, `TRT01A` present.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `patient_profile_block`
#'
#' @examples
#' # Construct the block
#' new_patient_profile_block(selected = c("patient_overview", "ae_gantt"))
#'
#' \dontrun{
#' # Serve it on a single-patient dm built from public CDISC ADaM data
#' library(pharmaverseadam)
#' library(dm)
#'
#' one <- adsl$USUBJID[1]
#' pp_dm <- dm(
#'   adsl = adsl[adsl$USUBJID == one, ],
#'   adae = adae[adae$USUBJID == one, ],
#'   advs = advs[advs$USUBJID == one, ]
#' )
#'
#' # `data` is keyed by block input name; a bare dm would be splatted into
#' # one argument per table.
#' blockr.core::serve(
#'   new_patient_profile_block(selected = c("patient_overview", "ae_gantt")),
#'   data = list(data = pp_dm)
#' )
#'
#' # Or hand it the whole cohort and pick a patient in the header. Passing
#' # `subject` preselects one; omit it to start on the picker.
#' cohort <- dm(adsl = adsl, adae = adae, advs = advs)
#' blockr.core::serve(
#'   new_patient_profile_block(subject = one),
#'   data = list(data = cohort)
#' )
#' }
#'
#' @export
new_patient_profile_block <- function(selected = NULL,
                                              viz_settings = list(),
                                              timeline_mode = "rday",
                                              subject = NULL,
                                              arm_var = NULL,
                                              ...) {
  timeline_mode <- match.arg(timeline_mode, c("rday", "date"))
  subject <- pp_validate_subject(subject)
  stopifnot(
    is.null(arm_var) ||
      (is.character(arm_var) && length(arm_var) == 1L && nzchar(arm_var))
  )

  # Validate selected viz IDs (static vizs only; findings group IDs

  # are validated at runtime when the dm data is available)
  if (!is.null(selected)) {
    static_ids <- names(patient_profile_static_vizs())
    bad <- setdiff(selected, static_ids)
    # Only warn for IDs that look like typos of static vizs
    # Findings group IDs (liver_panel, cbc, etc.) are valid at runtime
  }

  # viz_settings keys are validated at runtime (groups are dynamic)

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Currently picked USUBJID: character(0) when no patient chosen.
          r_subject <- shiny::reactiveVal(subject)

          # Study-declared arm column. Set once at study setup, no UI; held
          # as a reactiveVal so it round-trips through the block state like
          # every other constructor argument.
          r_arm_var <- shiny::reactiveVal(arm_var)

          # The incoming cohort. This is the universe the picker selects
          # within: an upstream drill-down narrows it, the picker never
          # widens it, so the two can never conflict.
          r_cohort <- shiny::reactive({
            pp_subject_choices(data(), r_arm_var())
          })

          # Stale-selection guard. When the upstream cohort changes and the
          # picked subject is no longer in it, clear the pick rather than
          # falling back to another patient. `subject` is in
          # `allow_empty_state` precisely so this clear does not wedge the
          # block.
          shiny::observeEvent(r_cohort(), {
            cur <- r_subject()
            if (length(cur) == 1L && !cur %in% r_cohort()$ids) {
              r_subject(character())
            }
          }, ignoreNULL = FALSE)

          # Pick a patient from the header popover.
          # Blockr.Select writes its value straight to the container's input id.
          shiny::observeEvent(input$pp_subject, {
            sel <- as.character(input$pp_subject)
            if (length(sel) == 1L && sel %in% r_cohort()$ids) {
              r_subject(sel)
            }
          })

          # Step to the previous / next patient in cohort order. Wraps, and
          # steps in from either end when nothing is picked yet.
          shiny::observeEvent(input$step_subject, {
            dir <- suppressWarnings(as.integer(input$step_subject))
            ids <- r_cohort()$ids
            if (is.na(dir) || dir == 0L || length(ids) < 2L) return()
            cur <- r_subject()
            at <- if (length(cur) == 1L) match(cur, ids) else NA_integer_
            nxt <- if (is.na(at)) {
              if (dir > 0L) 1L else length(ids)
            } else {
              ((at - 1L + dir) %% length(ids)) + 1L
            }
            r_subject(ids[[nxt]])
          })

          # The profile is by definition a per-patient view. It renders when
          # the incoming dm carries exactly one subject, or when the picker
          # has committed to one of many. Otherwise `single` is FALSE and the
          # chart area shows a placeholder. dm_filter cascades via FKs, so
          # every downstream read of a scoped dm sees the single-patient dm.
          r_scoped_dm <- shiny::reactive({
            dm_obj <- data()
            shiny::req(inherits(dm_obj, "dm"))
            tbls <- dm::dm_get_tables(dm_obj)
            shiny::req("adsl" %in% names(tbls))
            ids <- pp_subject_ids(dm_obj)
            picked <- pp_resolve_subject(ids, r_subject())
            # Reconcile the study's names with the ones the vizs declare
            # against: short prod table names (ae, lb, vs, ...) become ADaM
            # canonical ones, and a SDTM-shaped adsl gains the treatment
            # dates it does not ship. Done AFTER dm_filter so the FK cascade
            # (which keys off the original names) still subject-filters child
            # tables.
            if (!is.na(picked)) {
              list(
                dm     = pp_normalize_dm(
                  dm::dm_filter(dm_obj, adsl = USUBJID == picked)
                ),
                picked = picked,
                total  = length(ids),
                single = TRUE
              )
            } else {
              # Keep the dm unfiltered so the viz sidebar still
              # populates; the chart area shows the placeholder.
              list(
                dm     = pp_normalize_dm(dm_obj),
                picked = NA_character_,
                total  = length(ids),
                single = FALSE
              )
            }
          })

          # The incoming dm with table aliases normalized, unscoped by
          # subject. Data-coverage diagnostics are a property of the tables
          # and columns the study collected, not of which patient is on
          # screen, so they read from here and the header bar stays
          # independent of `r_subject`.
          r_norm_dm <- shiny::reactive({
            dm_obj <- data()
            shiny::req(inherits(dm_obj, "dm"))
            pp_normalize_dm(dm_obj)
          })

          r_cohort_vizs <- shiny::reactive({
            dm_obj <- r_norm_dm()
            c(patient_profile_static_vizs(), pp_findings_vizs(dm_obj))
          })

          # All vizs: static + dynamic findings groups from dm data
          r_all_vizs <- shiny::reactive({
            dm_obj <- r_scoped_dm()$dm
            shiny::req(inherits(dm_obj, "dm"))
            c(patient_profile_static_vizs(), pp_findings_vizs(dm_obj))
          })

          # Available vizs (those whose tables exist in the dm)
          r_available <- shiny::reactive({
            dm_obj <- r_scoped_dm()$dm
            shiny::req(inherits(dm_obj, "dm"))
            tbl_names <- names(dm::dm_get_tables(dm_obj))
            all_vizs <- r_all_vizs()
            Filter(function(v) all(v$tables %in% tbl_names), all_vizs)
          })

          # Selected viz IDs
          r_selected <- shiny::reactiveVal(selected)

          # Per-viz settings
          r_viz_settings <- shiny::reactiveVal(viz_settings)

          # Board-level scale map (NULL when the board has no "scale_map"
          # option). Resolved per render; never stored in block state.
          r_scale_map <- blockr.theme::board_scale_map()

          # Block-level timeline x-axis mode ("date" / "rday")
          r_timeline_mode <- shiny::reactiveVal(timeline_mode)

          # Toggle timeline mode from the gear popover
          shiny::observeEvent(input$timeline_mode, {
            new_mode <- input$timeline_mode
            if (isTRUE(new_mode %in% c("date", "rday"))) {
              r_timeline_mode(new_mode)
            }
          })

          # Initialize selection to patient_overview + first available
          init_done <- shiny::reactiveVal(FALSE)
          shiny::observeEvent(r_available(), {
            if (!init_done()) {
              avail <- r_available()
              cur <- r_selected()
              if (is.null(cur) || !any(cur %in% names(avail))) {
                default_ids <- names(avail)
                # Ensure patient_overview is first if available
                if ("patient_overview" %in% default_ids) {
                  others <- setdiff(default_ids, c("patient_overview",
                                                    "ae_gantt"))
                  r_selected(c("patient_overview", utils::head(others, 2L)))
                } else {
                  r_selected(utils::head(default_ids, 2L))
                }
              }
              # Initialize default settings for all vizs
              settings <- r_viz_settings()
              for (v in avail) {
                if (is.null(settings[[v$id]])) {
                  settings[[v$id]] <- pp_viz_defaults(v)
                }
              }
              r_viz_settings(settings)
              init_done(TRUE)
            }
          })

          # Toggle viz on card click
          shiny::observeEvent(input$toggle_viz, {
            viz_id <- input$toggle_viz
            sel <- r_selected()
            if (viz_id %in% sel) {
              r_selected(setdiff(sel, viz_id))
            } else {
              r_selected(c(sel, viz_id))
            }
          })

          # Reorder vizs via drag-drop
          shiny::observeEvent(input$reorder_viz, {
            new_order <- input$reorder_viz
            if (is.null(new_order)) return()
            if (is.character(new_order) && length(new_order) == 1) {
              new_order <- list(new_order)
            }
            new_order <- as.character(unlist(new_order))
            cur <- r_selected()
            # Validate: must be a permutation of current selection
            if (setequal(new_order, cur)) {
              r_selected(new_order)
            }
          })

          # Sync sidebar toggle state to client whenever selection changes
          shiny::observe({
            sel <- r_selected()
            # Touch r_available so sync fires after sidebar re-renders on
            # data change (not just on selection change)
            r_available()
            session$sendCustomMessage(
              session$ns("sync_selected"), sel
            )
          })

          # Ship the cohort to the client's Blockr.Select. `options` are
          # {value = USUBJID, label = "Placebo · 63F"} pairs: the component
          # renders the value, then the label as a muted
          # `.blockr-select__opt-label`. Only sent when the cohort itself
          # changes, so stepping through patients does not re-ship 2000 rows.
          shiny::observe({
            co <- r_cohort()
            picked <- pp_resolve_subject(co$ids, shiny::isolate(r_subject()))
            opts <- unname(Map(
              function(v, l) list(value = v, label = l),
              co$ids, co$meta
            ))
            session$sendCustomMessage(
              session$ns("subject_picker"),
              list(
                id      = session$ns("pp_subject"),
                options = opts,
                selected = if (is.na(picked)) "" else picked,
                locked  = length(co$ids) <= 1L,
                static  = if (length(co$ids) == 1L) {
                  co$labels[[1L]]
                } else {
                  "No patients"
                }
              )
            )
          })

          # Push a selection the server made (the prev/next steppers, or a
          # cleared stale pick) back into the already-mounted select. Carries
          # no options: the client reuses the ones it has.
          shiny::observe({
            co <- r_cohort()
            picked <- pp_resolve_subject(co$ids, r_subject())
            session$sendCustomMessage(
              session$ns("subject_value"),
              list(
                id = session$ns("pp_subject"),
                selected = if (is.na(picked)) "" else picked
              )
            )
          })

          # Handle viz control changes from client
          shiny::observeEvent(input$viz_ctrl, {
            msg <- input$viz_ctrl
            if (is.null(msg)) return()
            viz_id <- msg$viz_id
            param <- msg$param
            value <- msg$value
            if (is.null(viz_id) || is.null(param)) return()

            settings <- r_viz_settings()
            if (is.null(settings[[viz_id]])) settings[[viz_id]] <- list()
            settings[[viz_id]][[param]] <- value
            r_viz_settings(settings)
          })

          # Shared time range
          r_time_range <- shiny::reactive({
            dm_obj <- r_scoped_dm()$dm
            shiny::req(inherits(dm_obj, "dm"))
            pp_compute_time_range(dm_obj)
          })

          # Reference timestamp (TRTSDT) used for relative-day mode
          r_ref_ms <- shiny::reactive({
            dm_obj <- r_scoped_dm()$dm
            shiny::req(inherits(dm_obj, "dm"))
            pp_compute_ref_ms(dm_obj)
          })

          # Render sidebar cards (re-renders when dm changes)
          output$sidebar_cards <- shiny::renderUI({
            avail <- r_available()
            sel <- shiny::isolate(r_selected())
            if (length(avail) == 0) {
              return(shiny::div(class = "pp-empty-state",
                shiny::p(class = "pp-empty-state-text",
                  "No visualizations available"),
                shiny::p(class = "pp-empty-state-hint",
                  "Check that upstream data contains expected tables")
              ))
            }

            # Helper to build a single card
            build_card <- function(v, is_sel) {
              shiny::div(
                class = paste("pp-card", if (is_sel) "is-selected"),
                `data-viz-id` = v$id,
                `data-domain` = v$domain,
                `data-search-text` = paste(
                  v$label, v$description, v$domain
                ),
                shiny::div(class = "pp-card-main",
                  shiny::div(class = "pp-card-icon",
                    shiny::HTML(pp_icon_html(v$icon, v$color))
                  ),
                  shiny::div(class = "pp-card-content",
                    shiny::tags$p(class = "pp-card-title", v$label),
                    shiny::tags$p(class = "pp-card-description",
                      v$description)
                  ),
                  shiny::div(class = "pp-card-check",
                    shiny::HTML(paste0(
                      '<svg xmlns="http://www.w3.org/2000/svg" ',
                      'width="12" height="12" fill="currentColor" ',
                      'viewBox="0 0 16 16">',
                      '<path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7',
                      'a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 1 1 .708-',
                      '.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 ',
                      '0z"/></svg>'
                    ))
                  )
                )
              )
            }

            # Active section: selected cards in order
            active_ids <- intersect(sel, names(avail))
            active_cards <- lapply(active_ids, function(vid) {
              build_card(avail[[vid]], is_sel = TRUE)
            })

            # Available section: unselected cards grouped by domain
            unsel_vizs <- avail[setdiff(names(avail), sel)]
            domains <- unique(vapply(avail, `[[`, character(1L), "domain"))
            domain_groups <- lapply(domains, function(domain) {
              dvizs <- Filter(function(v) v$domain == domain, unsel_vizs)
              if (length(dvizs) == 0) return(NULL)
              shiny::div(
                class = "pp-category-group",
                `data-domain` = domain,
                shiny::div(class = "pp-category-header",
                  shiny::tags$span(toupper(domain))
                ),
                lapply(dvizs, function(v) build_card(v, is_sel = FALSE))
              )
            })
            domain_groups <- Filter(Negate(is.null), domain_groups)

            shiny::tagList(
              # Active (selected) section
              shiny::div(class = "pp-active-section",
                shiny::div(class = "pp-section-header", "SELECTED"),
                shiny::div(
                  class = paste(
                    "pp-active-hint",
                    if (length(active_cards) < 2) "is-hidden"
                  ),
                  "Drag to reorder"
                ),
                shiny::div(class = "pp-active-list",
                  active_cards,
                  shiny::div(
                    class = paste(
                      "pp-active-empty",
                      if (length(active_cards) > 0) "is-hidden"
                    ),
                    "Click a card below to add it here"
                  )
                )
              ),
              # Available (unselected) section
              shiny::div(class = "pp-available-section",
                shiny::div(
                  class = "pp-section-header pp-section-header-available",
                  "AVAILABLE"
                ),
                domain_groups
              )
            )
          })

          # Build per-viz control toolbar HTML
          pp_controls_ui <- function(viz, viz_id, dm_obj, settings) {
            controls <- viz$controls
            if (is.null(controls) || length(controls) == 0) return(NULL)
            ns <- session$ns
            ctrl_id <- paste0("ctrl_", viz_id)

            tags <- lapply(names(controls), function(param) {
              ctrl <- controls[[param]]
              input_id <- paste0(viz_id, "__", param)
              cur_val <- settings[[param]] %||% ctrl$default

              if (ctrl$type == "checkbox") {
                # Get choices from data
                choices <- ctrl$choices
                if (is.null(choices) && !is.null(ctrl$choices_from)) {
                  tbls <- dm::dm_get_tables(dm_obj)
                  for (tbl_name in viz$tables) {
                    if (tbl_name %in% names(tbls)) {
                      tbl <- as.data.frame(tbls[[tbl_name]])
                      col <- ctrl$choices_from
                      if (col %in% colnames(tbl)) {
                        choices <- sort(unique(as.character(tbl[[col]])))
                        break
                      }
                    }
                  }
                }
                if (is.null(choices)) choices <- character(0)
                if (is.null(cur_val)) cur_val <- choices

                # Build compact multi-select chips
                chips <- lapply(choices, function(ch) {
                  is_active <- ch %in% cur_val
                  shiny::tags$button(
                    class = paste(
                      "pp-ctrl-chip",
                      if (is_active) "is-active"
                    ),
                    `data-viz-id` = viz_id,
                    `data-param` = param,
                    `data-value` = ch,
                    ch
                  )
                })
                shiny::div(class = "pp-ctrl-group",
                  shiny::span(class = "pp-ctrl-label", ctrl$label),
                  shiny::div(class = "pp-ctrl-chips", chips)
                )
              } else if (ctrl$type == "toggle") {
                is_on <- isTRUE(cur_val)
                shiny::div(class = "pp-ctrl-group",
                  shiny::span(class = "pp-ctrl-label", ctrl$label),
                  shiny::tags$button(
                    class = paste(
                      "pp-ctrl-toggle",
                      if (is_on) "is-on"
                    ),
                    `data-viz-id` = viz_id,
                    `data-param` = param,
                    shiny::span(class = "pp-ctrl-toggle-track",
                      shiny::span(class = "pp-ctrl-toggle-thumb")
                    )
                  )
                )
              } else if (ctrl$type == "radio") {
                choices <- ctrl$choices
                if (is.null(choices)) choices <- character(0)
                if (is.null(cur_val)) cur_val <- choices[1]
                choice_names <- names(choices) %||% choices

                btns <- lapply(seq_along(choices), function(ci) {
                  is_active <- choices[ci] == cur_val
                  shiny::tags$button(
                    class = paste(
                      "pp-ctrl-radio",
                      if (is_active) "is-active"
                    ),
                    `data-viz-id` = viz_id,
                    `data-param` = param,
                    `data-value` = choices[ci],
                    choice_names[ci]
                  )
                })
                shiny::div(class = "pp-ctrl-group",
                  shiny::span(class = "pp-ctrl-label", ctrl$label),
                  shiny::div(class = "pp-ctrl-radios", btns)
                )
              } else {
                NULL
              }
            })
            tags <- Filter(Negate(is.null), tags)
            if (length(tags) == 0) return(NULL)
            shiny::div(class = "pp-chart-controls", tags)
          }

          # Header bar (subject picker + gear popover) — depends on the
          # cohort, NOT on r_timeline_mode or r_subject. This keeps either
          # popover from being rebuilt (and closing) when the user flips the
          # timeline toggle or picks a patient. Both button labels are kept
          # in sync by optimistic JS plus a confirming custom message.
          output$header_bar <- shiny::renderUI({
            co <- r_cohort()
            ns <- session$ns
            init_mode <- shiny::isolate(r_timeline_mode())
            # Whether relative-day mode is possible at all is a property of
            # the study (does ADSL carry TRTSDT), not of the patient on
            # screen. Read it from the unscoped dm: routing through
            # `r_ref_ms()` would make the header depend on `r_subject`, and
            # every pick would rebuild the header and slam both popovers shut.
            gear_disabled <- is.na(pp_compute_ref_ms(r_norm_dm()))


            gear_tag <- shiny::div(
              class = "pp-gear-wrap",
              shiny::tags$button(
                class = "pp-gear-btn",
                id = ns("pp_gear_btn"),
                type = "button",
                title = "Block settings",
                shiny::HTML(paste0(
                  '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
                  'height="16" fill="currentColor" viewBox="0 0 16 16">',
                  '<path d="M8 4.754a3.246 3.246 0 1 0 0 6.492 3.246 ',
                  '3.246 0 0 0 0-6.492M5.754 8a2.246 2.246 0 1 1 4.492 ',
                  '0 2.246 2.246 0 0 1-4.492 0"/>',
                  '<path d="M9.796 1.343c-.527-1.79-3.065-1.79-3.592 ',
                  '0l-.094.319a.873.873 0 0 1-1.255.52l-.292-.16c-1.64-',
                  '.892-3.433.901-2.54 2.541l.159.292a.873.873 0 0 1-.52 ',
                  '1.255l-.319.094c-1.79.527-1.79 3.065 0 3.592l.319.094a',
                  '.873.873 0 0 1 .52 1.255l-.16.292c-.892 1.64.901 3.434 ',
                  '2.541 2.541l.292-.159a.873.873 0 0 1 1.255.52l.094.319c',
                  '.527 1.79 3.065 1.79 3.592 0l.094-.319a.873.873 0 0 1 ',
                  '1.255-.52l.292.16c1.64.893 3.434-.902 2.541-2.541l-.159',
                  '-.292a.873.873 0 0 1 .52-1.255l.319-.094c1.79-.527 ',
                  '1.79-3.065 0-3.592l-.319-.094a.873.873 0 0 1-.52-1.255',
                  'l.16-.292c.892-1.64-.902-3.433-2.541-2.54l-.292.159a',
                  '.873.873 0 0 1-1.255-.52zm-2.633.283c.246-.835 1.428-',
                  '.835 1.674 0l.094.319a1.873 1.873 0 0 0 2.693 1.115l',
                  '.291-.16c.764-.415 1.6.42 1.184 1.185l-.159.292a1.873 ',
                  '1.873 0 0 0 1.116 2.692l.318.094c.835.246.835 1.428 0 ',
                  '1.674l-.319.094a1.873 1.873 0 0 0-1.115 2.693l.16.291c',
                  '.415.764-.42 1.6-1.185 1.184l-.291-.159a1.873 1.873 0 ',
                  '0 0-2.693 1.116l-.094.318c-.246.835-1.428.835-1.674 ',
                  '0l-.094-.319a1.873 1.873 0 0 0-2.692-1.115l-.292.16c-',
                  '.764.415-1.6-.42-1.184-1.185l.159-.291A1.873 1.873 0 ',
                  '0 0 1.945 8.93l-.319-.094c-.835-.246-.835-1.428 0-',
                  '1.674l.319-.094A1.873 1.873 0 0 0 3.06 4.377l-.16-',
                  '.292c-.415-.764.42-1.6 1.185-1.184l.292.159a1.873 ',
                  '1.873 0 0 0 2.692-1.115z"/></svg>'
                ))
              ),
              shiny::div(
                class = "pp-gear-popover",
                id = ns("pp_gear_popover"),
                shiny::div(class = "pp-popover-row",
                  shiny::span(class = "pp-popover-label", "Timeline"),
                  shiny::tags$button(
                    class = paste(
                      "pp-popover-toggle",
                      if (gear_disabled) "is-disabled"
                    ),
                    id = ns("pp_tl_toggle"),
                    `data-tl-mode` = init_mode,
                    `data-disabled` = if (gear_disabled) "1" else NULL,
                    type = "button",
                    title = if (gear_disabled) {
                      "TRTSDT not available \u2014 relative day disabled"
                    } else {
                      "Click to switch"
                    },
                    if (identical(init_mode, "rday")) {
                      "Relative day"
                    } else {
                      "Date"
                    }
                  )
                ),
                # Data coverage: visuals that can't render for this data,
                # with the reason (missing table or required column). Lets
                # users see what's collected without each one having to be
                # selected first. Hidden behind the gear, not permanent.
                {
                  cov <- pp_coverage_report(r_norm_dm(), r_cohort_vizs())
                  shiny::tagList(
                    shiny::div(class = "pp-popover-divider"),
                    shiny::div(class = "pp-popover-section-label",
                      "Data coverage"),
                    if (length(cov) == 0L) {
                      shiny::div(class = "pp-coverage-ok",
                        "All visuals available")
                    } else {
                      lapply(cov, function(c) {
                        shiny::div(class = "pp-coverage-item",
                          shiny::span(class = "pp-coverage-label", c$label),
                          shiny::span(class = "pp-coverage-reason", c$reason)
                        )
                      })
                    }
                  )
                }
              )
            )

            # The header row now carries only the gear. The subject picker
            # lives in the static UI (see `ui=` below) so its Blockr.Select
            # container is present before the mount message arrives.
            shiny::div(
              class = paste(
                "pp-cohort-hint d-flex justify-content-end",
                "align-items-center"
              ),
              gear_tag
            )
          })

          # Render chart list (depends on selected, settings, time range,
          # and timeline mode; rebuilds on every change — but the header
          # bar above is independent so the popover stays open).
          output$chart_area <- shiny::renderUI({
            scoped <- r_scoped_dm()
            dm_obj <- scoped$dm
            shiny::req(inherits(dm_obj, "dm"))
            # The profile needs exactly one patient. Until the header picker
            # or an upstream drill-down commits to one, show an info
            # placeholder rather than auto-picking the first subject.
            if (!isTRUE(scoped$single)) {
              return(shiny::div(class = "pp-empty-state",
                shiny::div(class = "pp-empty-state-icon",
                  shiny::HTML(paste0(
                    '<svg xmlns="http://www.w3.org/2000/svg" width="40" ',
                    'height="40" fill="currentColor" viewBox="0 0 16 16">',
                    '<path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6m2-3a2 2 0 ',
                    '1 1-4 0 2 2 0 0 1 4 0m4 8c0 1-1 1-1 1H3s-1 0-1-1 ',
                    '1-4 6-4 6 3 6 4m-1-.004c-.001-.246-.154-.986-.832',
                    '-1.664C11.516 10.68 10.289 10 8 10c-2.29 0-3.516 ',
                    '.68-4.168 1.332-.678.678-.83 1.418-.832 ',
                    '1.664z"/></svg>'
                  ))
                ),
                shiny::p(class = "pp-empty-state-text",
                  "No patient selected"),
                shiny::p(class = "pp-empty-state-hint",
                  if (isTRUE(scoped$total > 1L)) {
                    paste0("Pick one of ", scoped$total,
                           " patients above, or drill down on a chart")
                  } else {
                    "No patient data in the incoming tables"
                  })
              ))
            }
            time_range <- r_time_range()
            shiny::req(time_range)
            sel <- r_selected()
            avail <- r_available()
            all_settings <- r_viz_settings()
            ref_ms <- r_ref_ms()
            tl_mode <- r_timeline_mode()
            # Relative-day mode requires a reference timestamp; if TRTSDT
            # isn't available, silently fall back to date mode rather than
            # rendering an empty/value axis.
            if (identical(tl_mode, "rday") && is.na(ref_ms)) {
              tl_mode <- "date"
            }

            # Keep only selected vizs that are available
            active_ids <- intersect(sel, names(avail))
            if (length(active_ids) == 0) {
              return(shiny::div(class = "pp-empty-state",
                shiny::div(class = "pp-empty-state-icon",
                  shiny::HTML(paste0(
                    '<svg xmlns="http://www.w3.org/2000/svg" width="40" ',
                    'height="40" fill="currentColor" viewBox="0 0 16 16">',
                    '<path d="M14 1a1 1 0 0 1 1 1v12a1 1 0 0 1-1 1H2a1 ',
                    '1 0 0 1-1-1V2a1 1 0 0 1 1-1zM2 0a2 2 0 0 0-2 ',
                    '2v12a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V2a2 2 0 0 ',
                    '0-2-2z"/>',
                    '<path d="M6.854 4.646a.5.5 0 0 1 0 .708L4.207 ',
                    '8l2.647 2.646a.5.5 0 0 1-.708.708l-3-3a.5.5 0 0 ',
                    '1 0-.708l3-3a.5.5 0 0 1 .708 0zm2.292 0a.5.5 0 0 ',
                    '0 0 .708L11.793 8l-2.647 2.646a.5.5 0 0 0 .708',
                    '.708l3-3a.5.5 0 0 0 0-.708l-3-3a.5.5 0 0 0-.708 ',
                    '0z"/></svg>'
                  ))
                ),
                shiny::p(class = "pp-empty-state-text",
                  "No visualizations selected"),
                shiny::p(class = "pp-empty-state-hint",
                  "Click cards in the sidebar to add charts")
              ))
            }

            # Board scale map: resolve AE severity colors once and inject
            # them as a render-time setting for the severity-colored vizs
            # (not persisted -- r_viz_settings is untouched). Falls back to
            # each viz's own constants when no map / no binding is present.
            sev_colors <- pp_sev_scale_colors(r_scale_map(), dm_obj)

            chart_tags <- lapply(active_ids, function(viz_id) {
              viz <- avail[[viz_id]]
              viz_settings <- all_settings[[viz_id]] %||% list()
              if (viz_id %in% c("ae_gantt", "patient_overview") &&
                    !is.null(sev_colors)) {
                viz_settings$sev_colors <- sev_colors
              }
              if (identical(viz_id, "patient_overview")) {
                viz_settings$arm_var <- r_arm_var()
              }

              # Resolve declared `requires` / `optional` column dependencies.
              # If a required column (or any alias) is missing, render a
              # pp_empty_chart message instead of calling the viz renderer.
              resolved <- pp_resolve_requires(dm_obj, viz)
              chart <- if (!isTRUE(resolved$ok)) {
                pp_empty_chart(resolved$msg)
              } else {
                tryCatch(
                  viz$render(resolved$dm, time_range, viz_settings,
                             ref_ms, tl_mode),
                  error = function(e) pp_empty_chart(
                    paste("Error:", conditionMessage(e))
                  )
                )
              }

              # Build controls toolbar
              controls_ui <- pp_controls_ui(viz, viz_id, dm_obj, viz_settings)

              is_treatment <- viz_id == "patient_overview"
              panel_class <- if (is_treatment) {
                "pp-chart-panel pp-treatment-strip"
              } else {
                "pp-chart-panel"
              }

              shiny::div(class = panel_class,
                shiny::div(class = "pp-chart-header",
                  shiny::div(class = "pp-chart-title", viz$label),
                  controls_ui,
                  shiny::div(class = "pp-chart-domain", viz$domain)
                ),
                shiny::div(class = "pp-chart-body", chart)
              )
            })

            shiny::tagList(chart_tags)
          })

          list(
            # Unconfigured, the block passes the cohort straight through.
            # Once a patient is picked it filters, so a downstream block can
            # never show 254 people while the charts show one. The subject is
            # re-checked against the live cohort here rather than trusted
            # from state: a stale id would otherwise filter to zero rows for
            # the instant before the stale-selection guard fires.
            expr = shiny::reactive({
              sel <- r_subject()
              ids <- pp_subject_ids(data())
              if (length(sel) != 1L || !sel %in% ids) {
                return(quote(identity(data)))
              }
              bquote(
                dm::dm_filter(data, adsl = USUBJID == .(subject)),
                list(subject = sel)
              )
            }),
            state = list(
              selected = r_selected,
              viz_settings = r_viz_settings,
              timeline_mode = r_timeline_mode,
              subject = r_subject,
              arm_var = r_arm_var
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::tags$link(
          rel = "stylesheet",
          href = "blockr-pharma/css/patient-profile.css"
        ),
        # Blockr.Select: the shared single-select primitive. Its dropdown is
        # portalled to <body>, which is what lets it escape `.pp-chart-area`'s
        # `overflow-y: auto` — a hand-rolled absolute popover gets clipped and
        # scrolls away with the chart list. blockr_blocks_css_dep() carries the
        # canonical `.blockr-field--required-empty` amber cue.
        blockr.dplyr::blockr_blocks_css_dep(),
        blockr.dplyr::blockr_select_dep(),
        shiny::div(
          class = "pp-layout", id = ns("pp_layout"),

          # Left sidebar
          shiny::div(
            class = "pp-sidebar", id = ns("pp_sidebar"),

            # Header
            shiny::div(class = "pp-sidebar-header",
              shiny::tags$h3(class = "pp-sidebar-title", "Visualizations"),
              shiny::tags$button(
                class = "pp-pin-btn",
                id = ns("pin_btn"),
                title = "Toggle sidebar",
                shiny::HTML(paste0(
                  '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
                  'height="16" fill="currentColor" viewBox="0 0 16 16">',
                  '<path d="M4.146.146A.5.5 0 0 1 4.5 0h7a.5.5 0 0 1 ',
                  '.5.5c0 .68-.342 1.174-.646 1.479-.126.125-.25.224-',
                  '.354.298v4.431l.078.048c.203.127.476.314.751.555',
                  'C12.36 7.775 13 8.527 13 9.5a.5.5 0 0 1-.5.5h-4v4.5',
                  'a.5.5 0 0 1-1 0V10h-4A.5.5 0 0 1 3 9.5c0-.973.64-',
                  '1.725 1.17-2.189A6 6 0 0 1 5 6.708V2.277a3 3 0 0 ',
                  '1-.354-.298C4.342 1.674 4 1.179 4 .5a.5.5 0 0 1 ',
                  '.146-.354z"/></svg>'
                ))
              )
            ),

            # Search
            shiny::div(class = "pp-sidebar-search",
              shiny::div(class = "pp-sidebar-search-wrapper",
                shiny::span(class = "pp-sidebar-search-icon",
                  shiny::HTML(paste0(
                    '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
                    'height="16" fill="currentColor" viewBox="0 0 16 16">',
                    '<path d="M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h',
                    '-.001q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-',
                    '1.414l-3.85-3.85a1 1 0 0 0-.115-.1zM12 6.5a5.5 ',
                    '5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z"/></svg>'
                  ))
                ),
                shiny::tags$input(
                  type = "text",
                  class = "pp-sidebar-search-input",
                  id = ns("search"),
                  placeholder = "Search visualizations..."
                )
              )
            ),

            # Card list
            shiny::div(class = "pp-sidebar-content",
              shiny::uiOutput(ns("sidebar_cards"))
            )
          ),

          # Expand button (shown when sidebar collapsed)
          shiny::tags$button(
            class = "pp-expand-btn",
            id = ns("expand_btn"),
            title = "Show sidebar",
            shiny::HTML(paste0(
              '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
              'height="16" fill="currentColor" viewBox="0 0 16 16">',
              '<path fill-rule="evenodd" d="M1 8a.5.5 0 0 1 .5-.5h11',
              '.793l-3.147-3.146a.5.5 0 0 1 .708-.708l4 4a.5.5 0 0 ',
              '1 0 .708l-4 4a.5.5 0 0 1-.708-.708L13.293 8.5H1.5A.5',
              '.5 0 0 1 1 8z"/></svg>'
            ))
          ),

          # Chart area: a static subject picker, the dynamic header bar (gear
          # popover) and the dynamic chart list. The picker is static so its
          # Blockr.Select container exists before the mount message lands, and
          # so that stepping through patients never rebuilds it. The header bar
          # is split off so flipping r_timeline_mode only invalidates
          # chart_area and the gear popover stays open.
          shiny::div(class = "pp-chart-area",
            shiny::div(class = "pp-chart-toolbar",
              # Required-empty: the control carries the amber cue and nothing
              # else. No help line — the "Select a patient" placeholder is
              # already the message, and no banner: this is attention, not
              # error. Steppers flank the select and never move, because the
              # control is a fixed width, so a long arm name ellipsizes rather
              # than shunting the arrows sideways as you page through patients.
              shiny::div(class = "pp-subject-picker", id = ns("pp_picker"),
                shiny::tags$button(
                  class = "pp-subject-step",
                  id = ns("pp_subject_prev"),
                  type = "button",
                  `data-dir` = "-1",
                  title = "Previous patient",
                  shiny::HTML("&lsaquo;")
                ),
                shiny::div(class = "pp-subject-select", id = ns("pp_subject")),
                shiny::span(class = "pp-subject-static",
                            id = ns("pp_subject_static")),
                shiny::tags$button(
                  class = "pp-subject-step",
                  id = ns("pp_subject_next"),
                  type = "button",
                  `data-dir` = "1",
                  title = "Next patient",
                  shiny::HTML("&rsaquo;")
                )
              ),
              shiny::uiOutput(ns("header_bar"))
            ),
            shiny::uiOutput(ns("chart_area"))
          )
        ),

        # Client-side JS for sidebar interactions + viz controls
        # Use paste0 instead of sprintf to avoid 8192-char format limit
        shiny::tags$script(shiny::HTML(paste0("
          $(function() {
            var layoutId = '", ns("pp_layout"), "';
            var sidebarId = '", ns("pp_sidebar"), "';
            var searchId = '", ns("search"), "';
            var pinBtnId = '", ns("pin_btn"), "';
            var expandBtnId = '", ns("expand_btn"), "';
            var toggleInputId = '", ns("toggle_viz"), "';
            var ctrlInputId = '", ns("viz_ctrl"), "';
            var syncMsgId = '", ns("sync_selected"), "';
            var reorderInputId = '", ns("reorder_viz"), "';

            var dragActive = false;
            var tlModeInputId = '", ns("timeline_mode"), "';
            var gearBtnId = '", ns("pp_gear_btn"), "';
            var gearPopoverId = '", ns("pp_gear_popover"), "';

            var pickerId = '", ns("pp_picker"), "';
            var subjectContainerId = '", ns("pp_subject"), "';
            var subjectStaticId = '", ns("pp_subject_static"), "';
            var stepSubjectInputId = '", ns("step_subject"), "';
            var subjectPickerMsgId = '", ns("subject_picker"), "';
            var subjectValueMsgId = '", ns("subject_value"), "';

            // Required-empty amber cue on the control itself.
            // `.blockr-field--required-empty` is the canonical class from
            // blockr-blocks.css; it paints the .blockr-select__control of the
            // --bordered variant. A locked cohort is never 'empty'.
            function syncRequiredEmpty(locked) {
              var root = document.getElementById(subjectContainerId);
              var empty = !locked && !!root && !!root._ppPicker &&
                          root._ppPicker.getValue() === '';
              $('#' + subjectContainerId)
                .toggleClass('blockr-field--required-empty', empty);
            }

            // Mount Blockr.Select once, then setOptions() on later messages ", "\u2014", "
            // the same lifecycle blockr.dm's table picker uses. `allowEmpty`
            // is what keeps '' alive across setOptions(): without it the
            // component slides the selection onto the first patient whenever
            // the cohort changes, which is exactly the silent auto-pick this
            // block exists to avoid.
            Shiny.addCustomMessageHandler(subjectPickerMsgId, function(msg) {
              if (!msg) return;
              var root = document.getElementById(msg.id);
              if (!root) return;
              var opts = Array.isArray(msg.options) ? msg.options : [];

              // Cache the option list: setOptions(null, ...) would clear it,
              // so the value-only message below has to hand it back verbatim.
              root._ppOptions = opts;

              if (!root._ppPicker) {
                root._ppPicker = Blockr.Select.single(root, {
                  options: opts,
                  selected: msg.selected || '',
                  allowEmpty: true,
                  placeholder: 'Select a patient',
                  onChange: function(value) {
                    Shiny.setInputValue(msg.id, value, {priority: 'event'});
                    syncRequiredEmpty($('#' + pickerId).hasClass('is-locked'));
                  }
                });
                // Standalone control, so the bordered 42px variant, as in
                // blockr.dm's table picker. The amber cue paints this border.
                root._ppPicker.el.classList.add('blockr-select--bordered');
              } else {
                root._ppPicker.setOptions(opts, msg.selected || '');
              }

              // Zero or one subject: nothing to choose. Show a plain label
              // instead of a dropdown that affords a choice which does not
              // exist, and hide the steppers with it.
              var $picker = $('#' + pickerId);
              $picker.toggleClass('is-locked', !!msg.locked);
              $('#' + subjectStaticId).text(msg.locked ? (msg.static || '') : '');
              syncRequiredEmpty(!!msg.locked);
            });

            // A selection the server made (steppers, or a cleared stale pick).
            // Carries no options; reuse the ones the component already holds.
            Shiny.addCustomMessageHandler(subjectValueMsgId, function(msg) {
              if (!msg) return;
              var root = document.getElementById(msg.id);
              if (!root || !root._ppPicker) return;
              if (root._ppPicker.getValue() === (msg.selected || '')) return;
              root._ppPicker.setOptions(root._ppOptions || [], msg.selected || '');
              syncRequiredEmpty($('#' + pickerId).hasClass('is-locked'));
            });

            // Prev / next patient
            $(document).on('click', '#' + pickerId + ' .pp-subject-step',
              function(e) {
                e.stopPropagation();
                var dir = parseInt($(this).attr('data-dir'), 10);
                if (!dir) return;
                Shiny.setInputValue(stepSubjectInputId, dir, {priority: 'event'});
              });

            // Toggle gear popover open/close
            $(document).on('click', '#' + gearBtnId, function(e) {
              e.stopPropagation();
              var popover = document.getElementById(gearPopoverId);
              if (popover) popover.classList.toggle('is-open');
              $(this).toggleClass('is-active');
            });

            // Close popover when clicking outside it
            $(document).on('click', function(e) {
              var $btn = $('#' + gearBtnId);
              var $pop = $('#' + gearPopoverId);
              if (!$btn.length || !$pop.length) return;
              if (!$btn.is(e.target) && !$btn.has(e.target).length &&
                  !$pop.is(e.target) && !$pop.has(e.target).length) {
                $pop.removeClass('is-open');
                $btn.removeClass('is-active');
              }
            });

            // Timeline mode click-through: flip current value on click.
            // Read/write via attr(), not data() ", "\u2014", " jQuery's .data() caches
            // the initial attribute value and ignores later attr() writes,
            // so subsequent clicks would always read the original mode.
            $(document).on('click', '#' + layoutId + ' .pp-popover-toggle', function(e) {
              e.stopPropagation();
              if ($(this).attr('data-disabled') === '1') return;
              var cur = $(this).attr('data-tl-mode');
              var next = (cur === 'rday') ? 'date' : 'rday';
              // Optimistic UI update; server re-render will confirm.
              $(this).text(next === 'rday' ? 'Relative day' : 'Date');
              $(this).attr('data-tl-mode', next);
              Shiny.setInputValue(tlModeInputId, next, {priority: 'event'});
            });

            // Card click: toggle selection (server-driven, no optimistic toggle)
            $(document).on('click', '#' + layoutId + ' .pp-card', function(e) {
              if (dragActive) return;
              var vizId = $(this).data('viz-id');
              if (!vizId) return;
              Shiny.setInputValue(toggleInputId, vizId, {priority: 'event'});
            });

            // Search: client-side filtering across both sections
            $(document).on('input', '#' + searchId, function() {
              var query = $(this).val().toLowerCase();
              var sidebar = $(this).closest('.pp-sidebar');
              sidebar.find('.pp-card').each(function() {
                var text = ($(this).data('search-text') || '').toLowerCase();
                $(this).toggle(!query || text.indexOf(query) >= 0);
              });
              sidebar.find('.pp-category-group').each(function() {
                var hasVisible = $(this).find('.pp-card:visible').length > 0;
                $(this).toggle(hasVisible);
              });
              sidebar.find('.pp-active-section').each(function() {
                var hasVisible = $(this).find('.pp-card:visible').length > 0;
                $(this).toggle(hasVisible);
              });
              sidebar.find('.pp-available-section').each(function() {
                var hasVisible = $(this).find('.pp-card:visible').length > 0;
                $(this).toggle(hasVisible);
              });
            });

            // Pin/unpin sidebar
            $(document).on('click', '#' + pinBtnId, function() {
              var sidebar = document.getElementById(sidebarId);
              var layout = document.getElementById(layoutId);
              sidebar.classList.toggle('collapsed');
              layout.classList.toggle('sidebar-collapsed');
              $(this).toggleClass('is-unpinned');
            });

            // Expand button
            $(document).on('click', '#' + expandBtnId, function() {
              var sidebar = document.getElementById(sidebarId);
              var layout = document.getElementById(layoutId);
              sidebar.classList.remove('collapsed');
              layout.classList.remove('sidebar-collapsed');
              $('#' + pinBtnId).removeClass('is-unpinned');
            });

            // Chip click (checkbox controls)
            $(document).on('click', '#' + layoutId + ' .pp-ctrl-chip', function(e) {
              e.stopPropagation();
              $(this).toggleClass('is-active');
              var vizId = $(this).data('viz-id');
              var param = $(this).data('param');
              var active = [];
              $(this).closest('.pp-ctrl-chips').find('.pp-ctrl-chip.is-active').each(function() {
                active.push($(this).data('value'));
              });
              Shiny.setInputValue(ctrlInputId, {
                viz_id: vizId, param: param, value: active
              }, {priority: 'event'});
            });

            // Toggle click
            $(document).on('click', '#' + layoutId + ' .pp-ctrl-toggle', function(e) {
              e.stopPropagation();
              $(this).toggleClass('is-on');
              var vizId = $(this).data('viz-id');
              var param = $(this).data('param');
              var isOn = $(this).hasClass('is-on');
              Shiny.setInputValue(ctrlInputId, {
                viz_id: vizId, param: param, value: isOn
              }, {priority: 'event'});
            });

            // Radio click
            $(document).on('click', '#' + layoutId + ' .pp-ctrl-radio', function(e) {
              e.stopPropagation();
              $(this).siblings('.pp-ctrl-radio').removeClass('is-active');
              $(this).addClass('is-active');
              var vizId = $(this).data('viz-id');
              var param = $(this).data('param');
              var value = $(this).data('value');
              Shiny.setInputValue(ctrlInputId, {
                viz_id: vizId, param: param, value: value
              }, {priority: 'event'});
            });

            // Sync sidebar state from server
            Shiny.addCustomMessageHandler(syncMsgId, function(selected) {
              if (!selected) selected = [];
              if (typeof selected === 'string') selected = [selected];

              var $layout = $('#' + layoutId);
              var $activeList = $layout.find('.pp-active-list');
              var $availSection = $layout.find('.pp-available-section');

              // Build index of all cards
              var cardMap = {};
              $layout.find('.pp-card').each(function() {
                var vid = $(this).data('viz-id');
                if (vid) cardMap[vid] = $(this);
              });

              // Move selected cards to active list in order
              var $hint = $activeList.find('.pp-active-empty');
              for (var i = 0; i < selected.length; i++) {
                var $card = cardMap[selected[i]];
                if ($card && $card.length) {
                  $card.addClass('is-selected').attr('draggable', 'true');
                  $hint.before($card);
                }
              }

              // Move unselected cards back to their domain group
              Object.keys(cardMap).forEach(function(vid) {
                if (selected.indexOf(vid) >= 0) return;
                var $card = cardMap[vid];
                $card.removeClass('is-selected').removeAttr('draggable');
                var domain = $card.data('domain');
                var $group = $availSection
                  .find('.pp-category-group[data-domain=' + JSON.stringify(domain) + ']');
                if (!$group.length) {
                  $group = $('<div class=pp-category-group data-domain=' +
                    JSON.stringify(domain) + '>' +
                    '<div class=pp-category-header><span>' +
                    domain.toUpperCase() + '</span></div></div>');
                  $availSection.append($group);
                }
                $group.append($card);
              });

              // Toggle empty hint and drag-to-reorder hint
              if (selected.length > 0) {
                $hint.addClass('is-hidden');
              } else {
                $hint.removeClass('is-hidden');
              }
              var $dragHint = $layout.find('.pp-active-hint');
              if (selected.length >= 2) {
                $dragHint.removeClass('is-hidden');
              } else {
                $dragHint.addClass('is-hidden');
              }

              // Hide empty domain groups, show non-empty ones
              $availSection.find('.pp-category-group').each(function() {
                var hasCards = $(this).find('.pp-card').length > 0;
                $(this).toggle(hasCards);
              });
            });

            // --- HTML5 Drag and Drop on active list ---
            var $doc = $(document);

            $doc.on('dragstart', '#' + layoutId + ' .pp-active-list .pp-card', function(e) {
              dragActive = true;
              $(this).addClass('is-dragging');
              e.originalEvent.dataTransfer.effectAllowed = 'move';
              e.originalEvent.dataTransfer.setData('text/plain', $(this).data('viz-id'));
            });

            $doc.on('dragover', '#' + layoutId + ' .pp-active-list .pp-card', function(e) {
              if (!dragActive) return;
              e.preventDefault();
              e.originalEvent.dataTransfer.dropEffect = 'move';
              var rect = this.getBoundingClientRect();
              var midY = rect.top + rect.height / 2;
              if (e.originalEvent.clientY < midY) {
                $(this).addClass('drop-above').removeClass('drop-below');
              } else {
                $(this).addClass('drop-below').removeClass('drop-above');
              }
            });

            $doc.on('dragleave', '#' + layoutId + ' .pp-active-list .pp-card', function(e) {
              $(this).removeClass('drop-above drop-below');
            });

            $doc.on('drop', '#' + layoutId + ' .pp-active-list .pp-card', function(e) {
              e.preventDefault();
              var draggedId = e.originalEvent.dataTransfer.getData('text/plain');
              var $target = $(this);
              var targetId = $target.data('viz-id');
              $target.removeClass('drop-above drop-below');

              if (draggedId === targetId) return;

              var $dragged = $target.closest('.pp-active-list')
                .find('.pp-card[data-viz-id=' + JSON.stringify(draggedId) + ']');
              if (!$dragged.length) return;

              // Determine insertion position
              var rect = this.getBoundingClientRect();
              var midY = rect.top + rect.height / 2;
              if (e.originalEvent.clientY < midY) {
                $dragged.insertBefore($target);
              } else {
                $dragged.insertAfter($target);
              }

              // Read new order and send to server
              var newOrder = [];
              $dragged.closest('.pp-active-list').find('.pp-card').each(function() {
                newOrder.push($(this).data('viz-id'));
              });
              Shiny.setInputValue(reorderInputId, newOrder, {priority: 'event'});
            });

            $doc.on('dragend', '#' + layoutId + ' .pp-active-list .pp-card', function(e) {
              $(this).removeClass('is-dragging');
              $(this).closest('.pp-active-list')
                .find('.pp-card').removeClass('drop-above drop-below');
              setTimeout(function() { dragActive = false; }, 0);
            });

            $doc.on('dragover', '#' + layoutId + ' .pp-active-list', function(e) {
              if (!dragActive) return;
              e.preventDefault();
            });
          });
        ")))
      )
    },
    dat_valid = function(data) {
      if (!inherits(data, "dm")) {
        stop("Input must be a dm object")
      }
    },
    # `selected` may legitimately be empty (no viz chosen, or no single
    # patient yet) — the UI shows a grey placeholder, not an error. Same for
    # `subject`: the stale-selection guard clears it whenever the picked
    # patient leaves the cohort, and clearing a field that is not listed
    # here wedges the block.
    allow_empty_state = c("selected", "viz_settings", "subject"),
    external_ctrl = c("selected", "viz_settings", "timeline_mode", "subject"),
    class = c("patient_profile_block", "dm_block"),
    ...
  )
}

#' @rdname new_patient_profile_block
#' @param id Module ID
#' @param x Block object
#' @importFrom blockr.core block_ui
#' @method block_ui patient_profile_block
#' @export
block_ui.patient_profile_block <- function(id, x, ...) {
  # Emit a real (but empty) output container so Shiny registers the
  # `-result_hidden` clientData binding. Without it, lazy-eval in
  # blockr.core sees the block as "hidden" and suspends its entire
  # upstream chain — leaving the custom UI with no data.
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "result"))
  )
}

#' @rdname new_patient_profile_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @importFrom blockr.core block_output
#' @method block_output patient_profile_block
#' @export
block_output.patient_profile_block <- function(x, result, session) {
  # Render a zero-height sentinel: this keeps the `-result` output
  # bound on the client (so `-result_hidden` clientData stays up to
  # date) without showing anything. All visible content is produced
  # by the expression UI.
  shiny::renderUI(
    shiny::tags$span(style = "display:none", "patient_profile")
  )
}
