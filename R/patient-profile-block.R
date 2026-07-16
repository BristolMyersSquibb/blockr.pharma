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
#' @param show_prestudy Show the full pre-treatment history? By default the
#'   timeline starts 30 days before treatment start (the screening window,
#'   so baselines stay visible) -- one medication started years earlier must
#'   not stretch every axis to it. `TRUE` restores the full range; also a
#'   toggle in the gear popover.
#' @param subject USUBJID to display, as a length-1 character. Only meaningful
#'   when the incoming dm carries more than one subject; a single-subject dm
#'   always renders its one subject. Ignored (and cleared) when the value is
#'   absent from the incoming cohort. Defaults to `NULL`, i.e. no patient
#'   chosen.
#' @details
#' The ADSL column holding the treatment / arm label is study-level
#' configuration, not block state: it is the arm field of the
#' `"study_roles"` *board option* (see [new_study_roles_option()], which
#' also declares the severity column, the timeline reference and table
#' aliases), declared once per study in the board's settings sidebar and
#' serialized with the board. It is used by both the subject picker and the
#' treatment lane, so the two cannot disagree. Undeclared, the column is
#' `ACTARM`; a declared column that the data does not carry is a named
#' error, never a fallback. The legacy app-level option
#' `options(blockr.pharma_arm_var =)` (or `BLOCKR_PHARMA_ARM_VAR`) is still
#' honored on boards that have not declared an arm, for the migration only.
#' A legacy `arm_var` constructor argument (from boards saved before the
#' option existed) is ignored with a warning.
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
                                              show_prestudy = FALSE,
                                              ...) {
  timeline_mode <- match.arg(timeline_mode, c("rday", "date"))
  subject <- pp_validate_subject(subject)
  stopifnot(isTRUE(show_prestudy) || isFALSE(show_prestudy))

  # `arm_var` is study-level configuration, not block state: it is the
  # "arm_var" BOARD option (read reactively in the server below), never
  # persisted with the block, and deliberately NOT a constructor formal --
  # core requires every formal to round-trip through `state`, which would
  # make it saved-board state and put it one step from user control (the AI
  # surface is every non-dots formal). Boards saved before this change carry
  # arm_var in their serialized state; on restore it lands in `...` and
  # new_block() stores it as an inert attribute, so old boards still load --
  # warn so the silently-dropped setting is at least visible.
  if ("arm_var" %in% names(list(...))) {
    warning(
      "new_patient_profile_block(arm_var=) is ignored: declare the study's ",
      "arm column in the board sidebar (Study > Arm column) instead.",
      call. = FALSE
    )
  }

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
          # De-duplicated input dm. The board re-emits the SAME dm two or three
          # times on a cold start: the block re-evaluates as the dock's
          # visibility handshake settles (pending -> required -> rendered) and
          # blockr.core does not compare a block's data by value, so each
          # re-evaluation looks like a change. Everything here derives from
          # `data()`, so the whole chart area was rebuilt -- every echarts
          # container destroyed and recreated -- three times on startup, for a
          # dm that is `identical()` each time.
          #
          # reactiveVal skips invalidation when the new value is identical to
          # the current one, so funnelling the input through it makes a
          # re-emitted dm cost nothing. Read `r_data()`, never `data()`.
          r_data <- shiny::reactiveVal(NULL)
          shiny::observe(r_data(data()))

          # Currently picked USUBJID: character(0) when no patient chosen.
          r_subject <- shiny::reactiveVal(subject)

          # Study-declared roles. Study-level configuration, not user input:
          # kept OUT of the block state -- neither persisted with the block
          # nor exposed to external control / the AI assistant. It is the
          # "study_roles" BOARD option (sidebar-editable, serialized with
          # the board), read reactively so a sidebar edit re-resolves every
          # consumer. The legacy app-level option
          # (options(blockr.pharma_arm_var=) / BLOCKR_PHARMA_ARM_VAR) is
          # transitional: it still covers the ARM on boards that have not
          # declared, and goes away -- here and in the deployment's app.R --
          # once every deployed study has declared in its sidebar (see the
          # study-metadata design spec's sequencing).
          r_option_roles <- board_study_roles()
          legacy_arm_var <- blockr.core::blockr_option("pharma_arm_var", NULL)
          stopifnot(
            is.null(legacy_arm_var) ||
              (is.character(legacy_arm_var) && length(legacy_arm_var) == 1L &&
                 nzchar(legacy_arm_var))
          )
          r_declared <- shiny::reactive({
            d <- r_option_roles() %||% list()
            d$arm <- d$arm %||% legacy_arm_var
            d
          })

          # The incoming dm, reconciled ONCE with the names the vizs declare
          # against (pp_normalize_dm(): SDTM-style table names, SDTM/vendor
          # column spellings, typed date derivations).
          # Everything below reads from here -- the picker, roles,
          # availability, coverage, the time range and the renders -- so
          # nothing can see a pre-normalization name again (the class of bug
          # where the time axis was computed from raw names and silently
          # clipped aliased studies). Total (NULL until a dm arrives):
          # observers consume its dependents.
          r_norm_dm <- shiny::reactive({
            dm_obj <- r_data()
            if (!inherits(dm_obj, "dm")) return(NULL)
            pp_normalize_dm(dm_obj)
          })

          # Role resolution, once per (dm, declaration): which column is the
          # arm, which codes severity, which anchors the timeline. Total --
          # an unresolved role lands in $errors and is raised loudly via
          # pp_roles_blocker() on the eval path, never from here.
          r_roles <- shiny::reactive({
            pp_resolve_roles(r_norm_dm(), r_declared())
          })

          # The incoming cohort. This is the universe the picker selects
          # within: an upstream drill-down narrows it, the picker never
          # widens it, so the two can never conflict.
          r_cohort <- shiny::reactive({
            nd <- r_norm_dm()
            if (is.null(nd)) {
              return(list(ids = character(), labels = character(),
                          meta = character()))
            }
            pp_subject_choices(nd, r_roles()$arm)
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
          # chart area shows a placeholder. Scoping is a plain per-table
          # USUBJID filter on the already-normalized dm (pp_scope_subject():
          # every CDISC table carries USUBJID, so no FK cascade is needed),
          # which means a patient switch costs a filter, not a second
          # normalization pass, and there is no ordering constraint between
          # scoping and normalization left to get wrong.
          r_scoped_dm <- shiny::reactive({
            dm_obj <- r_norm_dm()
            shiny::req(inherits(dm_obj, "dm"))
            # Post-normalization this is the canonical name even for a study
            # that shipped the SDTM `dm` domain -- checking the RAW dm here
            # is what used to kill SDTM studies before the alias machinery
            # ever ran.
            shiny::req("adsl" %in% names(dm::dm_get_tables(dm_obj)))
            ids <- pp_subject_ids(dm_obj)
            picked <- pp_resolve_subject(ids, r_subject())
            if (!is.na(picked)) {
              list(
                dm     = pp_scope_subject(dm_obj, picked),
                picked = picked,
                total  = length(ids),
                single = TRUE
              )
            } else {
              # Keep the dm unfiltered so the viz sidebar still
              # populates; the chart area shows the placeholder.
              list(
                dm     = dm_obj,
                picked = NA_character_,
                total  = length(ids),
                single = FALSE
              )
            }
          })

          r_cohort_vizs <- shiny::reactive({
            dm_obj <- r_norm_dm()
            shiny::req(inherits(dm_obj, "dm"))
            # Static vizs are decidable from the schema, so they are always
            # listed and pp_coverage_report() explains any that cannot render.
            # The generated ones answer a question the schema cannot: findings
            # exist per discovered PARAMCD, the cycle lane only where the study
            # is dosed in cycles. Absent means absent -- no card, no gap report.
            c(patient_profile_static_vizs(), pp_cycle_vizs(dm_obj),
              pp_findings_vizs(dm_obj))
          })

          # Available vizs (those whose tables exist in the dm). Derived
          # from the UNSCOPED dm: which vizs the study's data supports is a
          # property of the cohort, not of the patient on screen, so the
          # sidebar and the panel skeleton stay put when the patient
          # changes. A patient with no rows for a viz gets a "no data"
          # message in its chart slot instead of the card vanishing. Same
          # source as the gear's Data coverage report, so the two agree.
          r_available_src <- shiny::reactive({
            dm_obj <- r_norm_dm()
            shiny::req(inherits(dm_obj, "dm"))
            tbl_names <- names(dm::dm_get_tables(dm_obj))
            Filter(function(v) all(v$tables %in% tbl_names), r_cohort_vizs())
          })

          # De-duplicated viz catalog. An upstream dm update (a refreshed
          # read, a re-filtered cohort) almost always yields the SAME set of
          # cards, but pp_findings_vizs() builds fresh render closures every
          # time, so no downstream identical() could ever skip -- the whole
          # sidebar (SELECTED and AVAILABLE cards alike) re-rendered on
          # every upstream emission. Compare catalogs by their non-function
          # fields instead (everything a card or the dispatch reads off a
          # definition -- renders take the dm as an argument, so keeping the
          # previous closures is equivalent when those fields match) and
          # only then let the new object through. The sidebar renderUI and
          # the slot observers below simply do not invalidate on a
          # same-catalog update. Same trick as r_data's identical-skip
          # above, one level up.
          r_available_val <- shiny::reactiveVal(NULL)
          shiny::observe({
            avail <- r_available_src()
            sig <- pp_vizs_signature(avail)
            cur <- shiny::isolate(r_available_val())
            if (!identical(sig, attr(cur, "pp_sig", exact = TRUE))) {
              attr(avail, "pp_sig") <- sig
              r_available_val(avail)
            }
          })
          r_available <- shiny::reactive({
            avail <- r_available_val()
            shiny::req(!is.null(avail))
            avail
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

          # Show the full pre-treatment history (default: clip to a 30-day
          # screening window before treatment start; see pp_clip_prestudy)
          r_show_prestudy <- shiny::reactiveVal(isTRUE(show_prestudy))
          shiny::observeEvent(input$show_prestudy, {
            r_show_prestudy(isTRUE(input$show_prestudy))
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
                count   = length(co$ids),
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

          # Whether a single patient is on screen, plus the cohort size for
          # the placeholder text. A reactiveVal fed by an observer, not a
          # reactive: switching from patient A to patient B re-executes
          # r_scoped_dm but leaves this pair unchanged, and reactiveVal
          # skips invalidation on identical values. That is what keeps the
          # panel skeleton (and with it every chart container) out of the
          # patient-switch redraw path.
          r_pick_state <- shiny::reactiveVal(NULL)
          shiny::observe({
            scoped <- r_scoped_dm()
            r_pick_state(list(single = scoped$single, total = scoped$total))
          })

          # Shared time range. Unless the user opts into the full
          # pre-treatment history, the range floor is a 30-day screening
          # window before treatment start: one medication started years ago
          # must not stretch every axis to it, while baselines (screening
          # labs and vitals) stay on screen. Ongoing bars still enter from
          # the left edge; only events entirely before the floor drop out.
          r_time_range <- shiny::reactive({
            dm_obj <- r_scoped_dm()$dm
            shiny::req(inherits(dm_obj, "dm"))
            tr <- pp_compute_time_range(dm_obj, ref_col = r_roles()$timeline)
            if (!r_show_prestudy()) {
              tr <- pp_clip_prestudy(
                tr, pp_compute_ref_ms(dm_obj, r_roles()$timeline)
              )
            }
            tr
          })

          # Reference timestamp (TRTSDT) used for relative-day mode. The
          # reference is a per-PATIENT value (this subject's treatment
          # start), so it exists only once a single patient is on screen --
          # computing it from an unscoped cohort takes whichever subject
          # happens to sit in ADSL row 1, and one arbitrary patient with a
          # missing treatment start would disable relative-day mode for the
          # whole study.
          r_ref_ms <- shiny::reactive({
            scoped <- r_scoped_dm()
            shiny::req(inherits(scoped$dm, "dm"))
            if (!isTRUE(scoped$single)) return(NA_real_)
            pp_compute_ref_ms(scoped$dm, ref_col = r_roles()$timeline)
          })

          # Treatment cycle anchors (see pp-cycle.R). Per-PATIENT, like
          # r_ref_ms and for the same reason: the cycle calendar is this
          # subject's, delays included. Computed once here rather than per
          # viz -- the cycle lane and every cycle-labelled tooltip read the
          # same frame, so they cannot disagree. NULL for a study without the
          # cycle vocabulary, which is the common case and not an error.
          r_cycle_anchors <- shiny::reactive({
            scoped <- r_scoped_dm()
            shiny::req(inherits(scoped$dm, "dm"))
            if (!isTRUE(scoped$single)) return(NULL)
            pp_cycle_anchor_days(pp_cycle_anchors(scoped$dm), r_ref_ms())
          })

          # Render sidebar cards (re-renders when the cohort's data
          # changes; availability is cohort-based, so patient switches
          # leave the sidebar untouched)
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
              # Available (unselected) section. The groups live in their own
              # scroll box so the sidebar's height is driven by the SELECTED
              # list, not by however many vizs the cohort happens to offer.
              shiny::div(class = "pp-available-section",
                shiny::div(
                  class = "pp-section-header pp-section-header-available",
                  "AVAILABLE"
                ),
                shiny::div(class = "pp-available-list", domain_groups)
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
                        # Visits come in visit order (AVISITN when present):
                        # lexical order puts "Week 10" before "Week 2".
                        choices <- if (identical(col, "AVISIT")) {
                          pp_visit_levels(tbl)
                        } else {
                          sort(unique(as.character(tbl[[col]])))
                        }
                        # Restrict to the viz's declared subset (a findings
                        # group's PARAMCDs), in the subset's clinical order.
                        if (!is.null(ctrl$choices_subset)) {
                          choices <- intersect(ctrl$choices_subset, choices)
                        }
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
            init_prestudy <- shiny::isolate(r_show_prestudy())
            # Whether relative-day mode is possible at all is a property of
            # the study (does ADSL carry a usable TRTSDT), not of the patient
            # on screen. Read it from the unscoped dm: routing through
            # `r_ref_ms()` would make the header depend on `r_subject`, and
            # every pick would rebuild the header and slam both popovers
            # shut. pp_has_ref() asks study-wide -- the per-patient
            # pp_compute_ref_ms() here would let one arbitrary cohort member
            # with a missing treatment start disable the mode for everyone.
            gear_disabled <- !pp_has_ref(r_norm_dm(), r_roles()$timeline)


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
                shiny::div(class = "pp-popover-row",
                  shiny::span(class = "pp-popover-label", "Pre-treatment"),
                  shiny::tags$button(
                    class = "pp-popover-toggle",
                    id = ns("pp_prestudy_toggle"),
                    `data-prestudy` = if (init_prestudy) "1" else "0",
                    type = "button",
                    title = paste0(
                      "Show the full pre-treatment history, or only the ",
                      "30-day screening window before treatment start"
                    ),
                    if (init_prestudy) "Full history" else "Screening only"
                  )
                ),
                # Data coverage: visuals that can't render for this data,
                # with the reason (missing table or required column). Lets
                # users see what's collected without each one having to be
                # selected first. Hidden behind the gear, not permanent.
                {
                  vizs <- r_cohort_vizs()  # req()s until a dm has arrived
                  cov <- pp_coverage_report(r_norm_dm(), vizs)
                  roles <- r_roles()
                  shiny::tagList(
                    shiny::div(class = "pp-popover-divider"),
                    shiny::div(class = "pp-popover-section-label",
                      "Study variables"),
                    shiny::div(class = "pp-coverage-item",
                      shiny::span(class = "pp-coverage-label", "Arm"),
                      shiny::span(class = "pp-coverage-reason",
                        roles$arm %||% "unresolved — see block error")
                    ),
                    shiny::div(class = "pp-coverage-item",
                      shiny::span(class = "pp-coverage-label", "Severity"),
                      shiny::span(class = "pp-coverage-reason",
                        roles$severity %||% "none in adae (bars uncolored)")
                    ),
                    shiny::div(class = "pp-coverage-item",
                      shiny::span(class = "pp-coverage-label", "Timeline"),
                      shiny::span(class = "pp-coverage-reason",
                        roles$timeline %||% "none (relative day off)")
                    ),
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

          # Chart-area skeleton: one stable panel shell per selected viz,
          # each holding its own uiOutput slot (filled by render_viz_slot
          # below). Depends on the selection, the cohort's available vizs
          # and the picked / not-picked state — NOT on which patient is
          # picked, so a patient switch re-renders only the slot contents;
          # the panels, the sidebar and the scroll position stay put.
          output$chart_area <- shiny::renderUI({
            st <- r_pick_state()
            shiny::req(!is.null(st))
            # The profile needs exactly one patient. Until the header picker
            # or an upstream drill-down commits to one, show an info
            # placeholder rather than auto-picking the first subject.
            if (!isTRUE(st$single)) {
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
                  if (isTRUE(st$total > 1L)) {
                    paste0("Pick one of ", st$total,
                           " patients above, or drill down on a chart")
                  } else {
                    "No patient data in the incoming tables"
                  })
              ))
            }
            sel <- r_selected()
            avail <- r_available()

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

            # Panel shells only: the uiOutput itself is the .pp-chart-panel
            # div, so the DOM shape (panel > header + body) is unchanged
            # once the slot renders into it.
            ns <- session$ns
            shiny::tagList(lapply(active_ids, function(viz_id) {
              shiny::uiOutput(
                ns(paste0("viz_slot_", viz_id)),
                class = if (identical(viz_id, "patient_overview")) {
                  "pp-chart-panel pp-treatment-strip"
                } else {
                  "pp-chart-panel"
                }
              )
            }))
          })

          # Render one viz's panel content (header + controls + chart).
          # Everything patient-dependent lives here, so a patient switch
          # re-renders each slot in place and nothing around it.
          render_viz_slot <- function(viz_id) {
            scoped <- r_scoped_dm()
            dm_obj <- scoped$dm
            shiny::req(inherits(dm_obj, "dm"), isTRUE(scoped$single))
            viz <- r_available()[[viz_id]]
            shiny::req(!is.null(viz))
            time_range <- r_time_range()
            shiny::req(time_range)
            ref_ms <- r_ref_ms()
            tl_mode <- r_timeline_mode()
            # Relative-day mode requires a reference timestamp; if TRTSDT
            # isn't available, silently fall back to date mode rather than
            # rendering an empty/value axis.
            if (identical(tl_mode, "rday") && is.na(ref_ms)) {
              tl_mode <- "date"
            }

            # Role injection, driven by the viz's `uses` declaration -- no
            # viz-id matching. The resolved role columns arrive as
            # settings$roles; for the severity role the board scale map's
            # colors ride along as settings$sev_colors (render-time only,
            # r_viz_settings is untouched; each viz falls back to its own
            # constants when no map / no binding resolves).
            viz_settings <- r_viz_settings()[[viz_id]] %||% list()
            roles <- r_roles()
            uses <- viz$uses %||% character()
            if (length(uses)) {
              viz_settings$roles <- roles[intersect(uses, names(roles))]
            }
            if ("severity" %in% uses) {
              sev_colors <- pp_sev_scale_colors(
                r_scale_map(), dm_obj, sev_col = roles$severity
              )
              if (!is.null(sev_colors)) {
                viz_settings$sev_colors <- sev_colors
              }
            }
            if ("cycle" %in% uses) {
              viz_settings$cycle_anchors <- r_cycle_anchors()
            }

            # Check the declared `requires` / `requires_any` columns (a pure
            # presence check -- names were reconciled dm-wide by
            # pp_normalize_dm()). If a required column is missing, render a
            # pp_empty_chart message instead of calling the viz renderer.
            resolved <- pp_resolve_requires(dm_obj, viz)
            chart <- if (!isTRUE(resolved$ok)) {
              pp_empty_chart(resolved$msg)
            } else if (pp_no_patient_rows(dm_obj, viz$tables)) {
              # Availability is cohort-based, so the viz can exist while
              # this particular patient has no rows in any of its tables;
              # say so instead of drawing an empty axis.
              pp_empty_chart("No data for this patient")
            } else {
              tryCatch(
                viz$render(dm_obj, time_range, viz_settings,
                           ref_ms, tl_mode),
                error = function(e) pp_empty_chart(
                  paste("Error:", conditionMessage(e))
                )
              )
            }

            controls_ui <- pp_controls_ui(viz, viz_id, dm_obj, viz_settings)

            # Panel-header legend, declared by the viz itself (e.g. the AE
            # severity swatches) -- again no viz-id matching here.
            legend_ui <- if (is.function(viz$legend_ui)) {
              viz$legend_ui(dm_obj, viz_settings)
            }

            shiny::tagList(
              shiny::div(class = "pp-chart-header",
                shiny::div(class = "pp-chart-title", viz$label),
                controls_ui,
                legend_ui,
                shiny::div(class = "pp-chart-domain", viz$domain),
                # Same toggle the sidebar card fires: removing a viz here
                # deselects it, so the sidebar card slides back to AVAILABLE.
                shiny::tags$button(
                  class = "pp-chart-remove",
                  type = "button",
                  `data-viz-id` = viz_id,
                  title = paste0("Remove ", viz$label),
                  shiny::HTML(paste0(
                    '<svg xmlns="http://www.w3.org/2000/svg" width="12" ',
                    'height="12" fill="currentColor" viewBox="0 0 16 16">',
                    '<path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646',
                    '-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 ',
                    '0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708',
                    'L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/></svg>'
                  ))
                )
              ),
              shiny::div(class = "pp-chart-body", chart)
            )
          }

          # Register one output per available viz, once. The id set is
          # cohort-derived and stable across patient switches; a slot whose
          # viz is not currently selected has no container in the DOM and
          # Shiny keeps it suspended.
          slot_registered <- new.env(parent = emptyenv())
          shiny::observeEvent(r_available(), {
            for (viz_id in names(r_available())) {
              if (isTRUE(slot_registered[[viz_id]])) next
              slot_registered[[viz_id]] <- TRUE
              local({
                vid <- viz_id
                output[[paste0("viz_slot_", vid)]] <- shiny::renderUI(
                  render_viz_slot(vid)
                )
              })
            }
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
              # A role that does not resolve (declared but absent, or an
              # undeclared arm with no ACTARM) must stop the block loudly,
              # not decorate it with plausible labels. The stop() is
              # *returned* rather than raised: blockr.core wraps the
              # evaluation of this expression in its condition capture, so
              # it lands as a named error on the block, next to the sidebar
              # that fixes it.
              blocker <- pp_roles_blocker(data(), r_declared())
              if (!is.null(blocker)) {
                return(blocker)
              }
              if (length(sel) != 1L || !sel %in% ids) {
                return(quote(identity(data)))
              }
              pp_subject_filter_expr(
                pp_subject_tbl_name(names(dm::dm_get_tables(data()))),
                sel
              )
            }),
            state = list(
              selected = r_selected,
              viz_settings = r_viz_settings,
              timeline_mode = r_timeline_mode,
              subject = r_subject,
              show_prestudy = r_show_prestudy
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        # As an htmlDependency, NOT a raw tags$link to the resource path: the
        # dependency's served URL embeds the package version, so a Version
        # bump busts browser caches. A bare link URL never changes, and the
        # browser happily keeps a stale stylesheet across reloads (and
        # load_all()s) -- the inst/js convention, applied to CSS.
        htmltools::htmlDependency(
          "blockr-pharma-pp",
          as.character(utils::packageVersion("blockr.pharma")),
          src = system.file("assets", package = "blockr.pharma"),
          stylesheet = "css/patient-profile.css"
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
                ),
                # Clear: appears only while the box has text. Restores the
                # full list, SELECTED section included.
                shiny::tags$button(
                  class = "pp-sidebar-search-clear is-hidden",
                  id = ns("search_clear"),
                  type = "button",
                  title = "Clear search",
                  shiny::HTML(paste0(
                    '<svg xmlns="http://www.w3.org/2000/svg" width="14" ',
                    'height="14" fill="currentColor" viewBox="0 0 16 16">',
                    '<path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646',
                    '-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 ',
                    '0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708',
                    'L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/></svg>'
                  ))
                )
              )
            ),

            # Card list
            shiny::div(class = "pp-sidebar-content",
              shiny::uiOutput(ns("sidebar_cards"), class = "pp-sidebar-cards")
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
                ),
                # Cohort-size tag, two-tone like the dock's Package badge.
                # Filled by the subject_picker message (cohort-scoped), so
                # it updates when a drill narrows the cohort but never
                # redraws on a patient switch. Hidden until the first
                # cohort arrives.
                shiny::span(
                  class = "pp-cohort-count is-hidden",
                  id = ns("pp_cohort_count"),
                  title = "Patients in cohort",
                  shiny::HTML(paste0(
                    '<svg xmlns="http://www.w3.org/2000/svg" width="11" ',
                    'height="11" fill="currentColor" viewBox="0 0 16 16">',
                    '<path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6m2-3a2 2 0 ',
                    '1 1-4 0 2 2 0 0 1 4 0m4 8c0 1-1 1-1 1H3s-1 0-1-1 ',
                    '1-4 6-4 6 3 6 4m-1-.004c-.001-.246-.154-.986-.832',
                    '-1.664C11.516 10.68 10.289 10 8 10c-2.29 0-3.516 ',
                    '.68-4.168 1.332-.678.678-.83 1.418-.832 ',
                    '1.664z"/></svg>'
                  )),
                  shiny::span(class = "pp-cohort-count-n")
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
            var clearBtnId = '", ns("search_clear"), "';
            var ctrlInputId = '", ns("viz_ctrl"), "';
            var syncMsgId = '", ns("sync_selected"), "';
            var reorderInputId = '", ns("reorder_viz"), "';

            var dragActive = false;
            var tlModeInputId = '", ns("timeline_mode"), "';
            var prestudyInputId = '", ns("show_prestudy"), "';
            var gearBtnId = '", ns("pp_gear_btn"), "';
            var gearPopoverId = '", ns("pp_gear_popover"), "';

            var pickerId = '", ns("pp_picker"), "';
            var subjectContainerId = '", ns("pp_subject"), "';
            var subjectStaticId = '", ns("pp_subject_static"), "';
            var cohortCountId = '", ns("pp_cohort_count"), "';
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

              // Cohort-size tag. Cohort-scoped by construction: this
              // handler only runs when the cohort itself changes.
              var n = msg.count || 0;
              var $count = $('#' + cohortCountId);
              $count.find('.pp-cohort-count-n').text(n.toLocaleString());
              $count.attr('title',
                n === 1 ? '1 patient in cohort' : n + ' patients in cohort');
              $count.toggleClass('is-hidden', !n);
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
            $(document).on('click',
              '#' + layoutId + ' .pp-popover-toggle[data-tl-mode]',
              function(e) {
                e.stopPropagation();
                if ($(this).attr('data-disabled') === '1') return;
                var cur = $(this).attr('data-tl-mode');
                var next = (cur === 'rday') ? 'date' : 'rday';
                // Optimistic UI update; server re-render will confirm.
                $(this).text(next === 'rday' ? 'Relative day' : 'Date');
                $(this).attr('data-tl-mode', next);
                Shiny.setInputValue(tlModeInputId, next, {priority: 'event'});
              });

            // Pre-treatment history toggle, same flip-on-click pattern.
            $(document).on('click',
              '#' + layoutId + ' .pp-popover-toggle[data-prestudy]',
              function(e) {
                e.stopPropagation();
                var on = $(this).attr('data-prestudy') === '1';
                var next = !on;
                $(this).text(next ? 'Full history' : 'Screening only');
                $(this).attr('data-prestudy', next ? '1' : '0');
                Shiny.setInputValue(prestudyInputId, next,
                                    {priority: 'event'});
              });

            // Card click: toggle selection (server-driven, no optimistic toggle)
            $(document).on('click', '#' + layoutId + ' .pp-card', function(e) {
              if (dragActive) return;
              var vizId = $(this).data('viz-id');
              if (!vizId) return;
              Shiny.setInputValue(toggleInputId, vizId, {priority: 'event'});
            });

            // Panel x: remove the viz. Same input as the card, so the server
            // deselects it and the sidebar card slides back to AVAILABLE.
            $(document).on('click', '#' + layoutId + ' .pp-chart-remove',
              function(e) {
                e.stopPropagation();
                var vizId = $(this).attr('data-viz-id');
                if (!vizId) return;
                Shiny.setInputValue(toggleInputId, vizId, {priority: 'event'});
              });

            // Search: client-side filtering across both sections.
            //
            // Match on the card's own data-search-text and mark it with a
            // class -- never on `:visible`. `:visible` is false for every
            // card inside an already-hidden section, so a section hidden by
            // one keystroke could never come back on the next: the SELECTED
            // section stayed collapsed after the query was cleared.
            //
            // The empty-selection hints hide while a query is live: they
            // speak about the full list, not about the matches.
            function applyFilter() {
              var $sidebar = $('#' + sidebarId);
              var query = ($('#' + searchId).val() || '').toLowerCase().trim();

              $('#' + clearBtnId).toggleClass('is-hidden', !query);
              $sidebar.toggleClass('is-searching', !!query);

              $sidebar.find('.pp-card').each(function() {
                var text = ($(this).attr('data-search-text') || '').toLowerCase();
                var hit = !query || text.indexOf(query) >= 0;
                $(this).toggleClass('is-filtered-out', !hit);
              });

              // A section/group is shown when it still holds a matching card.
              // Empty domain groups stay hidden either way (the sync handler
              // leaves emptied groups in the DOM).
              $sidebar.find('.pp-category-group').each(function() {
                var hits = $(this).find('.pp-card').not('.is-filtered-out').length;
                $(this).toggleClass('is-hidden', hits === 0);
              });
              $sidebar.find('.pp-active-section, .pp-available-section')
                .each(function() {
                  var hits = $(this).find('.pp-card').not('.is-filtered-out').length;
                  // With no query, keep the SELECTED section up even when it
                  // holds no card: its empty hint is the invitation to add one.
                  $(this).toggleClass('is-hidden', !!query && hits === 0);
                });
            }

            $(document).on('input', '#' + searchId, applyFilter);

            // Clear (x): reset the query and hand back the full list
            $(document).on('click', '#' + clearBtnId, function(e) {
              e.stopPropagation();
              $('#' + searchId).val('').focus();
              applyFilter();
            });

            // Escape clears too, while the box has focus
            $(document).on('keydown', '#' + searchId, function(e) {
              if (e.key !== 'Escape' && e.keyCode !== 27) return;
              if (!$(this).val()) return;
              e.stopPropagation();
              $(this).val('');
              applyFilter();
            });

            // A live query must survive a sidebar re-render (cohort change)
            $(document).on('shiny:value', function(e) {
              if (e.name && e.name.indexOf('sidebar_cards') >= 0) {
                setTimeout(applyFilter, 0);
              }
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
              var $availList = $layout.find('.pp-available-list');

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
                var $group = $availList
                  .find('.pp-category-group[data-domain=' + JSON.stringify(domain) + ']');
                if (!$group.length) {
                  $group = $('<div class=pp-category-group data-domain=' +
                    JSON.stringify(domain) + '>' +
                    '<div class=pp-category-header><span>' +
                    domain.toUpperCase() + '</span></div></div>');
                  $availList.append($group);
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

              // Hide empty domain groups, show non-empty ones. Class, not
              // .toggle(): an inline display would outrank the search filter.
              $availList.find('.pp-category-group').each(function() {
                var hasCards = $(this).find('.pp-card').length > 0;
                $(this).toggleClass('is-empty', !hasCards);
              });

              // Cards moved between the sections keep the live query honest
              applyFilter();
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
    # here wedges the block. EVERY state field that can legitimately be
    # empty MUST be listed: an empty field missing here makes core's
    # state_ready() FALSE forever, which req()-blocks dat_eval — the block's
    # RESULT stays NULL (invisible while the block is terminal) and the AI
    # ctrl chat can never read the input data.
    allow_empty_state = c("selected", "viz_settings", "subject"),
    external_ctrl = c("selected", "viz_settings", "timeline_mode", "subject",
                      "show_prestudy"),
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
