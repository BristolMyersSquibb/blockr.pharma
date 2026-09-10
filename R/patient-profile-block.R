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
#' @param smooth Line smoothing for the findings value lines (labs, vitals):
#'   `"auto"` (default) draws monotone-smoothed lines -- no overshoot, the
#'   curve never implies values outside the measured range -- `"off"` draws
#'   straight segments. Also a toggle in the gear popover ("Value lines").
#'   Same wire values as the chart block's `smooth` option.
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
                                              smooth = "auto",
                                              ...) {
  timeline_mode <- match.arg(timeline_mode, c("rday", "date"))
  subject <- pp_validate_subject(subject)
  stopifnot(isTRUE(show_prestudy) || isFALSE(show_prestudy))
  smooth <- match.arg(smooth, c("auto", "off"))

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
  viz_settings <- pp_migrate_viz_settings(viz_settings)

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

          # The cohort as a LIST: one row per patient, for the sidebar and
          # the download. Separate from r_cohort() (which is the picker's
          # ids/labels) because it answers a different question -- not "who
          # can I pick" but "what does each of them look like".
          # The arm palette, resolved ONCE. The cohort list's chips and the
          # patient info card's arm chip both draw it, and they are two
          # surfaces a reader compares directly -- the row they clicked and
          # the card it opened -- so a second computation is a second chance
          # to disagree. Keyed off the COHORT frame, never a scoped one:
          # without a board binding the assignment is by sorted level, and a
          # single-patient frame has one level.
          r_arm_colors <- shiny::reactive({
            pp_cohort_arm_colors(r_cohort_frame(), r_scale_map(), r_norm_dm(),
                                 r_roles()$arm)
          })

          r_cohort_frame <- shiny::reactive({
            nd <- r_norm_dm()
            if (is.null(nd)) return(pp_cohort_frame(NULL))
            pp_cohort_frame(nd, r_roles())
          })

          # Which panel the cohort band draws. The FIRST selected viz that
          # declares a band form (pp_cohort_band_source()), or the one being
          # searched if there is one, so reordering the
          # panel list reorders what the strip shows and a panel with no
          # strip form -- the patient overview, a table -- is stepped over
          # rather than drawn badly. NULL when nothing selected can be drawn:
          # the rows then keep an empty track.
          # Which panel drives the cohort band -- and ONLY when it changes.
          #
          # A reactiveVal behind an observer, not a plain reactive. It depends
          # on the selection AND on every viz's settings (a findings card's
          # chips decide which parameter it leads with), and Shiny propagates
          # invalidation whether or not the value moved. So reordering the
          # last two panels, or ticking a chip on a card that is not first,
          # re-derived all 254 bands: measured at 2 cohort redraws for a
          # reorder that changed nothing about the strip.
          #
          # Guarded by identical(), which works because a source is plain
          # data -- ids, labels and a band declaration, no closures.
          r_band_source <- shiny::reactiveVal(NULL)
          shiny::observe({
            src <- pp_cohort_band_source(r_selected(), r_available(),
                                         r_viz_settings())
            if (!identical(shiny::isolate(r_band_source()), src)) {
              r_band_source(src)
            }
          })

          # The picks the STRIP follows, a beat behind the panel's.
          #
          # Both re-derive on the same change and they do not cost the same:
          # the panel redraws one patient's events, the strip redraws 254
          # patients' bands. Landing them together made the sidebar flicker
          # under the cursor while the chart above it was already settling.
          # Kept now that the picker applies a whole set at once rather than
          # a character at a time, because the two costs have not changed and
          # a set can still be built one tick at a time with the popover open.
          r_band_picks_raw <- shiny::debounce(
            shiny::reactive({
              src <- r_band_source()
              if (is.null(src) || !length(src$band$search %||% character())) {
                return(list())
              }
              pp_find_picks(r_viz_settings()[[src$viz_id]]$find)
            }),
            350
          )

          # Deduped, for the same reason the source above is. The debounce
          # re-emits whenever ANY of its inputs invalidate, so changing the
          # band source made the strip redraw twice for one change: once when
          # the source moved, and again 350ms later when the debounce fired
          # with a term that had not changed.
          r_band_picks <- shiny::reactiveVal(list())
          shiny::observe({
            v <- r_band_picks_raw()
            if (!identical(shiny::isolate(r_band_picks()), v)) {
              r_band_picks(v)
            }
          })

          # Event geometry for the row bands. Split from the frame so the
          # download never carries it and a richer band never widens the
          # export.
          r_cohort_marks <- shiny::reactive({
            nd <- r_norm_dm()
            src <- r_band_source()
            if (is.null(nd) || is.null(src)) return(pp_cohort_marks(NULL))
            # The driving panel's own filter filters the strip, so the two
            # show one subset of the records. A band whose panel declares no
            # find control is unfiltered whatever else is picked on the board.
            picks <- r_band_picks()
            # Follows the profile's Pre-treatment toggle, so the band and the
            # panels floor their axis at the same place.
            pp_cohort_marks(
              nd, r_roles(),
              prestudy_days = if (r_show_prestudy()) Inf else 30,
              band = src$band,
              picks = picks
            )
          })

          # Which key the list is sorted by. Not block state: it is a way of
          # looking at the cohort, not a fact about the board, and persisting
          # it would restore a board sorted by a column the next study may
          # not have.
          r_cohort_sort <- shiny::reactiveVal("id")
          shiny::observeEvent(input$cohort_sort, {
            key <- as.character(input$cohort_sort)
            if (length(key) == 1L && nzchar(key)) r_cohort_sort(key)
          })

          # The ladder follows the strip, so a key can stop existing under a
          # standing selection: sort by peak value, put the AE band back, and
          # the list would still be ordered by a peak nothing on screen shows.
          # Falls back to the id rather than to another signal -- the order a
          # reader can predict is the right place to land.
          shiny::observe({
            keys <- names(pp_cohort_sort_choices(r_cohort_frame(),
                                                 r_cohort_marks()$kind))
            if (!shiny::isolate(r_cohort_sort()) %in% keys) {
              r_cohort_sort("id")
            }
          })

          # Pick a patient from the cohort list. The whole point of the
          # merge: the click never leaves the block, so it needs no control
          # channel and cannot be gated off by the sender leaving the screen.
          shiny::observeEvent(input$pick_subject, {
            sel <- pp_cohort_pick(input$pick_subject, r_cohort()$ids)
            if (!is.null(sel)) r_subject(sel)
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

          # The study's parameter dictionary (PARAMCD -> PARAM -> category),
          # accumulated. Findings cards derive their label, description,
          # search text and chip list from it, so it must describe the STUDY,
          # not the patient on screen: a drilled single-patient dm reveals
          # only that patient's parameters, and a dictionary that shrank with
          # it would rewrite every card on every drill. Growing-only, so it is
          # complete from the first cohort-level emission and never regresses.
          r_param_dict <- shiny::reactiveVal(NULL)
          shiny::observe({
            dm_obj <- r_norm_dm()
            shiny::req(inherits(dm_obj, "dm"))
            merged <- pp_param_dict_merge(
              shiny::isolate(r_param_dict()), pp_dm_param_dict(dm_obj)
            )
            if (!identical(merged, shiny::isolate(r_param_dict()))) {
              r_param_dict(merged)
            }
          })

          r_cohort_vizs <- shiny::reactive({
            dm_obj <- r_norm_dm()
            shiny::req(inherits(dm_obj, "dm"))
            dict <- r_param_dict()
            shiny::req(!is.null(dict))
            # Static vizs are decidable from the schema, so they are always
            # listed and pp_coverage_report() explains any that cannot render.
            # The generated ones answer a question the schema cannot: findings
            # exist per discovered PARAMCD, the cycle lane only where the study
            # is dosed in cycles. Absent means absent -- no card, no gap report.
            c(patient_profile_static_vizs(), pp_cycle_vizs(dm_obj),
              pp_response_vizs(dm_obj),
              pp_findings_vizs_from_dict(
                dict, names(dm::dm_get_tables(dm_obj))
              ))
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
            cur <- shiny::isolate(r_available_val())

            # A chart drill passes through "no selection" between clicks,
            # and the upstream filter then emits an EMPTY dm before the new
            # patient lands. An empty selection says nothing about what the
            # study collected, so it must not erase the catalog: without
            # this hold, the findings cards vanished for the fraction of a
            # second between the empty emission and the patient arriving --
            # the sidebar "flash" on every swim-lane drill. (A table drill
            # emits one atomic update and never showed it.)
            if (!is.null(cur) &&
                  length(pp_subject_ids(shiny::isolate(r_norm_dm()))) == 0L) {
              return()
            }

            # The catalog REMEMBERS. A drilled single-patient input only
            # reveals the params THAT patient carries, but the cards describe
            # the study: the data-generated per-param cards (adlb_*) differed
            # per patient, so every swim-lane drill re-rendered the sidebar
            # (patients without basole/eosle/... dropped those cards, the
            # next one brought them back). A previously seen card therefore
            # stays as long as its table stays; a patient without the param
            # gets the "No records" message in its slot -- which is this
            # block's stated philosophy for patient-level emptiness anyway.
            # Remembered extras append sorted, so the merged order is a pure
            # function of the union and the signature converges after the
            # first few drills.
            if (!is.null(cur)) {
              tbl_names <- names(dm::dm_get_tables(
                shiny::isolate(r_norm_dm())
              ))
              seen <- Filter(
                function(v) all(v$tables %in% tbl_names),
                cur
              )
              extra <- setdiff(names(seen), names(avail))
              if (length(extra)) {
                avail <- c(avail, seen[sort(extra)])
              }
            }

            sig <- pp_vizs_signature(avail)
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

          # The same settings, split one key per viz, so a panel depends on
          # ITS OWN settings and nothing else.
          #
          # r_viz_settings is a single reactiveVal holding every viz's
          # settings, so reading it takes a dependency on all of them: one
          # keystroke in the AE find box invalidated the chemistry panel, the
          # overview and every other slot on screen, and the whole profile
          # redrew per character. reactiveValues tracks per KEY, and the
          # observer below only writes a key whose value actually changed --
          # so typing in one panel now invalidates one panel.
          r_slot_settings <- shiny::reactiveValues()
          shiny::observe({
            all_settings <- r_viz_settings()
            shiny::isolate({
              for (id in union(names(all_settings), names(r_slot_settings))) {
                cur <- all_settings[[id]]
                if (!identical(r_slot_settings[[id]], cur)) {
                  r_slot_settings[[id]] <- cur
                }
              }
            })
          })

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

          # Line smoothing for the findings value lines ("auto" = monotone,
          # "off" = straight segments). Same wire values as the chart block's
          # `smooth` option, so the two timeline surfaces speak one language.
          r_smooth <- shiny::reactiveVal(smooth)
          shiny::observeEvent(input$smooth_mode, {
            if (isTRUE(input$smooth_mode %in% c("auto", "off"))) {
              r_smooth(input$smooth_mode)
            }
          })

          # Initialize selection to patient_overview + first available
          init_done <- shiny::reactiveVal(FALSE)
          shiny::observeEvent(r_available(), {
            if (!init_done()) {
              avail <- r_available()
              cur <- r_selected()
              # A board saved when a findings card was one viz per GROUP names
              # ids that no longer exist. Expand them into the parameter cards
              # that card would have drawn, before deciding the selection is
              # unusable and replacing it with defaults.
              grown <- pp_expand_groups(cur, avail)
              if (!identical(grown, cur)) {
                r_selected(grown)
                cur <- grown
              }
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

          # Toggle one parameter from the search results. Three cases, and the
          # middle one is the point of the feature: a card already on the
          # panel GAINS the parameter (you were adding aspartate next to
          # alanine), while a card not yet there opens showing that parameter
          # ALONE -- opening a twenty-parameter Chemistry card because you
          # searched for one of them would bury the thing you asked for.
          # Unchecking the last parameter takes the card off the panel: an
          # empty card would draw an axis and nothing else.
          shiny::observeEvent(input$pick_param, {
            msg <- input$pick_param
            viz_id <- msg$viz_id
            code <- msg$paramcd
            if (is.null(viz_id) || is.null(code)) return()
            viz_id <- as.character(viz_id)
            code <- as.character(code)
            viz <- r_available()[[viz_id]]
            if (is.null(viz) || !code %in% names(viz$params)) return()

            sel <- r_selected()
            settings <- r_viz_settings()
            if (is.null(settings[[viz_id]])) {
              settings[[viz_id]] <- pp_viz_defaults(viz)
            }
            cur <- as.character(settings[[viz_id]]$items %||% character())

            if (!viz_id %in% sel) {
              settings[[viz_id]]$items <- code
              r_selected(c(sel, viz_id))
            } else if (code %in% cur) {
              rest <- setdiff(cur, code)
              if (length(rest) == 0L) {
                r_selected(setdiff(sel, viz_id))
              } else {
                settings[[viz_id]]$items <- rest
              }
            } else {
              settings[[viz_id]]$items <- union(cur, code)
            }
            r_viz_settings(settings)
          })

          # Ship the parameters currently on screen to the result rows'
          # check marks. Depends on BOTH the selection and the settings: a
          # card off the panel shows none of its parameters as checked, however
          # its items happen to be set.
          shiny::observe({
            sel <- r_selected()
            settings <- r_viz_settings()
            avail <- r_available()
            keys <- unlist(lapply(intersect(sel, names(avail)), function(vid) {
              items <- settings[[vid]]$items %||%
                pp_viz_defaults(avail[[vid]])$items
              if (length(items) == 0L) return(NULL)
              paste0(vid, "@@", as.character(items))
            }))
            pp_send(session, "sync_params", keys
            )
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

            # Validate against what the client could SEE, not against the whole
            # selection. `selected` may name vizs this data cannot offer -- the
            # sidebar renders intersect(sel, avail) and the chart area does the
            # same -- so a card id that is selected but unavailable is invisible
            # to the user and can never come back in the drag payload.
            #
            # Comparing the payload against all of `cur` therefore FAILED on
            # exactly the boards that matter: a real study whose findings cards
            # are derived from its own PARCAT1 / LBCAT categories, restored
            # against a patient or a study that does not produce one of them.
            # One stale id in the selection and setequal() is FALSE for every
            # drag, forever, silently -- no error, no message, the card just
            # snaps back. Demo boards never showed it because their selection
            # is always a subset of what the demo data offers.
            avail <- names(r_available())
            visible <- intersect(cur, avail)

            # Still a permutation check, so a malformed payload is still
            # refused; it is just the permutation of the VISIBLE cards.
            if (!setequal(new_order, visible)) {
              return()
            }

            # Write the new order into the visible slots and leave the rest
            # pinned where they are. The unavailable ids belong to the board,
            # not to this patient, so they must survive the reorder -- and
            # reordering what you can see should not move what you cannot.
            out <- cur
            out[cur %in% visible] <- new_order
            r_selected(out)
          })

          # Sync sidebar toggle state to client whenever selection changes
          shiny::observe({
            sel <- r_selected()
            # Touch r_available so sync fires after sidebar re-renders on
            # data change (not just on selection change)
            r_available()
            pp_send(session, "sync_selected", sel
            )
          })

          # The cohort's SIZE, for the tag in the header.
          #
          # This used to ship every patient as a {value, label} pair to feed a
          # Blockr.Select in the header. The sidebar's cohort list is the
          # picker now, so the message carries a count -- 2000 options no
          # longer cross the wire on every cohort change, and the tag it
          # fills is the sidebar's toggle.
          shiny::observe({
            pp_send(session, "subject_picker", list(count = length(r_cohort()$ids))
            )
          })

          # Was this cohort drilled into, and by what. Read off the incoming
          # dm's filter trail against the board's drill filter (pp-drill.R);
          # the pill in the header takes the active-filter tint and grows a
          # reset when there is a clause. An empty string, not NULL, for
          # "no drill": jsonlite ships a NULL element as `{}`, which the
          # client would read as true.
          r_drill <- shiny::reactive({
            tgt <- blockr.viz::ctrl_targets("drill_filter_block",
                                            session = session)
            pp_drill_state(blockr.dm::filter_trail(r_data()), unname(tgt))
          })

          shiny::observe({
            pp_send(session, "drill", list(clause = r_drill()$clause %||% ""))
          })

          # The reset: tell the drill filter to forget its claim, over the
          # same channel the click came in on. `ctrl_send()`, not
          # `ctrl_clear()`: the clear is scoped to the block that made the
          # claim, and this block did not -- the reader did, from the place
          # where the drill's effect is visible.
          shiny::observeEvent(input$undrill, {
            d <- shiny::isolate(r_drill())
            if (is.null(d)) return()
            blockr.viz::ctrl_send(d$id, state = list(columns = list()),
                                  session = session)
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

          # The cohort's vocabulary for a find popover, on request.
          #
          # The picker ships this patient's terms with the header (a few
          # hundred bytes). The COHORT's terms are a different thing: several
          # hundred rows and about 16kB, bounded by the coding dictionary
          # rather than by the study's size, and needed only when the reader
          # types -- looking for a term this patient does not have is how you
          # arm a filter before paging through the cohort. So it is fetched,
          # once, and only then.
          #
          # The token is a counter rather than a hash: it is bumped by the
          # same dm the vocabulary is built from, so a client holding the
          # current token is holding the current list, and an upstream filter
          # that narrows the cohort invalidates it for free.
          r_vocab_token <- shiny::reactiveVal(0L)
          shiny::observeEvent(r_norm_dm(), {
            r_vocab_token(shiny::isolate(r_vocab_token()) + 1L)
          })
          vocab_cache <- new.env(parent = emptyenv())

          shiny::observeEvent(input$find_vocab, {
            msg <- input$find_vocab
            viz_id <- as.character(msg$viz_id %||% "")
            if (!nzchar(viz_id)) return()
            token <- as.character(r_vocab_token())
            # The client already has this one; say so rather than resending
            # 16kB it would throw away.
            if (identical(as.character(msg$have %||% ""), token)) {
              pp_send(session, "find_vocab",
                      list(viz_id = viz_id, token = token, unchanged = TRUE))
              return()
            }
            viz <- r_available()[[viz_id]]
            ctrl <- viz$controls$find
            if (is.null(viz) || is.null(ctrl)) return()
            key <- paste0(viz_id, "@", token)
            groups <- vocab_cache[[key]]
            if (is.null(groups)) {
              groups <- pp_find_vocab(pp_find_table(r_norm_dm(), viz$tables),
                                      ctrl$levels)
              # One entry per token: the previous cohort's list is dead the
              # moment the token moves, and holding both would grow the
              # session by 16kB per upstream change.
              rm(list = ls(vocab_cache), envir = vocab_cache)
              assign(key, groups, envir = vocab_cache)
            }
            pp_send(session, "find_vocab",
                    list(viz_id = viz_id, token = token, groups = groups))
          })

          # Whether a single patient is on screen. A reactiveVal fed by an
          # observer, not a reactive: switching from patient A to patient B
          # re-executes r_scoped_dm but leaves this flag unchanged, and
          # reactiveVal skips invalidation on identical values. That is what
          # keeps the panel skeleton (and with it every chart container) out
          # of the redraw path.
          #
          # The cohort SIZE is deliberately not in here. It used to be, for
          # the placeholder's "Pick one of N patients" line, which meant every
          # upstream filter changed the pair and rebuilt the whole chart area
          # -- the placeholder flashing, and, with a patient picked, every
          # chart container torn down and remade. The size reaches that one
          # sentence through r_cohort_total below instead.
          r_pick_state <- shiny::reactiveVal(NULL)
          shiny::observe({
            r_pick_state(list(single = r_scoped_dm()$single))
          })

          # Cohort size, for the placeholder sentence only. Same identical-
          # skip, so an upstream emission that leaves the count alone does not
          # redraw the sentence either.
          r_cohort_total <- shiny::reactiveVal(NULL)
          shiny::observe({
            r_cohort_total(r_scoped_dm()$total)
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
          # subject's, delays included. The cycle BAND is the only consumer --
          # no tooltip derives a cycle/day, every label that shows one reads
          # it off its own record. NULL for a study without the cycle
          # vocabulary, which is the common case and not an error.
          r_cycle_anchors <- shiny::reactive({
            scoped <- r_scoped_dm()
            shiny::req(inherits(scoped$dm, "dm"))
            if (!isTRUE(scoped$single)) return(NULL)
            pp_cycle_anchors(scoped$dm)
          })

          # Render sidebar cards (re-renders when the cohort's data
          # changes; availability is cohort-based, so patient switches
          # leave the sidebar untouched)
          # The cohort list. Rows are built server-side, like the viz cards:
          # the row's marks are data-derived SVG, and shipping them as
          # markup keeps the client with no cohort logic of its own.
          output$sidebar_cohort <- shiny::renderUI({
            frame <- r_cohort_frame()
            # ISOLATED, deliberately. A pick must not invalidate this output:
            # taking a dependency on r_subject() rebuilt all 254 rows on every
            # click, which is a visible redraw of the list you are clicking
            # in. The class is stamped once for the initial render and moved
            # by the sync_subject message afterwards -- the same "update over
            # a message, never a re-render" treatment the download menu's
            # labels get, and for the same reason.
            picked <- shiny::isolate(r_subject())

            if (!nrow(frame)) {
              return(shiny::div(class = "pp-cohort-empty",
                "No patients in the cohort"))
            }

            marks <- r_cohort_marks()
            color <- pp_cohort_sev_color(
              pp_sev_scale_colors(r_scale_map(), r_norm_dm(),
                                  r_roles()$severity)
            )
            arm_col <- r_arm_colors()

            ord <- pp_cohort_order(frame, r_cohort_sort(), marks)

            # A prod USUBJID is ~20 characters and most of them are the study
            # id, which is the same on every row of the board. Lift the shared
            # prefix out to the section header; the row keeps every character
            # that distinguishes one patient from another, and the full id
            # stays in the tooltip, the click payload and the download.
            disp <- pp_cohort_id_display(frame$USUBJID)

            # The rows are built as HTML rather than as a tag tree, and the
            # escaping that costs is done there. See pp_cohort_rows_html().
            # Straight or rounded, from the same gear toggle the panels
            # read: a strip curving above a panel drawing straight segments
            # would be two different claims about the same measurements.
            pp_cohort_rows_html(frame, ord, disp, marks, color, arm_col,
                                picked,
                                smooth = !identical(r_smooth(), "off"),
                                sort_by = r_cohort_sort())
          })

          # Which panel the strip is drawing, for the tag in its header.
          # A message rather than a re-render: marking the panel is a class,
          # and rebuilding two charts to move a five-word label would undo
          # the point of the guard above.
          shiny::observe({
            src <- r_band_source()
            pp_send(session, "sync_band", list(viz_id = if (is.null(src)) "" else src$viz_id)
            )
          })

          # Move the selected class when the pick changes from ANYWHERE: the
          # header picker, the step arrows, an external ctrl_send(), or the
          # stale-selection guard clearing it. The row click also moves it
          # optimistically client-side, so this is what keeps the other four
          # paths in step without touching the list's markup.
          shiny::observeEvent(r_subject(), {
            cur <- r_subject()
            pp_send(session, "sync_subject", list(id = if (length(cur) == 1L) cur else "")
            )
          }, ignoreNULL = FALSE, ignoreInit = TRUE)

          # Sort keys follow the data: a study with no adae is not offered a
          # sort by event count.
          #
          # It reads as a clause on the caption row -- "Chemistry . ALT ...
          # by peak" -- rather than as a labelled control on a row of its
          # own. The caption says what the strip draws and the sort says how
          # those are ordered, which is one sentence, and putting it on one
          # line gave the well back a row of the sidebar. A click walks to
          # the next rung, as the pill did.
          cohort_sort_ui <- function() {
            pp_cohort_sort_ui(
              pp_cohort_sort_choices(r_cohort_frame(), r_cohort_marks()$kind),
              shiny::isolate(r_cohort_sort()),
              session$ns
            )
          }

          # What the band is showing. A severity strip explains itself; a
          # line of unlabelled numbers does not, and the parameter behind it
          # is the part a reader cannot guess. Absent when nothing drawable
          # is selected -- there is then nothing to name.
          output$cohort_band_caption <- shiny::renderUI({
            src <- r_band_source()
            sorter <- cohort_sort_ui()
            pre <- pp_cohort_id_display(r_cohort_frame()$USUBJID)$prefix
            if (is.null(src) && is.null(sorter) && !nzchar(pre)) return(NULL)
            pp_band_caption_ui(src, sorter, pre, r_band_picks())
          })

          # Who is on screen, and the facts about them the sidebar row has
          # no room for. Reads the cohort FRAME, which already carries every
          # one of them (SEX, AGE, TRTDURD, AE_N, AE_WORST) plus the arm --
          # so the header costs a lookup, not a derivation.
          output$subject_facts <- shiny::renderUI({
            cur <- r_subject()
            if (length(cur) != 1L || !nzchar(cur)) {
              return(shiny::span(class = "pp-subject-none",
                                 "No patient selected"))
            }
            frame <- r_cohort_frame()
            at <- match(cur, frame$USUBJID)
            disp <- pp_cohort_id_display(frame$USUBJID)
            arm <- pp_subject_arm(r_norm_dm(), cur, r_roles()$arm)
            pp_subject_facts_ui(frame, at, cur, disp, arm,
                                pp_cohort_sev_color(
                                  pp_sev_scale_colors(r_scale_map(),
                                                      r_norm_dm(),
                                                      r_roles()$severity)
                                ))
          })

          # The picker behind the + button.
          #
          # Rendered ONCE per catalogue, not per selection: it re-renders only
          # when the study's available vizs change, so opening it, typing in
          # it and ticking things off never rebuilds it underneath the user.
          # Which rows are ticked is kept in step by the sync_selected message
          # the sidebar cards already use.
          #
          # Filtering is client-side. The catalogue is a few dozen rows and it
          # is all here already; a round trip per keystroke would buy nothing
          # and cost the caret (see the AE find box).
          output$panel_picker <- shiny::renderUI({
            avail <- r_available()
            if (!length(avail)) return(NULL)
            pp_add_picker_ui(avail, session$ns)
          })

          # Build per-viz control toolbar HTML

          # Header bar (subject picker + gear popover) — depends on the
          # cohort, NOT on r_timeline_mode or r_subject. This keeps either
          # popover from being rebuilt (and closing) when the user flips the
          # timeline toggle or picks a patient. Both button labels are kept
          # in sync by optimistic JS plus a confirming custom message.
          # Whether relative-day mode is possible at all is a property of the
          # study (does ADSL carry a usable TRTSDT), not of the patient on
          # screen. Read from the unscoped dm: routing through `r_ref_ms()`
          # would make the header depend on `r_subject`, and every pick would
          # rebuild the header and slam both popovers shut. pp_has_ref() asks
          # study-wide -- the per-patient pp_compute_ref_ms() would let one
          # arbitrary cohort member with a missing treatment start disable the
          # mode for everyone.
          #
          # A reactiveVal, not a read inside the header: `r_norm_dm()` is a
          # plain reactive and so invalidates on EVERY upstream emission, even
          # one that leaves this flag alone. The header must only rebuild when
          # the flag actually flips, or an upstream filter rebuilds the gear
          # (and shuts an open popover) on every keystroke.
          r_gear_disabled <- shiny::reactiveVal(NULL)
          shiny::observe({
            r_gear_disabled(!pp_has_ref(r_norm_dm(), r_roles()$timeline))
          })

          output$header_bar <- shiny::renderUI({
            gear_disabled <- r_gear_disabled()
            shiny::req(!is.null(gear_disabled))
            pp_header_bar_ui(
              session$ns, gear_disabled,
              mode = shiny::isolate(r_timeline_mode()),
              prestudy = shiny::isolate(r_show_prestudy()),
              smooth = shiny::isolate(r_smooth())
            )
          })

          # Keep the static menu's labels and section visibility in step
          # with the pick and the cohort -- text updates over a message,
          # never a re-render, so the button holds as steady as the gear.
          # Unconditional, unlike the exhibit entries it also drives: the
          # menu now carries the cohort xlsx, which needs neither ggplot2 nor
          # officer, so gating the state message on the exhibit would leave
          # the whole menu hidden on a deployment that can still write the
          # one file people asked for.
          shiny::observe({
            scoped <- r_scoped_dm()
            pp_send(session, "dl_menu_state", list(
                single = isTRUE(scoped$single),
                picked = if (isTRUE(scoped$single)) scoped$picked else "",
                n = length(pp_subject_ids(r_norm_dm()))
              )
            )
          })

          # The data-derived half of the gear popover (see the uiOutput
          # above). This one SHOULD track the data -- what a study collects is
          # exactly what it reports -- it just must not drag the gear button
          # with it.
          output$gear_coverage <- shiny::renderUI({
            vizs <- r_cohort_vizs()  # req()s until a dm has arrived
            pp_gear_coverage_ui(pp_coverage_report(r_norm_dm(), vizs),
                                r_roles())
          })
          # The popover is display:none until the gear is clicked, so Shiny
          # would suspend this output and leave the coverage list blank on the
          # first open. It is a handful of divs; render it with the header.
          shiny::outputOptions(output, "gear_coverage",
                               suspendWhenHidden = FALSE)

          # The placeholder's one live sentence (see pp_empty_hint above).
          # Only mounted while the placeholder is, so it needs no suspend
          # override: when it is hidden there is nothing to say.
          output$pp_empty_hint <- shiny::renderUI({
            total <- r_cohort_total()
            shiny::req(!is.null(total))
            if (isTRUE(total > 1L)) {
              paste0("Pick one of ", total,
                     " patients above, or drill down on a chart")
            } else {
              "No patient data in the incoming tables"
            }
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
            if (!isTRUE(st$single)) {
              return(pp_chart_area_ui(session$ns, FALSE, character()))
            }
            pp_chart_area_ui(session$ns, TRUE,
                             intersect(r_selected(), names(r_available())))
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
            # settings$roles; for the severity and indication roles the board
            # scale map's colors ride along as settings$sev_colors /
            # settings$indc_colors (render-time only,
            # r_viz_settings is untouched; each viz falls back to its own
            # constants when no map / no binding resolves). The assembly
            # itself is pp_viz_exhibit_settings() -- shared with the static
            # exhibit path (downloads, deck export), so the live panel and
            # its printed twin read identical settings.
            viz_settings <- pp_viz_exhibit_settings(
              viz, r_slot_settings[[viz_id]], r_roles(), dm_obj,
              scale_map = r_scale_map(),
              arm_colors = r_arm_colors(),
              cycle_anchors = if ("cycle" %in% (viz$uses %||% character())) {
                r_cycle_anchors()
              },
              smooth = r_smooth()
            )

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

            # Per-viz download menu, only for vizs with a static twin and
            # only when the writers can run (ggplot2 + blockr.viz). The
            # files are re-derived from state via viz$exhibit -- the same
            # rendering a deck slide gets -- never the echarts canvas.
            # Formats follow the twin's kind: a plot downloads as a
            # picture, a table as a sheet / page / native slide table. A
            # format whose writer is missing is left out, not disabled.
            download_ui <- pp_slot_download_ui(viz, viz_id, session$ns)

            list(
              chart = chart,
              header = pp_slot_header_ui(viz, viz_id, controls_ui, legend_ui,
                                         download_ui)
            )
          }

          # The static exhibit behind one viz's download buttons: the same
          # inputs the live render gets, handed to the viz's `exhibit` twin
          # instead of its `render`. Re-derived from state at click time --
          # the file and a deck slide of this block are the same picture.
          build_slot_exhibit <- function(viz_id) {
            scoped <- r_scoped_dm()
            dm_obj <- scoped$dm
            shiny::req(inherits(dm_obj, "dm"), isTRUE(scoped$single))
            viz <- r_available()[[viz_id]]
            shiny::req(!is.null(viz), is.function(viz$exhibit))
            time_range <- r_time_range()
            shiny::req(time_range)
            ref_ms <- r_ref_ms()
            tl_mode <- r_timeline_mode()
            if (identical(tl_mode, "rday") && is.na(ref_ms)) {
              tl_mode <- "date"
            }
            settings <- pp_viz_exhibit_settings(
              viz, r_viz_settings()[[viz_id]], r_roles(), dm_obj,
              scale_map = r_scale_map(),
              arm_colors = r_arm_colors(),
              cycle_anchors = if ("cycle" %in% (viz$uses %||% character())) {
                r_cycle_anchors()
              },
              smooth = r_smooth()
            )
            p <- viz$exhibit(dm_obj, time_range, settings, ref_ms, tl_mode)
            shiny::req(!is.null(p))
            p
          }

          slot_file_stem <- function(viz_id) {
            scoped <- r_scoped_dm()
            subj <- if (isTRUE(scoped$single)) scoped$picked else NULL
            paste(
              c(viz_id, if (!is.null(subj)) gsub("[^A-Za-z0-9_-]+", "-",
                                                 subj)),
              collapse = "-"
            )
          }

          # The block-level downloads: the whole profile through
          # pp_patient_exhibit(), from the same live state the panels
          # render. Two scopes, both offered in the menu: the picked
          # patient, and the whole incoming cohort (one group of visuals
          # per patient -- what a deck built over this block shows).
          build_profile_exhibit <- function(scope = c("patient", "cohort")) {
            scope <- match.arg(scope)
            scoped <- r_scoped_dm()
            shiny::req(inherits(scoped$dm, "dm"))
            if (identical(scope, "patient")) {
              shiny::req(isTRUE(scoped$single))
            }
            ex <- pp_patient_exhibit(
              if (identical(scope, "patient")) scoped$dm else r_norm_dm(),
              selected = r_selected(),
              viz_settings = r_viz_settings(),
              timeline_mode = r_timeline_mode(),
              show_prestudy = r_show_prestudy(),
              smooth = r_smooth(),
              roles = r_declared(),
              scale_map = r_scale_map(),
              title = if (identical(scope, "patient")) {
                paste("Patient", scoped$picked)
              }
            )
            shiny::req(!is.null(ex))
            ex
          }

          profile_file_stem <- function(scope) {
            scoped <- r_scoped_dm()
            if (identical(scope, "patient") && isTRUE(scoped$single)) {
              paste0("patient-profile-",
                     gsub("[^A-Za-z0-9_-]+", "-", scoped$picked))
            } else {
              "patient-profiles"
            }
          }

          # The cohort as a list. Deliberately the SAME frame the sidebar
          # rows are built from (pp_cohort_frame()), so what you export is
          # what you were looking at -- a second column set assembled here
          # would drift from the list the first time either changed.
          #
          # The column set is a v1 default, not a decision: the frame is the
          # seam, so a study-configured or panel-derived set later replaces
          # one function and leaves this handler alone. Panel-derived is the
          # obvious next step and needs one thing first -- a panel is a
          # per-patient series and a row is one value, so somebody has to say
          # which value (last, worst, baseline, change) before a picked
          # panel can name a column.
          output$dl_cohort_xlsx <- shiny::downloadHandler(
            filename = function() {
              paste0("cohort-", format(Sys.Date(), "%Y%m%d"), ".xlsx")
            },
            content = function(file) {
              frame <- r_cohort_frame()
              # Export in the order on screen. A file sorted differently
              # from the list it came from is a small betrayal that costs
              # someone twenty minutes.
              frame <- frame[pp_cohort_order(frame, r_cohort_sort(),
                                             r_cohort_marks()), ,
                             drop = FALSE]
              blockr.viz::write_annotated_xlsx(frame, file)
            }
          )

          output$dl_profile_pptx <- shiny::downloadHandler(
            filename = function() {
              paste0(profile_file_stem("patient"), ".pptx")
            },
            content = function(file) {
              blockr.viz::write_exhibit_pptx(
                build_profile_exhibit("patient"), file
              )
            }
          )
          output$dl_profile_html <- shiny::downloadHandler(
            filename = function() {
              paste0(profile_file_stem("patient"), ".html")
            },
            content = function(file) {
              blockr.viz::write_exhibit_html(
                build_profile_exhibit("patient"), file,
                title = paste("Patient", r_scoped_dm()$picked)
              )
            }
          )
          output$dl_cohort_pptx <- shiny::downloadHandler(
            filename = function() {
              paste0(profile_file_stem("cohort"), ".pptx")
            },
            content = function(file) {
              blockr.viz::write_exhibit_pptx(
                build_profile_exhibit("cohort"), file
              )
            }
          )
          output$dl_cohort_html <- shiny::downloadHandler(
            filename = function() {
              paste0(profile_file_stem("cohort"), ".html")
            },
            content = function(file) {
              blockr.viz::write_exhibit_html(
                build_profile_exhibit("cohort"), file,
                title = "Patient profiles"
              )
            }
          )

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
                # A panel is BUILT once and then UPDATED in place.
                #
                # The slot output re-renders when the stack changes (a
                # panel added, removed or reordered), which is when its
                # placeholder is new and a widget has to be created, and
                # when the panel changes KIND: an echarts chart giving way
                # to a plain-HTML panel or back. Otherwise a patient switch
                # or a settings change leaves the widget where it is and
                # sends a `slot` message instead: the new header as HTML
                # and the new chart option, which the client applies with
                # setOption(). No canvas is torn down, so nothing blinks
                # and there is nothing to photograph.
                slot_r <- shiny::reactive(render_viz_slot(vid))
                slot_key <- shiny::reactiveVal(0L)
                slot_kind <- NULL
                kind_of <- function(chart) {
                  if (inherits(chart, "echarts4r")) "echarts" else "html"
                }
                output[[paste0("viz_slot_", vid)]] <- shiny::renderUI({
                  slot_key()
                  r_selected()
                  r_available()
                  s <- shiny::isolate(slot_r())
                  slot_kind <<- kind_of(s$chart)
                  shiny::tagList(s$header,
                                 shiny::div(class = "pp-chart-body", s$chart))
                })
                # Only for a panel that is on the profile: the observer
                # would otherwise render every registered panel on every
                # pick, on screen or not (measured: four off-screen
                # panels cost 31 of 56ms per pick).
                shiny::observeEvent({
                  if (vid %in% r_selected()) slot_r() else NULL
                }, {
                  s <- slot_r()
                  if (identical(slot_kind, "echarts") &&
                        identical(kind_of(s$chart), "echarts")) {
                    pp_send(session, "slot",
                            pp_slot_update(vid, s$header, s$chart))
                  } else {
                    # Not live as a chart yet, or no longer one: draw it
                    # in full.
                    slot_key(slot_key() + 1L)
                  }
                })
                output[[paste0("dl_png_", vid)]] <- shiny::downloadHandler(
                  filename = function() {
                    paste0(slot_file_stem(vid), ".png")
                  },
                  content = function(file) {
                    blockr.viz::write_exhibit_png(
                      build_slot_exhibit(vid), file
                    )
                  }
                )
                output[[paste0("dl_pptx_", vid)]] <- shiny::downloadHandler(
                  filename = function() {
                    paste0(slot_file_stem(vid), ".pptx")
                  },
                  content = function(file) {
                    viz <- r_available()[[vid]]
                    scoped <- r_scoped_dm()
                    subj <- if (isTRUE(scoped$single)) scoped$picked
                    blockr.viz::write_exhibit_pptx(
                      build_slot_exhibit(vid), file,
                      title = paste(
                        c(if (!is.null(subj)) paste0("Patient ", subj, ":"),
                          pp_viz_full_label(viz)),
                        collapse = " "
                      )
                    )
                  }
                )
                # Table-kind twins (a data frame, not a plot) add the table
                # block's other formats; registered for every slot, offered
                # only where the menu shows them.
                output[[paste0("dl_xlsx_", vid)]] <- shiny::downloadHandler(
                  filename = function() {
                    paste0(slot_file_stem(vid), ".xlsx")
                  },
                  content = function(file) {
                    blockr.viz::write_annotated_xlsx(
                      build_slot_exhibit(vid), file
                    )
                  }
                )
                output[[paste0("dl_html_", vid)]] <- shiny::downloadHandler(
                  filename = function() {
                    paste0(slot_file_stem(vid), ".html")
                  },
                  content = function(file) {
                    viz <- r_available()[[vid]]
                    scoped <- r_scoped_dm()
                    subj <- if (isTRUE(scoped$single)) scoped$picked
                    blockr.viz::write_exhibit_html(
                      build_slot_exhibit(vid), file,
                      title = paste(
                        c(if (!is.null(subj)) paste0("Patient ", subj, ":"),
                          pp_viz_full_label(viz)),
                        collapse = " "
                      )
                    )
                  }
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
              show_prestudy = r_show_prestudy,
              smooth = r_smooth
            )
          )
        }
      )
    },
    ui = pp_block_ui,
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
                      "show_prestudy", "smooth"),
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
