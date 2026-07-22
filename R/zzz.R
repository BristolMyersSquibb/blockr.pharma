# `USUBJID`: a dm::dm_filter() NSE column reference (adsl$USUBJID), not a
# real global -- R CMD check can't see through the quasiquotation.
utils::globalVariables("USUBJID")

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "blockr-pharma",
    system.file("assets", package = pkgname)
  )
  shiny::addResourcePath(
    "blockr-pharma-css",
    system.file("css", package = pkgname)
  )
  register_pharma_blocks()
  register_patient_profile_ai_effect()
  invisible(NULL)
}

#' @importFrom blockr.core register_blocks new_arg_specs new_arg_spec
#'   arg_array arg_string
register_pharma_blocks <- function() {
  register_blocks(
    "new_patient_profile_block",
    name = "Patient Profile",
    # One line. `description` is the SHORT summary by contract (see
    # ?register_block: `details` is "the longer human-facing description ...
    # complementing the short `description`"), and it is rendered straight
    # into human surfaces -- the block header popover, and bare `title=`
    # tooltips on the header subtitle and the sidebar card. It used to carry
    # 540 characters of model-facing guidance, against a 72-character median
    # across the 49 registered blocks; every one of those sentences now lives
    # in pp_block_guidance() below, which lands in the SAME model prompt
    # (blockr.ai pastes description, then guidance, two lines apart), so the
    # model lost nothing.
    description = paste0(
      "Stacked per-patient clinical charts (AE Gantt, labs, vitals) from a ",
      "CDISC dm."
    ),
    category = "plot",
    icon = "person-badge",
    guidance = pp_block_guidance(),
    arguments = list(
      pp_block_arguments()
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}

#' Build arguments metadata for the patient profile block
#' @noRd
pp_block_arguments <- function() {
  # Findings group ids and their PARAMCDs, straight from the group templates
  # (the single copy; a hand-written list here once drifted from them).
  group_desc <- paste(
    vapply(pp_findings_groups(), function(g) {
      sprintf("\"%s\" (%s)", g$id, paste(g$paramcds, collapse = ", "))
    }, character(1L)),
    collapse = ", "
  )

  new_arg_specs(
    selected = new_arg_spec(
      paste0(
        "Array of visualization IDs to display. ",
        "Static IDs: ",
        "\"patient_overview\" (treatment period, exposure/dosing bars, AE bars, ",
        "visit ticks + milestones from ADSL/adae/adex), ",
        "\"ae_gantt\" (adverse events Gantt chart from adae), ",
        "\"cm_gantt\" (concomitant medications Gantt chart from adcm), ",
        "\"adas_trajectory\" (ADAS-Cog score trajectory from adqsadas), ",
        "\"npix_radar\" (NPI-X radar chart from adqsnpix), ",
        "\"ortho_bp\" (orthostatic blood pressure from advs), ",
        "\"questionnaire_heatmap\" (heatmap of questionnaire scores). ",
        "Findings group IDs (generated from data; lab groups source from ",
        "adlbc/adlbh when those tables exist, or the combined adlb ",
        "otherwise; the PARAMCD lists include sponsor synonyms): ",
        group_desc, ". ",
        "PARAMCDs not in a pre-defined group get auto-generated IDs like ",
        "\"adlb_paramcd\" (e.g. \"adlb_trig\"). ",
        "Only use IDs from this list \u2014 do NOT put raw table names (adcm, ",
        "adae, adlb) or PARAMCDs in `selected`. ",
        "Set to the IDs the user wants to see. patient_overview is ",
        "usually kept as the first element."
      ),
      example = list(
        "patient_overview", "liver_panel", "blood_pressure"
      ),
      type = arg_array(arg_string())
    ),
    # An arbitrary-key map (keys are viz IDs, values are setting objects of
    # varying shape). The JSON-Schema subset has no open-ended object, so
    # `type` is left unset and the consumer infers the map from the example.
    viz_settings = new_arg_spec(
      paste0(
        "Object with per-visualization settings. Keys are viz IDs, values ",
        "are setting objects. Only set for vizs that have controls. ",
        "Available settings: ",
        "Findings groups (liver_panel, cbc, blood_pressure, etc.): ",
        "{items: array of PARAMCDs to show (e.g. [\"ALT\", \"AST\"])}. ",
        "adas_trajectory: {items: array of PARAMCDs (e.g. [\"ACTOT\", ",
        "\"ACITM01\"]), chg: boolean (true=change from baseline)}. ",
        "npix_radar: {visits: array of visit names (e.g. [\"Baseline\", ",
        "\"Week 8\"])}. ",
        "ortho_bp: {visits: array of visit names}. ",
        "questionnaire_heatmap: {domain: \"adqsadas\" or \"adqsnpix\", ",
        "value: \"AVAL\" or \"CHG\"}."
      ),
      example = list(
        liver_panel = list(items = list("ALT", "AST")),
        adas_trajectory = list(items = list("ACTOT"), chg = FALSE),
        questionnaire_heatmap = list(domain = "adqsadas", value = "AVAL")
      )
    ),
    subject = new_arg_spec(
      paste0(
        "USUBJID of the patient to display, as a single string. Only needed ",
        "when the input dm carries MORE THAN ONE subject: the block then ",
        "filters the dm down to this patient. A dm that already holds ",
        "exactly one subject renders that subject and ignores this. ",
        "The value MUST be an existing USUBJID in adsl -- probe the data ",
        "first (e.g. `unique(adsl$USUBJID)`) rather than inventing an id; ",
        "an id that is not in the cohort is discarded and the block shows a ",
        "\"No patient selected\" placeholder. Omit to leave the user's ",
        "current selection untouched."
      ),
      example = "01-701-1015",
      type = arg_string()
    )
  )
}

#' Construction guidance for the patient profile block
#' @noRd
pp_block_guidance <- function() {
  paste(
      "This block displays stacked clinical visualizations for a single",
      "patient (ADAS trajectory, AE gantt, lab / vital panels). The user",
      "controls WHICH patient via `subject`, WHICH vizs are shown via",
      "`selected`, and HOW they look via `viz_settings`.",
      # Moved here from the registry `description`, which is a human-facing
      # one-liner and was the ONLY place these two steers existed. Both reach
      # the model in the same system prompt either way.
      #
      # OUT OF SCOPE is the phrase the harness's HOW TO WORK section looks
      # for: it is what licenses replying without configuring. Saying "use a
      # chart block instead" on its own does not work -- the model reads it,
      # then configures the nearest single-patient approximation anyway,
      # because the generic "configuring is an ACTION, do not answer in plain
      # language and stop" rule sits later in the prompt and wins.
      "\n\n**Cohort questions are OUT OF SCOPE for this block.** It shows one",
      "patient at a time; a group-level or treatment-arm request (e.g. \"mean",
      "ADAS-Cog change from baseline by arm\", \"compare the arms\", \"which",
      "patients have the most AEs\") is one this block CANNOT answer, however",
      "the input dm is shaped -- a cohort dm coming in does not make it a",
      "cohort view. Do NOT configure the closest single-patient",
      "approximation and caveat it: that silently answers a different",
      "question than the one asked, and the chart on screen looks like a",
      "reply. Instead leave the config alone and say that this block shows a",
      "single patient and that a chart block on a pulled table is the right",
      "tool for arm-level trends.",
      "\n\nIt takes either a dm already filtered to one subject (e.g. from a",
      "drilldown chart into `dm_filter_by_data(table = \"adsl\", key_col =",
      "\"USUBJID\")`) or a full cohort dm, in which case `subject` names the",
      "USUBJID to display and the block filters the dm down to that patient.",
      "\n\n**CRITICAL \u2014 check how many patients the input dm holds before",
      "you query it.** Start with `unique(adsl$USUBJID)`.",
      "\n\n- **Exactly one USUBJID.** An upstream block already filtered to",
      "one patient. Every table (adsl, adae, advs, adlbc, ...) contains",
      "only that patient's rows. Do NOT add a `USUBJID == ...` filter to",
      "your data_query: a predicate like",
      "`adae$USUBJID == 'specific_patient_id'` will return zero rows and",
      "lead you to falsely conclude the patient has no records of that",
      "type. Just query the table directly, and leave `subject` unset.",
      "\n- **Many USUBJIDs.** The input is a cohort. This is a fully",
      "supported, normal input: a multi-patient dm does NOT mean the block",
      "cannot render. The block filters the cohort down to whichever USUBJID",
      "`subject` names -- it runs `dm::dm_filter(data, adsl = USUBJID ==",
      "<subject>)` internally -- and renders that one patient.",
      "\n  - **`subject` already set to a USUBJID in the cohort:** the block",
      "IS ALREADY showing that patient. Do NOT ask for a USUBJID, and do NOT",
      "say the block \"will not render\" or \"is not filtered to a specific",
      "patient\" -- that is wrong; it is filtered. Just answer the question.",
      "Scope your data queries to that same subject (e.g.",
      "`adae[adae$USUBJID == '01-701-1015', ]`), otherwise you are reading",
      "the whole trial and will report another patient's events as this",
      "one's.",
      "\n  - **`subject` unset (or naming an id absent from the cohort) and",
      "the user NAMED a patient:** set `subject` to that USUBJID, copied",
      "verbatim from `unique(adsl$USUBJID)` -- never invented, never guessed.",
      "\n  - **`subject` unset AND the user did not name a patient:** only",
      "then ask which patient, rather than picking for them.",
      "\n\nFindings (labs, vitals) are split into clinically meaningful",
      "groups instead of monolithic per-domain charts. Each group shows",
      "only its PARAMCDs and has an 'items' chip control to toggle",
      "individual parameters within the group.",
      "\n\nWhen the user asks about a clinical domain, select the",
      "relevant visualization(s):",
      "- Adverse events/AEs/safety -> ae_gantt",
      "- Medications/conmeds/concomitant meds -> cm_gantt",
      "- Liver function/hepatic/ALT/AST -> liver_panel",
      "- Renal function/kidney/creatinine -> renal_panel",
      "- Electrolytes/sodium/potassium -> electrolytes",
      "- Metabolic/glucose/cholesterol -> metabolic",
      "- CK/muscle enzymes -> muscle_enzymes",
      "- CBC/blood counts/hemoglobin/platelets -> cbc",
      "- RBC indices/MCV/MCH -> rbc_indices",
      "- WBC differential/lymphocytes -> wbc_differential",
      "- Blood pressure/BP -> blood_pressure",
      "- Pulse/heart rate -> pulse",
      "- Temperature -> temperature",
      "- Height/weight/BMI -> anthropometrics",
      "- ADAS-Cog/cognition/cognitive scores -> adas_trajectory",
      "- NPI-X/neuropsychiatric/behavior -> npix_radar",
      "- Orthostatic/positional BP -> ortho_bp",
      "- Score heatmap/item-level overview -> questionnaire_heatmap",
      "- Treatment/dosing/arm -> patient_overview",
      "\n\nAlways keep patient_overview as the first element of",
      "selected. Add the requested vizs after it. If the user says",
      "\"show me liver labs\", set selected to",
      "[\"patient_overview\", \"liver_panel\"].",
      "If the user says \"show all labs\", include all lab group IDs.",
      "If the user says \"show everything\", include all available IDs.",
      "\n\nWhen the user names SEVERAL specific domains or measures, include a",
      "SEPARATE viz for EACH one -- do not drop any or assume one viz",
      "subsumes another. E.g. \"blood pressure and pulse\" -> include BOTH",
      "blood_pressure AND pulse; \"treatment timeline and adverse events\" ->",
      "patient_overview AND ae_gantt (patient_overview does NOT replace",
      "ae_gantt). Map every named item to its own ID from the list above.",
      "\n\nUse viz_settings to filter items within a group. For example,",
      "to show only ALT and AST from the liver panel:",
      "viz_settings = {liver_panel: {items: [\"ALT\", \"AST\"]}}.",
      "Otherwise omit viz_settings or set it to {}.",
      "\n\nWhen the user asks a clinical question (e.g. 'how is the",
      "sodium?', 'any liver issues?', 'what's the kidney function",
      "like?'), use the data exploration capability to look at the",
      "actual patient values BEFORE answering. IMPORTANT: query",
      "whichever lab table is actually in the dm \u2014 it may be split",
      "(adlbc/adlbh) or combined (adlb). Look at the Input Data table",
      "list above and use that exact name. PARAMCDs also vary: ALP may",
      "be coded ALKPH, K may be POTAS, CHOL may be CHOLES, LYM may be",
      "LYMPH. Probe the PARAMCD column first if unsure. Example with",
      "combined adlb:",
      "\n```data_query",
      "\nadlb[adlb$PARAMCD %in% c('ALT','AST','BILI','ALP','ALKPH','GGT'), c('PARAMCD','AVISIT','AVAL','BASE','CHG','ADT','A1LO','A1HI')]",
      "\n```",
      "\nFor adverse events on a single-patient dm, query adae directly",
      "without a USUBJID filter:",
      "\n```data_query",
      "\nadae[, c('AETERM','AEDECOD','AESEV','AEBODSYS','ASTDT','AENDT')]",
      "\n```",
      "\nOn a cohort dm, scope the same query to the selected subject:",
      "\n```data_query",
      "\nadae[adae$USUBJID == '01-701-1015', c('AETERM','AEDECOD','AESEV','AEBODSYS','ASTDT','AENDT')]",
      "\n```",
      "\nThen summarize findings (trends, out-of-range values, notable",
      "changes) in your explanation.",
      "\n\nIMPORTANT \u2014 your reply is a clinical answer, not a list of the",
      "views you configured. Selecting the right vizs is half the job; the",
      "other half is telling the clinician what the data SHOWS. A reply like",
      "\"I've updated the profile to show the AE timeline and liver labs\" is a",
      "failure: it names views, not findings. Instead probe the data and state",
      "the findings with their VALUES and TIMING \u2014 e.g. \"CK rose to 1860 at",
      "Week 6 (ULN 198), a ~9x elevation that later settled; AST was mildly up",
      "(73) while ALT and bilirubin stayed normal, pointing to muscle rather",
      "than liver.\" Name what is abnormal, and say what is NOT there when it is",
      "clinically reassuring (\"no renal impairment, hematology normal\").",
      "\n\nFor a broad, open question (\"what is wrong with this patient?\", \"how",
      "is this patient doing?\"), do BOTH: configure a wide safety set of vizs,",
      "AND give a short structured read \u2014 the most notable objective findings",
      "(labs/vitals out of range, with values), the adverse-event burden and",
      "whether treatment was discontinued, and a one-line bottom line. Ground",
      "every claim in a value you actually saw; never invent one."
    )
}
