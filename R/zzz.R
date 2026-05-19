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
  invisible(NULL)
}

register_pharma_blocks <- function() {
  blockr.core::register_blocks(
    "new_patient_profile_block",
    name = "Patient Profile",
    description =
      "Stacked clinical charts with searchable sidebar (dm input)",
    category = "plot",
    icon = "person-badge",
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
  structure(
    c(
      selected = paste0(
        "Array of visualization IDs to display. ",
        "Static IDs: ",
        "\"patient_overview\" (treatment period + AE bars + milestones from ADSL/adae), ",
        "\"ae_gantt\" (adverse events Gantt chart from adae), ",
        "\"adas_trajectory\" (ADAS-Cog score trajectory from adqsadas), ",
        "\"npix_radar\" (NPI-X radar chart from adqsnpix), ",
        "\"ortho_bp\" (orthostatic blood pressure from advs), ",
        "\"questionnaire_heatmap\" (heatmap of questionnaire scores). ",
        "Findings group IDs (generated from data; lab groups source from ",
        "adlbc/adlbh when those tables exist, or the combined adlb otherwise): ",
        "\"liver_panel\" (ALT, AST, BILI, GGT, ALP/ALKPH), ",
        "\"renal_panel\" (BUN, CREAT, URATE), ",
        "\"electrolytes\" (SODIUM, K/POTAS, CL, CA, PHOS), ",
        "\"metabolic\" (GLUC, CHOL/CHOLES, PROT, ALB), ",
        "\"muscle_enzymes\" (CK), ",
        "\"cbc\" (WBC, RBC, HGB, HCT, PLAT), ",
        "\"rbc_indices\" (MCV, MCH, MCHC), ",
        "\"wbc_differential\" (LYM/LYMPH, MONO, EOS, BASO), ",
        "\"rbc_morphology\" (ANISO, MACROCY, MICROCY, POIKILO, POLYCHR), ",
        "\"blood_pressure\" (SYSBP, DIABP from advs), ",
        "\"pulse\" (PULSE from advs), ",
        "\"temperature\" (TEMP from advs), ",
        "\"anthropometrics\" (HEIGHT, WEIGHT from advs). ",
        "PARAMCDs not in a pre-defined group get auto-generated IDs like ",
        "\"adlb_paramcd\" (e.g. \"adlb_trig\"). ",
        "Only use IDs from this list — do NOT put raw table names (adcm, ",
        "adae, adlb) or PARAMCDs in `selected`. ",
        "Set to the IDs the user wants to see. patient_overview is ",
        "usually kept as the first element."
      ),
      viz_settings = paste0(
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
      )
    ),
    examples = list(
      selected = list(
        "patient_overview", "liver_panel", "blood_pressure"
      ),
      viz_settings = list(
        liver_panel = list(items = list("ALT", "AST")),
        adas_trajectory = list(items = list("ACTOT"), chg = FALSE),
        questionnaire_heatmap = list(domain = "adqsadas", value = "AVAL")
      )
    ),
    prompt = paste(
      "This block displays stacked clinical visualizations for a single",
      "patient. The user controls WHICH vizs are shown via `selected`",
      "and HOW they look via `viz_settings`.",
      "\n\n**CRITICAL — the input dm is ALREADY filtered to ONE patient.**",
      "Every table (adsl, adae, advs, adlbc, ...) contains only that one",
      "patient's rows. Do NOT add a `USUBJID == ...` filter to your",
      "data_query — there is exactly one USUBJID in scope and you do not",
      "need to know its value. Just query the table directly. Adding a",
      "predicate like `adae$USUBJID == 'specific_patient_id'` will return",
      "zero rows and lead you to falsely conclude the patient has no",
      "records of that type. If you actually need the USUBJID for your",
      "explanation, read it from `adsl$USUBJID` (a single value).",
      "\n\nFindings (labs, vitals) are split into clinically meaningful",
      "groups instead of monolithic per-domain charts. Each group shows",
      "only its PARAMCDs and has an 'items' chip control to toggle",
      "individual parameters within the group.",
      "\n\nWhen the user asks about a clinical domain, select the",
      "relevant visualization(s):",
      "- Adverse events/AEs/safety -> ae_gantt",
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
      "\n\nUse viz_settings to filter items within a group. For example,",
      "to show only ALT and AST from the liver panel:",
      "viz_settings = {liver_panel: {items: [\"ALT\", \"AST\"]}}.",
      "Otherwise omit viz_settings or set it to {}.",
      "\n\nWhen the user asks a clinical question (e.g. 'how is the",
      "sodium?', 'any liver issues?', 'what's the kidney function",
      "like?'), use the data exploration capability to look at the",
      "actual patient values BEFORE answering. IMPORTANT: query",
      "whichever lab table is actually in the dm — it may be split",
      "(adlbc/adlbh) or combined (adlb). Look at the Input Data table",
      "list above and use that exact name. PARAMCDs also vary: ALP may",
      "be coded ALKPH, K may be POTAS, CHOL may be CHOLES, LYM may be",
      "LYMPH. Probe the PARAMCD column first if unsure. Example with",
      "combined adlb:",
      "\n```data_query",
      "\nadlb[adlb$PARAMCD %in% c('ALT','AST','BILI','ALP','ALKPH','GGT'), c('PARAMCD','AVISIT','AVAL','BASE','CHG','ADT','A1LO','A1HI')]",
      "\n```",
      "\nFor adverse events, query adae directly without a USUBJID filter:",
      "\n```data_query",
      "\nadae[, c('AETERM','AEDECOD','AESEV','AEBODSYS','ASTDT','AENDT')]",
      "\n```",
      "\nThen summarize findings (trends, out-of-range values, notable",
      "changes) in your explanation.",
      "\n\nIMPORTANT: In your explanation before the JSON, do more than",
      "just describe the parameter choices. Actually answer the user's",
      "clinical question using the data provided. For example, if the",
      "user asks \"what adverse effects does this patient have?\",",
      "summarize the AEs you see in the data (terms, severity, timing)",
      "and then say you're showing the AE Gantt chart. If they ask",
      "about labs, comment on notable values or trends. The user sees",
      "your explanation as a chat message — make it clinically useful."
    )
  )
}
