# A toy study for the patient timeline's tests.

# Patient A: two AEs (one severe, one serious), a con med, two dose periods,
# PR then PD, stopped for an adverse event. Patient B: one AE, SD, died.
# Patient C: on treatment, no AE, CR.
pt_toy <- function() {
  adsl <- data.frame(
    USUBJID = c("A", "B", "C"),
    ACTARM = c("Drug", "Placebo", "Drug"),
    TRT01A = c("Drug 54 mg", "Placebo", "Drug 81 mg"),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
    TRTEDT = as.Date(c("2024-02-29", "2024-01-20", NA)),
    LSTALVDT = as.Date(c("2024-03-30", "2024-01-25", "2024-04-30")),
    DTHDT = as.Date(c(NA, "2024-01-25", NA)),
    DCTREAS = c("ADVERSE EVENT", "DEATH", NA),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("A", "A", "B"),
    AEDECOD = c("HEADACHE", "NAUSEA", "FALL"),
    AETERM = c("headache", "nausea", "fall"),
    AEBODSYS = c("NERVOUS SYSTEM DISORDERS", "GASTROINTESTINAL DISORDERS",
                 "INJURY"),
    AESEV = c("MILD", "SEVERE", "MODERATE"),
    ATOXGR = c("1", "3", "2"),
    AESER = c("N", "Y", "N"),
    TRTEMFL = "Y",
    ASTDY = c(2, 5, 3),
    AENDY = c(10, NA, 4),
    stringsAsFactors = FALSE
  )
  adcm <- data.frame(
    USUBJID = "A", CMDECOD = "PARACETAMOL", CMTRT = "Tylenol",
    CMCLAS = "ANALGESICS", ASTDY = 2, AENDY = 6, stringsAsFactors = FALSE
  )
  adex <- data.frame(
    USUBJID = c("A", "A", "B", "C"),
    EXTRT = c("DRUG", "DRUG", "PLACEBO", "DRUG"),
    EXDOSE = c(54, 81, 0, 81),
    EXDOSU = "mg",
    ASTDT = as.Date(c("2024-01-01", "2024-01-31", "2024-01-01",
                      "2024-01-01")),
    AENDT = as.Date(c("2024-01-30", "2024-02-29", "2024-01-20", NA)),
    stringsAsFactors = FALSE
  )
  adrs <- data.frame(
    USUBJID = c("A", "A", "B", "C"),
    PARAMCD = "OVR",
    AVALC = c("PR", "PD", "SD", "CR"),
    ADT = as.Date(c("2024-01-29", "2024-02-26", "2024-01-15",
                    "2024-02-01")),
    stringsAsFactors = FALSE
  )
  list(adsl = adsl, adae = adae, adcm = adcm, adex = adex, adrs = adrs)
}
