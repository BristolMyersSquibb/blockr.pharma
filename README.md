# blockr.pharma

Open, pharma-specific blocks for [blockr](https://blockr-org.github.io/blockr.site/).

The package provides a no-code **patient profile** for clinical safety
review on CDISC ADaM data. Point it at a `dm` of ADaM tables filtered to
one subject and it renders stacked, time-aligned visualizations: adverse
events, lab panels, vital signs, and questionnaire scores, with a
searchable sidebar to toggle views and adjust per-view settings.

## Installation

```r
# install.packages("pak")
pak::pak("blockr-org/blockr.pharma")
```

## Usage

The block takes a `dm` whose tables follow the ADaM standard (`adsl`,
`adae`, `advs`, `adlb`, ...). It expects the `dm` to be scoped to a
single subject; in a board this is normally done by an upstream
drill-down. The example below builds a one-subject `dm` directly from the
public [pharmaverseadam](https://pharmaverse.github.io/pharmaverseadam/)
CDISC pilot data.

```r
library(blockr.pharma)
library(pharmaverseadam)
library(dm)

one <- adsl$USUBJID[1]
pp_dm <- dm(
  adsl = adsl[adsl$USUBJID == one, ],
  adae = adae[adae$USUBJID == one, ],
  advs = advs[advs$USUBJID == one, ]
)

blockr.core::serve(
  new_patient_profile_block(selected = c("patient_overview", "ae_gantt")),
  data = pp_dm
)
```

## What it shows

- **Patient overview**: treatment period, adverse event bars, milestones
  from ADSL and ADAE.
- **AE Gantt**: adverse events over time from ADAE.
- **Lab and vitals panels**: clinically grouped findings (liver, renal,
  electrolytes, CBC, blood pressure, ...) from ADLB and ADVS, with
  per-parameter toggles.
- **Questionnaire views**: ADAS-Cog trajectory, NPI-X radar, and a
  questionnaire-score heatmap from ADQS.

All views share aligned time axes, switchable between calendar dates and
relative study day.

## License

GPL (>= 3).
