# EU-SILC Pipeline

Reproducible R pipeline to build a consolidated, person-level, cross-national
panel from the **EU-SILC Cross-sectional User Database (UDB)**, and to derive
standard sociological/economic indicators on top of it: poverty thresholds,
at-risk-of-poverty (AROP) flags (before- and after-housing-costs), household
composition, work intensity, and anchored poverty.

The pipeline is split into small, independent R scripts so that anyone can
run only the part they need — e.g. just build the base dataset, or just
recompute poverty indicators with a different equivalence scale — without
having to run (or understand) the whole thing.

## What this is *not*

This repository does **not** contain any EU-SILC microdata. EU-SILC UDB files
are distributed by Eurostat/national statistical institutes under a
restricted-access agreement (research/scientific use). You must request
access yourself:
<https://ec.europa.eu/eurostat/web/microdata/european-union-statistics-on-income-and-living-conditions>

This repository only contains the R code used to process that data once you
already have it on disk.

## Repository structure

```
EUSILC/
├── R/
│   ├── 00_config_utils.R                 # paths, country/year list, shared helpers
│   ├── 01_build_base_cross.R             # consolidated person-level base dataset
│   ├── 02_poverty_thresholds_deflators.R # BHC/AHC poverty thresholds + HICP deflators
│   ├── 03_poverty_indicators.R           # equivalised income + AROP flags (BHC/AHC)
│   ├── 04_household_composition.R        # HX060 -> 6-category household typology
│   ├── 05_work_intensity.R               # household work intensity (bands + 0.50 cut)
│   └── 06_anchored_poverty.R             # AROP60 anchored to a fixed base year
├── docs/
├── LICENSE
├── requirements.R
├── .gitignore
└── README.md
```

> The scripts as distributed use `source("00_config_utils.R")` assuming they
> are run from inside `R/`. If you keep them at the repository root instead,
> that's fine too — just make sure `00_config_utils.R` is on the working
> directory / sourced first.

## Pipeline overview

Every script after `00` starts with `source("00_config_utils.R")`, reads its
inputs from `output_dir` (or the raw UDB files), and writes its outputs back
into `output_dir`. Scripts `03`, `04` and `05` are all independent and only
require the output of `01`; `06` additionally requires `02`.

| Script | Reads | Writes | What it does |
|---|---|---|---|
| `00_config_utils.R` | — | — | Paths, country list (`paises`), year range (`anios_cross`), shared helper functions (`ensure_cols`, `find_cross_file`, `nz`, `wprop`). Edit this file first. |
| `01_build_base_cross.R` | raw UDB `D/H/P/R` files | `cross_eu_base.csv/.rds/.parquet` | Merges the four EU-SILC register files (household register, household, personal register, personal) per country-year, discovers a common column schema across all waves, and adds age, household size, and head-of-household. |
| `02_poverty_thresholds_deflators.R` | raw UDB `D/H/P/R` files, `inflationratesEU.xlsx` | `thresholds_all_years_BHC_AHC.csv`, `hicp_defl_2012eq1_fromInflation.csv` | (A) Computes national 40/50/60/70% median-income poverty thresholds, before- and after-housing-costs. (B) Builds a HICP-based price deflator per country-year from annual inflation rates, chained from a fixed base year. |
| `03_poverty_indicators.R` | `cross_eu_base.csv`, `thresholds_all_years_BHC_AHC.csv` | `cross_eu_poverty.csv/.rds/.parquet` | Computes household disposable income and equivalised income (BHC/AHC), and flags AROP at 40/50/60/70% for both measures. |
| `04_household_composition.R` | `cross_eu_base.csv` | `household_composition.csv` | Reclassifies HX060 into a simplified 6-category household typology (adults × presence of children). |
| `05_work_intensity.R` | `cross_eu_base.csv` | `work_intensity_person.csv`, `work_intensity_country_year.csv` | Computes household work intensity from the monthly activity calendar, low/medium work-intensity flags, four work-intensity bands, and weighted country-year shares. |
| `06_anchored_poverty.R` | `cross_eu_poverty.csv`, `hicp_defl_2012eq1_fromInflation.csv` | `anchored_poverty_person.csv`, `anchored_poverty_country_year.csv` | Deflates equivalised income to base-year euros and compares it against the base-year 60% threshold, to separate real income growth from the "moving target" effect of a relative poverty line. |

## Requirements

- R >= 4.1
- Packages: `data.table`, `Hmisc`, `readxl`, `dplyr`, `tidyr`, `readr`,
  `stringr`, and optionally `arrow` (for Parquet output).

```r
source("requirements.R")
```

## Setup

1. Request and download the EU-SILC Cross UDB files from Eurostat.
2. Arrange them under a single root folder with the expected structure:
   ```
   <ruta_base_cross>/<COUNTRY>/<YEAR>/UDB_c<CC><YY><D|H|P|R>.csv[.gz]
   # e.g. .../Cross/ES/2018/UDB_cES18D.csv
   ```
3. Open `00_config_utils.R` and edit the two paths at the top:
   - `output_dir`: where all pipeline outputs will be written.
   - `ruta_base_cross`: the root folder from step 2.
   - (optional) place an `inflationratesEU.xlsx` file in `output_dir` if you
     plan to run `02_poverty_thresholds_deflators.R` (one column identifying
     the country and one column per year, with annual inflation in %).
4. Adjust `paises` (country list) and `anios_cross` (year range) if needed —
   defaults cover all 26 EU-SILC countries and 2012-2024.
5. Run the scripts in order (or just the ones you need):
   ```r
   source("01_build_base_cross.R")
   source("02_poverty_thresholds_deflators.R")
   source("03_poverty_indicators.R")
   source("04_household_composition.R")
   source("05_work_intensity.R")
   source("06_anchored_poverty.R")
   ```

## Quick start: computing a national AROP rate

Once `03_poverty_indicators.R` has run, everything you need is in
`cross_eu_poverty.csv`:

```r
library(data.table)
DT <- fread(file.path(output_dir, "cross_eu_poverty.csv"))

# Weighted AROP60 rate (BHC) by country and year
DT[, .(arop60_rate = sum(PB040 * arop60_BHC, na.rm = TRUE) / sum(PB040, na.rm = TRUE)),
   by = .(country, year)][order(country, year)]
```

## Methodology

### Unit of analysis

The analytical universe is the **personal register (P file)**: every row is
a person, in a given country and year, linked to their household via `hhid`.
Household-level variables (income, housing costs, household type) are merged
onto every person in that household.

### Equivalised household income

Household disposable income is equivalised using the **modified OECD scale**:

```
eqs = 1 + 0.5 * (adults - 1) + 0.3 * children      (if adults >= 1)
eqs = 1 + 0.3 * max(children - 1, 0)                (if adults == 0)

hystd = household disposable income / eqs
```

where `adults` = household members aged 14+ and `children` = household
members under 14 (EU-SILC convention).

### Before- vs. after-housing-costs (BHC / AHC)

- **BHC**: household disposable income (`HY020`) as reported.
- **AHC**: `HY020` minus annualised housing costs (`HH070`, monthly ×12),
  floored at 0.

Both are equivalised and used to compute separate poverty thresholds and
AROP flags (`*_BHC` / `*_AHC` suffixes). Unsuffixed variables
(`hydisp`, `hystd`, `thresh40..70`, `arop40..70`) are kept as legacy aliases
that default to the **BHC** measure, for backward compatibility.

### Poverty thresholds and AROP

For each country-year, the national median of `hystd` (weighted by the
person weight `PB040`) defines the poverty thresholds at 40/50/60/70% of
the median. A person is At Risk Of Poverty (AROP) at a given percentage if
their household's equivalised income falls below the corresponding
threshold. The 60% threshold is the standard EU indicator.

### Household composition (HX060 → `hhcomp`)

EU-SILC's `HX060` household-type variable is collapsed into a simplified
6-category typology based on number of adults and presence of children:

| `hhcomp` | Description | `HX060` codes |
|---|---|---|
| 1 | 1 adult, no children | 5 |
| 2 | 2 adults, no children | 6, 7 |
| 3 | 3+ adults, no children | 8 |
| 4 | 1 adult with children | 9 |
| 5 | 2 adults with children | 10, 11, 12 |
| 6 | 3+ adults with children | 13 |

### Work intensity

Work intensity approximates the (Very) Low Work Intensity Europe 2020/2030
indicator, adapted to the monthly activity-calendar variables available in
the Cross UDB (`PL073`-`PL076`, full/part-time months worked; `PL085`,
months retired; `PL087`, months in education):

- **Eligible adults**: household members aged 18-59, excluding full-time
  students aged 18-24 and retirees, and excluding people whose whole
  monthly calendar is unknown.
- **FTE months worked** per person: full months (`PL073`+`PL075`) plus
  part-time months (`PL074`+`PL076`), all weighted equally at 1.0 in the
  absence of hours-worked detail — capped at 12.
- **Household work intensity** = sum of FTE months worked by eligible
  adults / (12 × number of eligible adults).
- Reported as: a continuous ratio, low/medium work-intensity flags
  (≤0.20, ≤0.50), four mutually exclusive bands (≤0.20, (0.20,0.50],
  (0.50,0.80], (0.80,1]), and a binary split at 0.50.

This is a simplified proxy and does not use actual hours worked per week —
treat it as directionally consistent with, but not identical to, Eurostat's
official (V)LWI indicator.

### Anchored poverty

Because the poverty threshold is relative (moving with each year's median
income), a country can show a stable or even rising AROP rate purely
because the threshold itself grew. Anchored poverty removes this "moving
target" effect:

1. Equivalised income (`hystd`) is deflated to base-year euros using the
   HICP-based deflator built in `02_poverty_thresholds_deflators.R`
   (chained from annual inflation rates, base year = the first year in
   `anios_cross`, default 2012).
2. This deflated income is compared against the 60% threshold observed **in
   the base year only** (not the current year's threshold).

The result (`arop60_anchored`) tells you whether real purchasing power fell
below a fixed reference line, isolating income growth from relative-poverty
dynamics.

## License

Released under the [MIT License](LICENSE).

## Citation

If you use this code in academic work or have any doubts, please contact me at
alberto.fabra@upf.edu // fabralopezalberto@gmail.com
