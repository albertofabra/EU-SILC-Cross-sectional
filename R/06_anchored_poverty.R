# ============================================================
# EU-SILC Pipeline — 06_anchored_poverty.R
# ------------------------------------------------------------
# Computes an "anchored" At-Risk-Of-Poverty indicator at the 60%
# threshold: instead of comparing each year's income to that same
# year's (moving) median-based threshold, income is deflated to
# base-year euros (using the HICP deflator from
# 02_poverty_thresholds_deflators.R) and compared against the
# poverty threshold observed in the base year. This isolates real
# income growth from the "moving target" effect of a relative
# poverty line.
#
# INPUT:
#   - <output_dir>/cross_eu_poverty.csv (from 03_poverty_indicators.R)
#   - <output_dir>/hicp_defl_2012eq1_fromInflation.csv (from 02_poverty_thresholds_deflators.R)
# OUTPUT:
#   - <output_dir>/anchored_poverty_person.csv       (person-level anchored AROP60)
#   - <output_dir>/anchored_poverty_country_year.csv (weighted shares)
# ============================================================

source("00_config_utils.R")

suppressPackageStartupMessages({
  library(data.table)
})

in_csv   <- file.path(output_dir, "cross_eu_poverty.csv")
defl_csv <- file.path(output_dir, "hicp_defl_2012eq1_fromInflation.csv")
stopifnot(file.exists(in_csv), file.exists(defl_csv))

# Anchor year = the first year covered by the pipeline (see 00_config_utils.R),
# consistent with the base year used to build the HICP deflator.
base_year <- min(anios_cross)

out_person_csv <- file.path(output_dir, "anchored_poverty_person.csv")
out_cy_csv     <- file.path(output_dir, "anchored_poverty_country_year.csv")

## ------------------------------------------------------------
## 1) Load deflator
## ------------------------------------------------------------
load_deflator <- function(path) {
  D <- fread(path)
  setnames(D, tolower(names(D)))
  stopifnot(all(c("country", "year") %in% names(D)))
  D[, country := toupper(trimws(as.character(country)))]
  D[, year    := as.integer(year)]

  cand <- intersect(c("defl", "deflator", "hicp_defl"), names(D))
  if (!length(cand)) {
    numc <- names(D)[vapply(D, is.numeric, TRUE)]
    cand <- tail(numc, 1)
  }
  setnames(D, cand[1], "defl")
  D[, defl := as.numeric(defl)]
  unique(D[, .(country, year, defl)])
}

## ------------------------------------------------------------
## 2) Anchor equivalised income to base-year euros and compare
##    against the base-year threshold
## ------------------------------------------------------------
compute_anchored_arop60 <- function(DTin, DEF, base_year) {
  DT <- copy(DTin)
  DT[, `:=`(
    country = toupper(trimws(as.character(country))),
    year    = as.integer(year)
  )]

  # Prefer the explicit BHC measure; fall back to the legacy alias if
  # 03_poverty_indicators.R produced an older/simplified schema.
  income_col <- if ("hystd_bhc" %in% names(DT)) "hystd_bhc" else "hystd"
  thresh_col <- if ("thresh60_BHC" %in% names(DT)) "thresh60_BHC" else "thresh60"
  stopifnot(income_col %in% names(DT), thresh_col %in% names(DT))

  DT <- merge(DT, DEF, by = c("country", "year"), all.x = TRUE)
  DT[, hystd_base_euros := as.numeric(get(income_col)) / as.numeric(defl)]

  thr_base <- DT[year == base_year & is.finite(get(thresh_col)),
                  .(thresh60_base = as.numeric(get(thresh_col))[1L]), by = country]
  DT <- merge(DT, thr_base, by = "country", all.x = TRUE)

  DT[, arop60_anchored := fifelse(
    is.finite(hystd_base_euros) & is.finite(thresh60_base),
    as.integer(hystd_base_euros < thresh60_base), NA_integer_
  )]

  DT[, w_person := suppressWarnings(as.numeric(PB040))]

  cy <- DT[is.finite(w_person) & w_person > 0,
           .(arop60_anchored_share = wprop(arop60_anchored, w_person)),
           by = .(country, year)][order(country, year)]

  list(DT = DT, cy = cy)
}

## ------------------------------------------------------------
## 3) Run and write outputs
## ------------------------------------------------------------
cat(">> Reading poverty dataset and deflator...\n")
DT  <- fread(in_csv, showProgress = TRUE)
DEF <- load_deflator(defl_csv)

cat(sprintf(">> Anchoring income and thresholds to base year %d...\n", base_year))
res <- compute_anchored_arop60(DT, DEF, base_year)

fwrite(res$DT[, .(country, year, hhid, pid, defl, hystd_base_euros,
                   thresh60_base, arop60_anchored)],
       out_person_csv)
cat("✓ Person-level anchored poverty written to:", out_person_csv, "\n")

fwrite(res$cy, out_cy_csv)
cat("✓ Country-year anchored poverty shares written to:", out_cy_csv, "\n")
