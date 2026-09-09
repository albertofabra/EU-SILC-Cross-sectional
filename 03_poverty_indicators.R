# ============================================================
# EU-SILC Pipeline — 03_poverty_indicators.R
# ------------------------------------------------------------
# Computes household disposable income (before- and after-housing
# costs), equivalised income, and At-Risk-Of-Poverty (AROP) flags
# at the 40/50/60/70% thresholds, both BHC and AHC.
#
# What this script does:
#   1) Reads the person-level base dataset from 01_build_base_cross.R.
#   2) Builds household disposable income BHC (HY020) and AHC
#      (HY020 minus annualised housing costs, HH070).
#   3) Equivalises both using the modified OECD scale and merges the
#      country-year poverty thresholds from
#      02_poverty_thresholds_deflators.R (reshaped to wide format).
#   4) Flags AROP at 40/50/60/70% for BHC and AHC, and adds legacy
#      aliases (hydisp, hystd, thresh40..70, arop40..70) that default
#      to the BHC measure, for backward compatibility with downstream
#      scripts that only expect one measure.
#
# INPUT:
#   - <output_dir>/cross_eu_base.csv                (from 01_build_base_cross.R)
#   - <output_dir>/thresholds_all_years_BHC_AHC.csv  (from 02_poverty_thresholds_deflators.R)
# OUTPUT:
#   - <output_dir>/cross_eu_poverty.csv / .rds / .parquet
#     (base dataset + hydisp_bhc/ahc, hystd_bhc/ahc, thresh*_BHC/AHC,
#      arop*_BHC/AHC, plus legacy aliases hydisp/hystd/thresh*/arop*)
# ============================================================

source("00_config_utils.R")

suppressPackageStartupMessages({
  library(data.table)
})

in_csv  <- file.path(output_dir, "cross_eu_base.csv")
thr_csv <- file.path(output_dir, "thresholds_all_years_BHC_AHC.csv")
stopifnot(file.exists(in_csv), file.exists(thr_csv))

out_csv     <- file.path(output_dir, "cross_eu_poverty.csv")
out_rds     <- file.path(output_dir, "cross_eu_poverty.rds")
out_parquet <- file.path(output_dir, "cross_eu_poverty.parquet")
write_parquet <- requireNamespace("arrow", quietly = TRUE)

## ------------------------------------------------------------
## 1) Load thresholds and reshape to wide (one row per country-year)
## ------------------------------------------------------------
thresholds_long <- fread(thr_csv)
thresholds_long[, country := toupper(trimws(as.character(country)))]
thresholds_long[, year    := as.integer(year)]
stopifnot(all(c("year", "country", "measure",
                "thresh40", "thresh50", "thresh60", "thresh70") %in% names(thresholds_long)))

thresholds_wide <- dcast(
  thresholds_long,
  year + country ~ measure,
  value.var = c("thresh40", "thresh50", "thresh60", "thresh70")
)
setkey(thresholds_wide, year, country)

## ------------------------------------------------------------
## 2) Household disposable income (BHC/AHC), equivalised income,
##    and AROP flags
## ------------------------------------------------------------
add_poverty_derivatives <- function(dt) {
  for (cl in intersect(c("HY020", "HH070", "age"), names(dt))) {
    suppressWarnings(dt[, (cl) := as.numeric(get(cl))])
  }
  dt[, `:=`(
    year    = as.integer(year),
    country = toupper(trimws(as.character(country))),
    hhid    = as.character(hhid),
    pid     = as.character(pid)
  )]

  dt[, `:=`(child = age < 14, adult = age >= 14)]

  # Household disposable income, before- and after-housing-costs.
  # HH070 is a monthly amount in EU-SILC, so it is annualised here.
  dt[, hydisp_bhc    := as.numeric(HY020)]
  dt[, hhcost_annual := pmax(fifelse(is.na(HH070), 0, HH070), 0) * 12]
  dt[, hydisp_ahc    := pmax(hydisp_bhc - hhcost_annual, 0)]

  hh <- dt[, .(
    hhnbr_child   = sum(child, na.rm = TRUE),
    hhnbr_adult   = sum(adult, na.rm = TRUE),
    hydisp_bhc_hh = unique(na.omit(hydisp_bhc))[1],
    hydisp_ahc_hh = unique(na.omit(hydisp_ahc))[1]
  ), by = .(country, year, hhid)]

  # Modified OECD equivalence scale: 1 for the first adult, 0.5 for
  # each additional adult (14+), 0.3 for each child (<14).
  hh[, eqs := fifelse(
    hhnbr_adult >= 1,
    1 + (hhnbr_adult - 1) * 0.5 + hhnbr_child * 0.3,
    1 + pmax(hhnbr_child - 1, 0) * 0.3
  )]
  hh[, hystd_bhc := hydisp_bhc_hh / eqs]
  hh[, hystd_ahc := hydisp_ahc_hh / eqs]

  dt <- merge(dt, hh[, .(country, year, hhid, hhnbr_child, hhnbr_adult, eqs, hystd_bhc, hystd_ahc)],
              by = c("country", "year", "hhid"), all.x = TRUE)

  dt <- merge(dt, thresholds_wide, by = c("year", "country"), all.x = TRUE)

  for (p in c(40, 50, 60, 70)) {
    thr_bhc <- paste0("thresh", p, "_BHC")
    thr_ahc <- paste0("thresh", p, "_AHC")
    v_bhc   <- paste0("arop",   p, "_BHC")
    v_ahc   <- paste0("arop",   p, "_AHC")

    if (!thr_bhc %in% names(dt)) dt[, (thr_bhc) := NA_real_]
    if (!thr_ahc %in% names(dt)) dt[, (thr_ahc) := NA_real_]

    dt[, (v_bhc) := fifelse(
      is.finite(hystd_bhc) & is.finite(get(thr_bhc)) & get(thr_bhc) > 0,
      as.integer(hystd_bhc < get(thr_bhc)), NA_integer_
    )]
    dt[, (v_ahc) := fifelse(
      is.finite(hystd_ahc) & is.finite(get(thr_ahc)) & get(thr_ahc) > 0,
      as.integer(hystd_ahc < get(thr_ahc)), NA_integer_
    )]
  }

  # Legacy aliases: the unqualified income/threshold/AROP variables
  # default to the BHC measure, so scripts written before the
  # BHC/AHC split (e.g. 06_anchored_poverty.R) keep working unchanged.
  dt[, hydisp := hydisp_bhc]
  dt[, hystd  := hystd_bhc]
  for (p in c(40, 50, 60, 70)) {
    dt[, paste0("thresh", p) := get(paste0("thresh", p, "_BHC"))]
    dt[, paste0("arop",   p) := get(paste0("arop",   p, "_BHC"))]
  }

  dt[]
}

## ------------------------------------------------------------
## 3) Run and write output
## ------------------------------------------------------------
cat(">> Reading base dataset:", in_csv, "\n")
DT <- fread(in_csv, showProgress = TRUE)

cat(">> Computing poverty derivatives (BHC/AHC)...\n")
DT <- add_poverty_derivatives(DT)

fwrite(DT, out_csv)
cat("✓ Poverty dataset written to:", out_csv, "\n")

saveRDS(DT, out_rds)
cat("✓ RDS written to:", out_rds, "\n")
if (write_parquet) {
  arrow::write_parquet(DT, out_parquet)
  cat("✓ Parquet written to:", out_parquet, "\n")
}
