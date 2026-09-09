# ============================================================
# EU-SILC Pipeline — 00_config_utils.R
# ------------------------------------------------------------
# Shared configuration (paths, country list, year range) and
# generic utility functions used across the whole pipeline.
#
# Every other script starts with:
#     source("00_config_utils.R")
#
# This script does NOT read or write any EU-SILC data itself.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
})

## ------------------------------------------------------------
## 1) EDIT THESE PATHS FOR YOUR OWN MACHINE / REPO
## ------------------------------------------------------------

# Folder where ALL pipeline outputs (CSV / RDS / Parquet) are written and read from.
# Every script downstream looks for its input file(s) here and writes its own
# output file(s) here too, so this is the only path most users need to change.
output_dir <- "PATH/TO/OUTPUT_DIR"

# Root folder holding the raw EU-SILC Cross-sectional microdata (UDB), with the
# expected structure:
#   ruta_base_cross/<COUNTRY>/<YEAR>/UDB_c<CC><YY><D|H|P|R>.csv[.gz]
# e.g. .../Cross/ES/2018/UDB_cES18D.csv
ruta_base_cross <- "PATH/TO/EU-SILC/Cross"

# Excel file with annual inflation rates (%) by country and year, used in
# 02_poverty_thresholds_deflators.R to build HICP-based deflators.
# Expected layout: one column identifying the country (geo/country/countries)
# and one column per year (e.g. 2012, 2013, ...).
infl_xlsx <- file.path(output_dir, "inflationratesEU.xlsx")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

## ------------------------------------------------------------
## 2) COUNTRIES AND YEARS COVERED BY THE PIPELINE
## ------------------------------------------------------------

paises <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR",
            "HR","HU","IE","IT","LT","LV","MT","NL","PL","PT","RO","SE",
            "SI","SK")

# Cross-sectional data years to process (adjust to what you actually have on disk)
anios_cross <- 2012:2024

## ------------------------------------------------------------
## 3) GENERIC UTILITIES
## ------------------------------------------------------------

# Add any missing columns to a data.table, filled with a default value.
# Used everywhere to guarantee a stable output schema even when a given
# country-year file is missing some variables.
ensure_cols <- function(dt, cols, default = NA_real_) {
  missing <- setdiff(cols, names(dt))
  if (length(missing)) for (mc in missing) dt[, (mc) := default]
  invisible(missing)
}

# Locate a Cross-sectional EU-SILC UDB file for a given country / year / block.
# `part` is one of "D" (household register), "H" (household), "P" (personal
# register), "R" (personal). Tolerant to .csv / .CSV / .csv.gz / .CSV.gz.
find_cross_file <- function(base_dir, pais, anio, part) {
  suf  <- paste0("UDB_c", pais, substr(anio, 3, 4), part)
  exts <- c(".csv", ".CSV", ".csv.gz", ".CSV.gz")
  for (e in exts) {
    f <- file.path(base_dir, pais, anio, paste0(suf, e))
    if (file.exists(f)) return(f)
  }
  NA_character_
}

# Coalesce helper: returns the first non-NA value, elementwise, across
# several numeric vectors of the same length.
nz <- function(...) Reduce(function(a, b) ifelse(!is.na(a), a, b), list(...))

# Weighted proportion of a 0/1 indicator, ignoring rows with non-finite
# values or non-positive weights. Returns NA if nothing is usable.
wprop <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- as.integer(x[ok] == 1); w <- w[ok]
  sum(w * x) / sum(w)
}

cat("✔ 00_config_utils.R loaded — output_dir:", output_dir, "\n")
