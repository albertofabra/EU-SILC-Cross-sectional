# ============================================================
# EU-SILC Pipeline — 01_build_base_cross.R
# ------------------------------------------------------------
# Builds the global, person-level, consolidated Cross-sectional
# EU-SILC dataset for all countries and years in `paises` /
# `anios_cross` (see 00_config_utils.R).
#
# What this script does:
#   1) Scans the raw D/H/P/R file headers of every country-year to
#      discover a common, fixed output schema (so all years/countries
#      end up with the same columns even if a variable only exists
#      in some waves).
#   2) For each country-year: reads and merges D + H + P + R, keeping
#      the discovered schema, using P (personal register) as the
#      universe of persons.
#   3) Adds structural derived variables that do NOT depend on
#      poverty/work-intensity definitions: age, household size, and
#      head-of-household identification.
#   4) Appends everything into one long CSV, then saves RDS/Parquet.
#
# This script does NOT compute poverty thresholds, AROP indicators,
# work intensity, or household-type reclassification — those live in
# 02_poverty_thresholds_deflators.R, 03_poverty_indicators.R,
# 04_household_composition.R and 05_work_intensity.R, which all take
# this script's output as their starting point.
#
# INPUT:  raw EU-SILC Cross UDB files under `ruta_base_cross`
# OUTPUT: <output_dir>/cross_eu_base.csv / .rds / .parquet
# ============================================================

source("00_config_utils.R")

suppressPackageStartupMessages({
  library(data.table)
})

out_csv     <- file.path(output_dir, "cross_eu_base.csv")
out_rds     <- file.path(output_dir, "cross_eu_base.rds")
out_parquet <- file.path(output_dir, "cross_eu_base.parquet")
if (file.exists(out_csv)) file.remove(out_csv)
if (file.exists(out_rds)) file.remove(out_rds)
write_parquet <- requireNamespace("arrow", quietly = TRUE)

## ------------------------------------------------------------
## 1) Discover a fixed, global output schema
## ------------------------------------------------------------
# Scans every available country-year header and keeps: core structural
# variables, plus any monthly PL/PY activity-calendar variables (which
# vary in naming across waves), so downstream scripts can rely on a
# stable set of columns.
discover_keep <- function() {
  keep_all <- character()

  base_keep <- c(
    "PB040", "DB080", "DB090",                     # weights
    "HY020", "HH070",                               # household income / housing cost (raw)
    "age", "RB090", "HB070", "RB050", "RB110",       # demographics / household roles
    "PE040", "PE041",                                # education
    "PB210", "PB220A", "RB280", "RB285", "RB290",    # migration background
    "household_size",
    "HX060", "HB110"                                 # household type / tenure
  )

  # Monthly activity-calendar variable patterns (naming differs across waves)
  pat_month_suffix  <- "_M(0?[1-9]|1[0-2])$"
  pat_pl073_090     <- "^PL0?(7[3-9]|8[0-9]|90)$"
  pat_pl_blocks     <- "^PL\\d{3}"
  pat_py_months     <- "^PY2\\d{2}"
  pat_generic_month <- "(?i)(month|months)"

  explicit_month_cols <- c(
    "PL032","PL060","PL080","PL085","PL086","PL087","PL088","PL089","PL090",
    "PL100","PL110","PL111","PL120","PL121","PL073","PL074","PL075","PL076"
  )

  extra2020 <- c("PL230","PL230_F","PL250","PL250_F","PL280","PL280_F")

  for (p in paises) for (y in anios_cross) {
    fD <- find_cross_file(ruta_base_cross, p, y, "D")
    fH <- find_cross_file(ruta_base_cross, p, y, "H")
    fP <- find_cross_file(ruta_base_cross, p, y, "P")
    fR <- find_cross_file(ruta_base_cross, p, y, "R")
    if (any(is.na(c(fD, fH, fP, fR)))) next

    hdrD <- names(fread(fD, nrows = 0))
    hdrH <- names(fread(fH, nrows = 0))
    hdrP <- names(fread(fP, nrows = 0))
    hdrR <- names(fread(fR, nrows = 0))
    nm   <- unique(c(hdrD, hdrH, hdrP, hdrR))

    monthly_cols <- nm[
      grepl(pat_month_suffix,  nm) |
      grepl(pat_pl073_090,     nm) |
      grepl(pat_pl_blocks,     nm) |
      grepl(pat_py_months,     nm) |
      grepl(pat_generic_month, nm)
    ]

    keep_all <- union(keep_all,
                       intersect(unique(c(base_keep, monthly_cols,
                                          explicit_month_cols, extra2020)), nm))
  }

  post_rename_and_derived <- c(
    "country", "year", "hhid", "pid",
    "age", "household_size",
    "head_pid", "head_age", "head_sex"
  )

  sort(unique(c(keep_all, post_rename_and_derived)))
}

schema_keep <- discover_keep()
cat("Fixed base schema with", length(schema_keep), "columns.\n")

## ------------------------------------------------------------
## 2) Merge D/H/P/R for one country-year, keeping the fixed schema
## ------------------------------------------------------------
read_merge_cross <- function(pais, anio) {
  fD <- find_cross_file(ruta_base_cross, pais, anio, "D")
  fH <- find_cross_file(ruta_base_cross, pais, anio, "H")
  fP <- find_cross_file(ruta_base_cross, pais, anio, "P")
  fR <- find_cross_file(ruta_base_cross, pais, anio, "R")
  if (any(is.na(c(fD, fH, fP, fR)))) stop("Missing Cross files for ", pais, "-", anio)

  hdrD <- names(fread(fD, nrows = 0))
  hdrH <- names(fread(fH, nrows = 0))
  hdrP <- names(fread(fP, nrows = 0))
  hdrR <- names(fread(fR, nrows = 0))

  minD <- c("DB020", "DB010", "DB030")
  minH <- c("HB020", "HB010", "HB030", "HY020", "HH070", "HB070")
  minP <- c("PB020", "PB010", "PX030", "PB030", "PB040")
  hhidR <- if ("RX030" %in% hdrR) "RX030" else if ("RX040" %in% hdrR) "RX040" else NA_character_
  minR  <- unique(c("RB020", "RB010", "RB030", "RB050", "RB080", "RX020", hhidR))

  wantD <- intersect(unique(c(minD, schema_keep)), hdrD)
  wantH <- intersect(unique(c(minH, schema_keep)), hdrH)
  wantP <- intersect(unique(c(minP, schema_keep)), hdrP)
  wantR <- intersect(unique(c(minR, setdiff(schema_keep, c("RX030", "RX040")), hhidR)), hdrR)

  dtD <- fread(fD, select = wantD, showProgress = FALSE)
  dtH <- fread(fH, select = wantH, showProgress = FALSE)
  dtP <- fread(fP, select = wantP, showProgress = FALSE)
  dtR <- fread(fR, select = wantR, showProgress = FALSE)

  if ("DB020" %in% names(dtD)) setnames(dtD, "DB020", "country")
  if ("DB010" %in% names(dtD)) setnames(dtD, "DB010", "year")
  if ("DB030" %in% names(dtD)) setnames(dtD, "DB030", "hhid")

  if ("HB020" %in% names(dtH)) setnames(dtH, "HB020", "country")
  if ("HB010" %in% names(dtH)) setnames(dtH, "HB010", "year")
  if ("HB030" %in% names(dtH)) setnames(dtH, "HB030", "hhid")

  if ("PB020" %in% names(dtP)) setnames(dtP, "PB020", "country")
  if ("PB010" %in% names(dtP)) setnames(dtP, "PB010", "year")
  if ("PX030" %in% names(dtP)) setnames(dtP, "PX030", "hhid")
  if ("PB030" %in% names(dtP)) setnames(dtP, "PB030", "pid")

  if ("RB020" %in% names(dtR)) setnames(dtR, "RB020", "country")
  if ("RB010" %in% names(dtR)) setnames(dtR, "RB010", "year")
  if (!is.na(hhidR) && hhidR %in% names(dtR)) setnames(dtR, hhidR, "hhid")
  if ("RB030" %in% names(dtR)) setnames(dtR, "RB030", "pid")

  ensure_cols(dtD, c("country", "year", "hhid"))
  ensure_cols(dtH, c("country", "year", "hhid", "HY020", "HH070", "HB070"))
  ensure_cols(dtP, c("country", "year", "hhid", "pid", "PB040"))
  ensure_cols(dtR, c("country", "year", "hhid", "pid", "RB050", "RB080", "RX020"))

  cast_ids <- function(x) {
    if ("country" %in% names(x)) x[, country := as.character(country)]
    if ("year"    %in% names(x)) suppressWarnings(x[, year := as.integer(as.character(year))])
    if ("hhid"    %in% names(x)) x[, hhid := as.character(hhid)]
    if ("pid"     %in% names(x)) x[, pid  := as.character(pid)]
    x
  }
  dtD <- cast_ids(dtD); dtH <- cast_ids(dtH); dtP <- cast_ids(dtP); dtR <- cast_ids(dtR)

  setkey(dtD, country, year, hhid)
  setkey(dtH, country, year, hhid)
  setkey(dtP, country, year, hhid, pid)
  setkey(dtR, country, year, hhid, pid)

  # Persons (P) are the analytical universe; D and H attach at household
  # level, R attaches only to persons that already exist in P.
  dtDH  <- merge(dtD, dtH,  by = c("country", "year", "hhid"), all = TRUE)
  dtDHP <- merge(dtP, dtDH, by = c("country", "year", "hhid"), all.x = TRUE)
  dt    <- merge(dtDHP, dtR, by = c("country", "year", "hhid", "pid"), all.x = TRUE)

  id_min <- intersect(c("country", "year", "hhid", "pid",
                         "HY020", "HH070", "HB070", "RB050", "RB080", "RX020", "PB040"),
                       names(dt))
  setcolorder(dt, c(id_min, setdiff(names(dt), id_min)))
  dt[]
}

## ------------------------------------------------------------
## 3) Structural derived variables: age, household size, head of household
## ------------------------------------------------------------
add_age_and_household_size <- function(dt) {
  for (cl in intersect(c("RB080", "RX020"), names(dt))) {
    suppressWarnings(dt[, (cl) := as.numeric(get(cl))])
  }
  dt[, age := fifelse(!is.na(RX020), as.numeric(RX020),
                       as.numeric(year) - as.numeric(RB080) - 1)]
  dt[is.infinite(age) | age < 0, age := NA_real_]

  if (!"household_size" %in% names(dt)) {
    hh_size <- dt[, .(household_size = uniqueN(pid)), by = .(country, year, hhid)]
    dt <- merge(dt, hh_size, by = c("country", "year", "hhid"), all.x = TRUE)
  }
  dt[]
}

# Identifies the household reference person: prefers the explicit HB070
# pointer, falls back to the oldest adult in the household.
add_head_of_household <- function(dt) {
  if (!"RB090" %in% names(dt)) dt[, RB090 := NA_real_]
  dt[, adult := age >= 14]

  head_hb070 <- dt[!is.na(HB070),
                    .(head_pid = as.character(unique(na.omit(HB070))[1])),
                    by = .(country, year, hhid)]

  data.table::setorder(dt, country, year, hhid, -adult, -age, pid)
  head_oldest <- dt[adult == TRUE, .SD[1], by = .(country, year, hhid)][
    , .(country, year, hhid, head_pid = as.character(pid))
  ]

  head_ids <- if (nrow(head_hb070)) {
    miss <- fsetdiff(head_oldest[, .(country, year, hhid)],
                      head_hb070[, .(country, year, hhid)])
    if (nrow(miss)) {
      rbindlist(list(head_hb070, head_oldest[miss, on = .(country, year, hhid)]),
                use.names = TRUE)
    } else head_hb070
  } else head_oldest

  head_profile <- merge(
    head_ids,
    dt[, .(country, year, hhid, pid = as.character(pid), age, RB090)],
    by.x = c("country", "year", "hhid", "head_pid"),
    by.y = c("country", "year", "hhid", "pid"),
    all.x = TRUE
  )[, .(country, year, hhid, head_pid, head_age = age, head_sex = RB090)]

  merge(dt, head_profile, by = c("country", "year", "hhid"), all.x = TRUE)
}

normalize_to_schema <- function(dt, target_cols) {
  miss <- setdiff(target_cols, names(dt))
  if (length(miss)) for (m in miss) dt[, (m) := NA]
  extra <- setdiff(names(dt), target_cols)
  if (length(extra)) dt[, (extra) := NULL]
  setcolorder(dt, target_cols)
  dt
}

## ------------------------------------------------------------
## 4) Main loop: build and write the consolidated base dataset
## ------------------------------------------------------------
setDTthreads(max(1, parallel::detectCores(logical = TRUE) - 1))

wrote_any  <- FALSE
rows_total <- 0L

for (p in paises) {
  for (y in anios_cross) {
    dt <- try(read_merge_cross(p, y), silent = TRUE)
    if (inherits(dt, "try-error") || is.null(dt) || !nrow(dt)) {
      cat(sprintf("· Skipping %s-%d (empty/error)\n", p, y)); next
    }

    if ("year" %in% names(dt)) {
      suppressWarnings(dt[, year := as.integer(year)])
      dt <- dt[year >= min(anios_cross)]
      if (!nrow(dt)) { cat(sprintf("· Skipping %s-%d (no rows after year filter)\n", p, y)); next }
    }

    dt <- add_age_and_household_size(dt)
    dt <- add_head_of_household(dt)
    dt <- normalize_to_schema(dt, schema_keep)

    fwrite(dt, out_csv, append = file.exists(out_csv), col.names = !file.exists(out_csv), eol = "\n")

    wrote_any  <- TRUE
    rows_total <- rows_total + nrow(dt)
    cat(sprintf("✔ %s-%d | rows: %d | cols: %d | total: %s\n",
                p, y, nrow(dt), ncol(dt), format(rows_total, big.mark = ",")))

    rm(dt); gc(FALSE)
  }
}

if (!wrote_any) stop("No blocks were written. Check paths/readers/years in 00_config_utils.R.")
cat("\n✓ Base cross CSV written to: ", out_csv, "\n", sep = "")

## ------------------------------------------------------------
## 5) Save RDS / Parquet copies
## ------------------------------------------------------------
DT <- fread(out_csv, showProgress = TRUE)
saveRDS(DT, out_rds)
cat("✓ RDS written to: ", out_rds, "\n", sep = "")
if (write_parquet) {
  arrow::write_parquet(DT, out_parquet)
  cat("✓ Parquet written to: ", out_parquet, "\n", sep = "")
}
