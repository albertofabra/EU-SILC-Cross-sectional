# ============================================================
# EU-SILC Pipeline — 02_poverty_thresholds_deflators.R
# ------------------------------------------------------------
# Two independent building blocks needed before poverty indicators
# can be computed:
#
#   A) Poverty thresholds (40/50/60/70% of median equivalised income),
#      computed directly from the raw Cross UDB files, before-housing-
#      costs (BHC) and after-housing-costs (AHC), per country and year.
#
#   B) HICP-based deflators (base year = 2012 -> value 1), built from a
#      user-supplied Excel file of annual inflation rates (%) by
#      country and year, used later to anchor poverty measures to a
#      fixed reference year (see 06_anchored_poverty.R).
#
# INPUT:
#   - raw EU-SILC Cross UDB files under `ruta_base_cross`
#   - `infl_xlsx` (annual inflation rates by country/year, in %)
# OUTPUT:
#   - <output_dir>/threshold_<year>_cross_PB040_BHC.csv / _AHC.csv (per year)
#   - <output_dir>/thresholds_all_years_BHC_AHC.csv (long format, all years)
#   - <output_dir>/hicp_defl_2012eq1_fromInflation.csv
# ============================================================

source("00_config_utils.R")

suppressPackageStartupMessages({
  library(data.table)
  library(Hmisc)   # wtd.quantile
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

## ============================================================
## A) POVERTY THRESHOLDS (BHC / AHC), per country and year
## ============================================================

# Minimal D/H/P/R reader: only the variables strictly needed to compute
# equivalised household income and the PB040 person weight.
read_merge_cross_minimal <- function(pais, anio) {
  fD <- find_cross_file(ruta_base_cross, pais, anio, "D")
  fH <- find_cross_file(ruta_base_cross, pais, anio, "H")
  fP <- find_cross_file(ruta_base_cross, pais, anio, "P")
  fR <- find_cross_file(ruta_base_cross, pais, anio, "R")
  if (any(is.na(c(fD, fH, fP, fR)))) stop("Missing Cross files for ", pais, "-", anio)

  dtD <- fread(fD, select = intersect(c("DB020","DB010","DB030"), names(fread(fD, nrows=0))), showProgress = FALSE)
  if ("DB020" %in% names(dtD)) setnames(dtD, "DB020","country")
  if ("DB010" %in% names(dtD)) setnames(dtD, "DB010","year")
  if ("DB030" %in% names(dtD)) setnames(dtD, "DB030","hhid")
  ensure_cols(dtD, c("country","year","hhid")); setkey(dtD, country, year, hhid)

  hdrH <- names(fread(fH, nrows=0))
  dtH <- fread(fH, select = intersect(c("HB020","HB010","HB030","HY020","HH070"), hdrH), showProgress = FALSE)
  if ("HB020" %in% names(dtH)) setnames(dtH, "HB020","country")
  if ("HB010" %in% names(dtH)) setnames(dtH, "HB010","year")
  if ("HB030" %in% names(dtH)) setnames(dtH, "HB030","hhid")
  ensure_cols(dtH, c("country","year","hhid","HY020","HH070")); setkey(dtH, country, year, hhid)

  hdrP <- names(fread(fP, nrows=0))
  dtP <- fread(fP, select = intersect(c("PB020","PB010","PX030","PB030","PB040"), hdrP), showProgress = FALSE)
  if ("PB020" %in% names(dtP)) setnames(dtP, "PB020","country")
  if ("PB010" %in% names(dtP)) setnames(dtP, "PB010","year")
  if ("PX030" %in% names(dtP)) setnames(dtP, "PX030","hhid")
  if ("PB030" %in% names(dtP)) setnames(dtP, "PB030","pid")
  ensure_cols(dtP, c("country","year","hhid","pid","PB040")); setkey(dtP, country, year, hhid)

  hdrR <- names(fread(fR, nrows=0))
  hhidR <- if ("RX030" %in% hdrR) "RX030" else if ("RX040" %in% hdrR) "RX040" else NA_character_
  selR <- intersect(c("RB020","RB010", hhidR, "RB030","RB050","RB080","RX020"), hdrR)
  dtR  <- fread(fR, select = selR, showProgress = FALSE)
  nm_old <- intersect(c("RB020","RB010", hhidR, "RB030","RB050","RB080","RX020"), names(dtR))
  nm_new <- c("country","year","hhid","pid","RB050","RB080","RX020")[match(nm_old,
              c("RB020","RB010", hhidR, "RB030","RB050","RB080","RX020"))]
  setnames(dtR, nm_old, nm_new)
  ensure_cols(dtR, c("country","year","hhid","pid","RB050","RB080","RX020"))
  setkey(dtR, country, year, hhid, pid)

  dtDH  <- merge(dtD, dtH,  by=c("country","year","hhid"), all=TRUE); setkey(dtDH, country, year, hhid)
  dtDHP <- merge(dtP, dtDH, by=c("country","year","hhid"), all=TRUE); setkey(dtDHP, country, year, hhid, pid)
  dt    <- merge(dtDHP, dtR, by=c("country","year","hhid","pid"), all=TRUE)

  FINAL <- c("country","year","hhid","pid","HY020","HH070","RB050","RB080","RX020","PB040")
  ensure_cols(dt, FINAL)
  setcolorder(dt, FINAL)
  dt[]
}

# Computes the 40/50/60/70% median-income poverty thresholds for one year,
# for every country, using the modified OECD equivalence scale.
# `ahc = TRUE` subtracts annualised housing costs (HH070) before equivalising.
calc_thresholds_year_pb040 <- function(anio, ahc = FALSE) {
  res <- vector("list", length(paises)); j <- 1L
  for (pais in paises) {
    dt <- try(read_merge_cross_minimal(pais, anio), silent = TRUE)
    if (inherits(dt, "try-error")) { cat(sprintf("⚠️  Missing Cross data: %s-%d\n", pais, anio)); next }

    to_num <- intersect(c("HY020","HH070","RB080","RX020","PB040","year"), names(dt))
    for (cl in to_num) suppressWarnings(dt[, (cl) := as.numeric(get(cl))])
    dt[, year := as.integer(round(year))]

    dt[, age := fifelse(is.na(RX020), as.numeric(year - RB080 - 1), as.numeric(RX020))]
    dt[age < 0, age := NA_real_]
    dt[, child := as.integer(age < 14)]
    dt[, adult := as.integer(age >= 14)]

    dt[, hydisp := HY020]
    if (ahc) {
      dt[, hydisp := pmax(hydisp - pmax(fifelse(is.na(HH070), 0, HH070), 0) * 12, 0)]
    }

    hh <- dt[, .(
      hhnbr_child = sum(child, na.rm=TRUE),
      hhnbr_adult = sum(adult, na.rm=TRUE),
      hydisp = unique(na.omit(hydisp))[1]
    ), by=.(country, year, hhid)]

    hh[, eqs := fifelse(hhnbr_adult >= 1,
                         1 + (hhnbr_adult - 1)*0.5 + hhnbr_child*0.3,
                         1 + pmax(hhnbr_child - 1, 0)*0.3)]
    hh[, hystd := hydisp / eqs]

    per <- merge(
      dt[, .(country, year, hhid, pid, PB040)],
      hh[, .(country, year, hhid, hystd)],
      by=c("country","year","hhid"), all.x=TRUE
    )

    valid <- per[is.finite(hystd) & is.finite(PB040) & PB040 > 0]
    if (nrow(valid) == 0L) { cat(sprintf("⚠️  No valid PB040 rows for %s-%d\n", pais, anio)); next }

    med <- as.numeric(Hmisc::wtd.quantile(valid$hystd, weights=valid$PB040, probs=0.5, na.rm=TRUE))

    res[[j]] <- data.table(
      year = anio, country = pais,
      measure = if (ahc) "AHC" else "BHC",
      thresh40 = 0.4*med, thresh50 = 0.5*med, thresh60 = 0.6*med, thresh70 = 0.7*med
    ); j <- j + 1L

    rm(dt, hh, per, valid); gc()
  }

  r <- rbindlist(res, use.names=TRUE, fill=TRUE)
  if (nrow(r)) {
    suffix <- if (ahc) "AHC" else "BHC"
    f <- file.path(output_dir, sprintf("threshold_%d_cross_PB040_%s.csv", anio, suffix))
    fwrite(r, f)
    cat("✔ Thresholds saved:", f, "\n")
  } else {
    cat("⚠️  No country produced thresholds for", anio, "\n")
  }
  r[]
}

thr_list_bhc <- lapply(anios_cross, calc_thresholds_year_pb040, ahc = FALSE)
thr_list_ahc <- lapply(anios_cross, calc_thresholds_year_pb040, ahc = TRUE)

thresholds_all <- rbindlist(c(thr_list_bhc, thr_list_ahc), use.names=TRUE, fill=TRUE)
setorder(thresholds_all, measure, year, country)
fwrite(thresholds_all, file.path(output_dir, "thresholds_all_years_BHC_AHC.csv"))
cat("✔ thresholds_all_years_BHC_AHC.csv written\n")

## ============================================================
## B) HICP DEFLATORS (base year 2012 = 1), from annual inflation rates
## ============================================================
out_defl_csv <- file.path(output_dir, "hicp_defl_2012eq1_fromInflation.csv")

raw <- readxl::read_excel(infl_xlsx, sheet = 1, guess_max = 1e5)

names_l <- tolower(names(raw))
geo_col <- dplyr::case_when(
  "geo"       %in% names_l ~ names(raw)[match("geo", names_l)],
  "country"   %in% names_l ~ names(raw)[match("country", names_l)],
  "countries" %in% names_l ~ names(raw)[match("countries", names_l)],
  TRUE ~ names(raw)[1]
)

year_cols <- names(raw)[str_detect(names(raw), "^[12][0-9]{3}$")]
if (!length(year_cols)) {
  raw2 <- readxl::read_excel(infl_xlsx, sheet = 1, skip = 1, guess_max = 1e5)
  names_l2 <- tolower(names(raw2))
  geo_col  <- dplyr::case_when(
    "geo"       %in% names_l2 ~ names(raw2)[match("geo", names_l2)],
    "country"   %in% names_l2 ~ names(raw2)[match("country", names_l2)],
    "countries" %in% names_l2 ~ names(raw2)[match("countries", names_l2)],
    TRUE ~ names(raw2)[1]
  )
  year_cols <- names(raw2)[str_detect(names(raw2), "^[12][0-9]{3}$")]
  stopifnot(length(year_cols) > 0)
  raw <- raw2
}

infl_long <- raw %>%
  rename(geo = all_of(geo_col)) %>%
  pivot_longer(all_of(year_cols), names_to = "year", values_to = "infl_pct_raw") %>%
  transmute(
    country  = toupper(trimws(as.character(geo))),
    year     = suppressWarnings(as.integer(year)),
    infl_pct = readr::parse_number(as.character(infl_pct_raw))
  ) %>%
  filter(!is.na(year) & year >= min(anios_cross)) %>%
  arrange(country, year) %>%
  as.data.table()

# Chains yearly inflation into an index: defl(base_year) = 1;
# defl(t) = defl(t-1) * (1 + infl(t)/100) for t > base_year.
build_defl <- function(df, base_year) {
  setorder(df, year)
  yrs <- df$year
  r   <- df$infl_pct / 100
  fac <- 1 + r
  fac[!is.finite(fac)] <- 1

  defl <- numeric(length(yrs))
  defl[yrs == base_year] <- 1
  if (all(defl == 0)) defl[1] <- 1

  for (i in seq_along(yrs)) {
    if (i == 1) next
    defl[i] <- defl[i-1] * fac[i]
  }

  if (any(yrs == base_year)) {
    base <- defl[yrs == base_year][1]
    if (is.finite(base) && base != 0) defl <- defl / base
  } else {
    defl <- defl / defl[1]
  }

  data.table(country = df$country, year = yrs, defl = defl)
}

base_year <- min(anios_cross)
DEF <- infl_long[, build_defl(.SD, base_year), by = country]
setorder(DEF, country, year)

chk_base <- DEF[year == base_year, .(defl_base = unique(defl)), by = country]
chk_inf  <- DEF[!is.finite(defl)]
if (nrow(chk_inf)) message("⚠️ Non-finite deflators for: ", paste(unique(chk_inf$country), collapse=", "))

fwrite(DEF, out_defl_csv)
cat("✓ Deflators (base ", base_year, " = 1) written to: ", out_defl_csv, "\n", sep = "")
