# ============================================================
# EU-SILC Pipeline — 05_work_intensity.R
# ------------------------------------------------------------
# Computes household (and person-level) Work Intensity (WI), based
# on self-reported full-time-equivalent months worked during the
# income reference period (PL073-PL076, PL085, PL087).
#
# Work intensity = (sum of FTE months worked by working-age household
# members) / (potential FTE months = 12 * number of eligible adults).
#
# Working-age eligible adults are those aged 18-59 who are not
# full-time students aged 18-24 and not retired, and for whom the
# monthly activity calendar is not entirely unknown.
#
# Produces:
#   - the continuous work_intensity indicator
#   - low / medium work-intensity flags (<=0.20, <=0.50)
#   - four mutually exclusive work-intensity bands: <=0.20,
#     (0.20,0.50], (0.50,0.80], (0.80,1]
#   - a binary split at 0.50
#   - weighted country-year shares for all of the above
#
# INPUT:
#   - <output_dir>/cross_eu_base.csv (from 01_build_base_cross.R)
# OUTPUT:
#   - <output_dir>/work_intensity_person.csv       (person-level WI variables)
#   - <output_dir>/work_intensity_country_year.csv  (weighted shares)
# ============================================================

source("00_config_utils.R")

suppressPackageStartupMessages({
  library(data.table)
})

in_csv <- file.path(output_dir, "cross_eu_base.csv")
stopifnot(file.exists(in_csv))

out_person_csv <- file.path(output_dir, "work_intensity_person.csv")
out_cy_csv     <- file.path(output_dir, "work_intensity_country_year.csv")

## ------------------------------------------------------------
## 1) Build the minimal person-level base needed for WI
## ------------------------------------------------------------
build_person_base <- function(path) {
  min_cols <- c(
    "country", "year", "hhid", "pid", "PB040", "age",
    "PL073", "PL074", "PL075", "PL076", "PL085", "PL087", "PE010"
  )
  hdr <- names(fread(path, nrows = 0))
  use <- intersect(min_cols, hdr)
  stopifnot(all(c("country", "year", "hhid", "pid", "PB040", "age") %in% use))

  DT <- fread(path, select = use, showProgress = TRUE)
  DT[, `:=`(
    country = toupper(trimws(as.character(country))),
    year    = as.integer(year),
    hhid    = as.character(hhid),
    pid     = as.character(pid)
  )]
  suppressWarnings(DT[, age := as.numeric(age)])

  # Recode EU-SILC missing / not-applicable codes to NA
  for (v in intersect(c("PL073", "PL074", "PL075", "PL076", "PL085", "PL087"), names(DT))) {
    suppressWarnings(DT[, (v) := as.numeric(get(v))])
    DT[get(v) %in% c(-9, -8, -7, -3, -2, -1), (v) := NA_real_]
  }

  DT[, w_person := as.numeric(PB040)]
  DT[!is.finite(w_person) | w_person <= 0, w_person := NA_real_]
  DT <- DT[is.finite(w_person) & w_person > 0]

  # Eligibility for the WI universe: 18-59, not a full-time student
  # aged 18-24, not retired, and with a known activity calendar.
  DT[, unknown_months := is.na(PL073) & is.na(PL074) & is.na(PL075) &
                          is.na(PL076) & is.na(PL085) & is.na(PL087)]

  has_PE010 <- "PE010" %in% names(DT)
  if (has_PE010) {
    suppressWarnings(DT[, PE010 := as.numeric(PE010)])
    DT[, student1824 := (age >= 18 & age <= 24) &
                         (nz(PL087, 0) >= 1 | PE010 == 1)]
  } else {
    DT[, student1824 := (age >= 18 & age <= 24) & nz(PL087, 0) >= 1]
  }
  DT[, retired       := nz(PL085, 0) >= 1]
  DT[, is_adult_base := (age >= 18 & age <= 59) & !student1824 & !retired & !unknown_months]

  DT[]
}

## ------------------------------------------------------------
## 2) Work intensity: FTE months, household ratio, bands
## ------------------------------------------------------------
compute_work_intensity <- function(DTin,
                                    lwi_threshold = 0.20,
                                    mwi_threshold = 0.50,
                                    mhwi_upper    = 0.80) {
  DT <- copy(DTin)

  # Full-time-equivalent months worked. PL073/PL075 are full months,
  # PL074/PL076 are part-time months; in the absence of hours-worked
  # information all four are weighted at 1.0.
  DT[, months_worked_fte := pmax(0, pmin(12,
        nz(PL073, 0) + nz(PL075, 0) + nz(PL074, 0) + nz(PL076, 0)))]
  DT[, is_adult_elig := is_adult_base]

  hh_comp <- DT[, .(any_elig_adult = any(is_adult_elig, na.rm = TRUE)),
                by = .(country, year, hhid)]
  hh_ag <- DT[is_adult_elig == TRUE,
              .(sum_fte = sum(months_worked_fte, na.rm = TRUE), n_adults = .N),
              by = .(country, year, hhid)]
  hh <- merge(hh_comp, hh_ag, by = c("country", "year", "hhid"), all.x = TRUE)
  hh[is.na(sum_fte),  sum_fte := 0]
  hh[is.na(n_adults), n_adults := 0]
  hh[, potential := 12 * n_adults]

  hh[(any_elig_adult == TRUE) & (potential > 0),
     work_intensity := sum_fte / potential]
  hh[(any_elig_adult == FALSE) | (potential == 0), work_intensity := NA_real_]

  hh[, hh_lwi := as.integer(is.finite(work_intensity) & work_intensity <= lwi_threshold)]
  hh[, hh_mwi := as.integer(is.finite(work_intensity) & work_intensity <= mwi_threshold)]

  hh[, wi_cat1 := as.integer(is.finite(work_intensity) & work_intensity <= lwi_threshold)]
  hh[, wi_cat2 := as.integer(is.finite(work_intensity) & work_intensity  > lwi_threshold & work_intensity <= mwi_threshold)]
  hh[, wi_cat3 := as.integer(is.finite(work_intensity) & work_intensity  > mwi_threshold & work_intensity <= mhwi_upper)]
  hh[, wi_cat4 := as.integer(is.finite(work_intensity) & work_intensity  > mhwi_upper)]

  hh[, workintensitycat := fifelse(wi_cat1 == 1L, "<=0.20",
                             fifelse(wi_cat2 == 1L, "(0.20,0.50]",
                             fifelse(wi_cat3 == 1L, "(0.50,0.80]", "(0.80,1]")))]
  hh[, workintensitycat := factor(workintensitycat,
        levels = c("<=0.20", "(0.20,0.50]", "(0.50,0.80]", "(0.80,1]"))]

  hh[, hh_wi_le_05 := as.integer(is.finite(work_intensity) & work_intensity <= 0.50)]
  hh[, hh_wi_gt_05 := as.integer(is.finite(work_intensity) & work_intensity  > 0.50)]
  hh[, workintensitycat05 := factor(fifelse(hh_wi_le_05 == 1L, "<=0.5", ">0.5"),
                                     levels = c("<=0.5", ">0.5"))]

  DT <- merge(DT, hh[, .(country, year, hhid, work_intensity, hh_lwi, hh_mwi,
                          wi_cat1, wi_cat2, wi_cat3, wi_cat4, workintensitycat,
                          hh_wi_le_05, hh_wi_gt_05, workintensitycat05, potential)],
              by = c("country", "year", "hhid"), all.x = TRUE)

  # Person-level WI split, restricted to the working-age (<=59) universe
  # belonging to a household with a defined work-intensity denominator.
  eligible_person <- (DT$age <= 59 & is.finite(DT$potential) & DT$potential > 0)
  DT[, person_wi_le_05 := fifelse(eligible_person, hh_wi_le_05, NA_integer_)]
  DT[, person_wi_gt_05 := fifelse(eligible_person, hh_wi_gt_05, NA_integer_)]

  cy <- DT[eligible_person, .(
    lwi_share     = wprop(hh_lwi, w_person),
    mwi_share     = wprop(hh_mwi, w_person),
    wi_cat1_share = wprop(wi_cat1, w_person),
    wi_cat2_share = wprop(wi_cat2, w_person),
    wi_cat3_share = wprop(wi_cat3, w_person),
    wi_cat4_share = wprop(wi_cat4, w_person),
    wi_le05_share = wprop(hh_wi_le_05, w_person),
    wi_gt05_share = wprop(hh_wi_gt_05, w_person)
  ), by = .(country, year)][order(country, year)]

  list(DT = DT, cy = cy)
}

## ------------------------------------------------------------
## 3) Run and write outputs
## ------------------------------------------------------------
cat(">> Building person-level base for work intensity...\n")
DTbase <- build_person_base(in_csv)
cat(sprintf("   rows: %s\n", format(nrow(DTbase), big.mark = ",")))

cat(">> Computing work intensity (bands + 0.50 cut)...\n")
res <- compute_work_intensity(DTbase)

fwrite(res$DT[, .(country, year, hhid, pid, work_intensity, hh_lwi, hh_mwi,
                   wi_cat1, wi_cat2, wi_cat3, wi_cat4, workintensitycat,
                   hh_wi_le_05, hh_wi_gt_05, workintensitycat05,
                   person_wi_le_05, person_wi_gt_05)],
       out_person_csv)
cat("✓ Person-level work intensity written to:", out_person_csv, "\n")

fwrite(res$cy, out_cy_csv)
cat("✓ Country-year work intensity shares written to:", out_cy_csv, "\n")
