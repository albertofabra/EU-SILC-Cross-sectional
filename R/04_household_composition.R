# ============================================================
# EU-SILC Pipeline — 04_household_composition.R
# ------------------------------------------------------------
# Reclassifies households into a simplified 6-category typology
# derived from HX060 (EU-SILC household type variable):
#   1 = one adult, no children
#   2 = two adults, no children
#   3 = three or more adults, no children
#   4 = one adult with children
#   5 = two adults with children
#   6 = three or more adults with children
#
# This is a lightweight, household-level add-on table (one row per
# country-year-household) so it can be merged into any other output
# of this pipeline by (country, year, hhid).
#
# INPUT:
#   - <output_dir>/cross_eu_base.csv (from 01_build_base_cross.R)
# OUTPUT:
#   - <output_dir>/household_composition.csv
#     (country, year, hhid, HX060, hhcomp)
# ============================================================

source("00_config_utils.R")

suppressPackageStartupMessages({
  library(data.table)
})

in_csv  <- file.path(output_dir, "cross_eu_base.csv")
stopifnot(file.exists(in_csv))
out_csv <- file.path(output_dir, "household_composition.csv")

cat(">> Reading base dataset:", in_csv, "\n")
hdr <- names(fread(in_csv, nrows = 0))
use_cols <- intersect(c("country", "year", "hhid", "HX060"), hdr)
stopifnot(all(c("country", "year", "hhid") %in% use_cols))

DT <- fread(in_csv, select = use_cols, showProgress = TRUE)
if (!"HX060" %in% names(DT)) DT[, HX060 := NA_real_]

DT[, `:=`(
  country = toupper(trimws(as.character(country))),
  year    = as.integer(year),
  hhid    = as.character(hhid)
)]
suppressWarnings(DT[, HX060 := as.integer(HX060)])

# One row per household
HH <- unique(DT[, .(country, year, hhid, HX060)])

# HX060 codes -> simplified 6-category household composition
HH[, hhcomp := fcase(
  HX060 == 5L,                 1L,  # 1 adult, no children
  HX060 %in% c(6L, 7L),        2L,  # 2 adults, no children
  HX060 == 8L,                 3L,  # 3+ adults, no children
  HX060 == 9L,                 4L,  # 1 adult with children
  HX060 %in% c(10L, 11L, 12L), 5L,  # 2 adults with children
  HX060 == 13L,                6L,  # 3+ adults with children
  default = NA_integer_
)]
HH[, hhcomp := factor(hhcomp, levels = 1:6, labels = c(
  "1 adult, no children", "2 adults, no children", "3+ adults, no children",
  "1 adult with children", "2 adults with children", "3+ adults with children"
))]

fwrite(HH, out_csv)
cat(sprintf("✓ Household composition written to: %s (%s households)\n",
            out_csv, format(nrow(HH), big.mark = ",")))
