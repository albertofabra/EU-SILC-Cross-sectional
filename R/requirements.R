# ============================================================
# EU-SILC Pipeline — requirements.R
# ------------------------------------------------------------
# Installs the R packages needed to run 00_config_utils.R through
# 06_anchored_poverty.R. `arrow` is optional (only needed for
# Parquet output) and is skipped gracefully if it fails to install.
# ============================================================

required_pkgs <- c(
  "data.table", # fast I/O and data manipulation, used throughout
  "Hmisc",      # wtd.quantile(), used for weighted poverty thresholds
  "readxl",     # reads inflationratesEU.xlsx
  "dplyr",      # tidy data wrangling in 02_poverty_thresholds_deflators.R
  "tidyr",      # pivot_longer() in 02_poverty_thresholds_deflators.R
  "readr",      # parse_number() in 02_poverty_thresholds_deflators.R
  "stringr"     # string pattern matching in 02_poverty_thresholds_deflators.R
)

optional_pkgs <- c(
  "arrow"       # optional: enables Parquet output in 01 and 03
)

missing_required <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_required)) install.packages(missing_required)

missing_optional <- setdiff(optional_pkgs, rownames(installed.packages()))
if (length(missing_optional)) {
  tryCatch(
    install.packages(missing_optional),
    error = function(e) message("Optional package(s) not installed: ",
                                 paste(missing_optional, collapse = ", "),
                                 " (Parquet output will be skipped).")
  )
}

cat("✓ All required packages are installed.\n")
