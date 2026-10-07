# ============================================================================
# DATA CLEANING - Simple Loader
# Purpose: Load existing cleaned NCAA and European data
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

message("\n========================================")
message("LOADING CLEANED DATA")
message("========================================\n")

# ===========================
# CHECK FOR EXISTING FILES
# ===========================

# Check what data files you have
if(file.exists("ncaa_long.rds")) {
  message("✓ Found ncaa_long.rds")
  ncaa_long <- readRDS("ncaa_long.rds")
  message(sprintf("  → %d observations, %d teams, years %d-%d",
                  nrow(ncaa_long),
                  n_distinct(ncaa_long$team),
                  min(ncaa_long$year, na.rm = TRUE),
                  max(ncaa_long$year, na.rm = TRUE)))
} else if(file.exists("ncaa_clean.rds")) {
  message("✓ Found ncaa_clean.rds")
  ncaa_long <- readRDS("ncaa_clean.rds")
  message(sprintf("  → %d observations", nrow(ncaa_long)))
} else {
  stop("ERROR: No NCAA data file found. Need ncaa_long.rds or ncaa_clean.rds")
}

# European data
if(file.exists("euro_long.rds")) {
  message("✓ Found euro_long.rds")
  euro_long <- readRDS("euro_long.rds")
  message(sprintf("  → %d observations", nrow(euro_long)))
} else if(file.exists("euro_clean.rds")) {
  message("✓ Found euro_clean.rds")
  euro_long <- readRDS("euro_clean.rds")
  message(sprintf("  → %d observations", nrow(euro_long)))
} else {
  message("⚠️  No European data found - will skip European comparisons")
  euro_long <- tibble()  # Empty tibble
}

# ===========================
# SAVE AS STANDARD NAMES
# ===========================

saveRDS(ncaa_long, "ncaa_long.rds")
if(nrow(euro_long) > 0) {
  saveRDS(euro_long, "euro_long.rds")
}

message("\n✓ Data loaded and ready")
message("========================================\n")