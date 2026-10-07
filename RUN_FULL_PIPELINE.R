# ============================================================================
# MASTER EXECUTION SCRIPT
# Run this to execute the complete enhanced model
# ============================================================================

message("\n")
message("========================================")
message("CFB RELEGATION MODEL V2")
message("Complete Execution Pipeline")
message("========================================\n")

start_time <- Sys.time()

# ===========================
# STEP 1: DATA EXTRACTION
# ===========================

message("STEP 1/4: Extracting parameters from historical data...")
source("DATA_EXTRACTION.R")

# ===========================
# STEP 2: PARAMETER UPDATES
# ===========================

message("\nSTEP 2/4: Applying user-specified parameter updates...")
source("PARAMETER_UPDATES.R")

# ===========================
# STEP 3: INTENSITY FIX
# ===========================

message("\nSTEP 3/4: Enhancing intensity calculation...")
source("INTENSITY_FIX.R")

# ===========================
# STEP 4: FULL MODEL EXECUTION
# ===========================

message("\nSTEP 4/4: Running full model simulation...")
source("FULL_MODEL_V2_PART1.R")
source("FULL_MODEL_V2_PART2.R")

# ===========================
# SUMMARY
# ===========================

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units="mins")

message("\n")
message("========================================")
message("✓ PIPELINE COMPLETE!")
message("========================================\n")
message(sprintf("Total execution time: %.1f minutes\n", as.numeric(elapsed)))

message("Generated files:")
message("  ✓ extracted_parameters.rds")
message("  ✓ parameter_summary.csv")
message("  ✓ updated_parameters.rds")
message("  ✓ intensity_functions.rds")
message("  ✓ intensity_comparison.png")
message("  ✓ model_v2_results.rds\n")

message("Next steps:")
message("  1. Review parameter_summary.csv to verify extracted values")
message("  2. Check intensity_comparison.png to verify 40-60% intensity")
message("  3. Run SHINY_APP_V2.R to visualize results\n")

# Quick preview of results
results <- readRDS("model_v2_results.rds")

message("Quick Results Preview:")
message("======================\n")

# Compare total revenue across scenarios
revenue_summary <- results$national %>%
  group_by(scenario_id) %>%
  summarise(
    total_10yr = sum(total_revenue, na.rm=TRUE),
    avg_annual = mean(total_revenue, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_10yr))

print(revenue_summary, n=20)

# Intensity comparison
intensity_summary <- results$team %>%
  group_by(scenario_id) %>%
  summarise(
    avg_intensity = mean(intensity, na.rm=TRUE) * 100,
    pct_teams_high_intensity = mean(intensity > 0.5, na.rm=TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(avg_intensity))

message("\nIntensity Comparison:")
print(intensity_summary, n=20)

message("\n========================================")
message("Ready to launch Shiny dashboard!")
message("Run: source('SHINY_APP_V2.R')")
message("========================================\n")
