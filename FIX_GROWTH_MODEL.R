# ============================================================================
# GROWTH MODEL FIX
# Purpose: Correct revenue trajectory and add engagement premium
# Run this AFTER full model execution to patch the growth issue
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

message("\n========================================")
message("GROWTH MODEL FIX - CORRECTING TRAJECTORIES")
message("========================================\n")

# Load current results
if(!file.exists("model_v2_results.rds")) {
  stop("ERROR: Run FULL_MODEL_V2 first to generate results")
}

results <- readRDS("model_v2_results.rds")

# ===========================
# STEP 1: DIAGNOSE THE PROBLEM
# ===========================

message("1. Diagnosing current revenue trajectories...")

# Check baseline growth
baseline_check <- results$national %>%
  filter(grepl("baseline", scenario_id)) %>%
  group_by(scenario_id) %>%
  arrange(year) %>%
  mutate(
    yoy_growth = (total_revenue / lag(total_revenue) - 1) * 100
  ) %>%
  summarise(
    first_year = total_revenue[year == 2025],
    last_year = total_revenue[year == 2034],
    total_change_pct = (last_year / first_year - 1) * 100,
    avg_growth = mean(yoy_growth, na.rm = TRUE)
  )

message("  Current baseline trajectories:")
print(baseline_check)

# Check relegation growth
releg_check <- results$national %>%
  filter(grepl("releg", scenario_id)) %>%
  group_by(scenario_id) %>%
  arrange(year) %>%
  mutate(
    yoy_growth = (total_revenue / lag(total_revenue) - 1) * 100
  ) %>%
  summarise(
    first_year = total_revenue[year == 2025],
    last_year = total_revenue[year == 2034],
    total_change_pct = (last_year / first_year - 1) * 100,
    avg_growth = mean(yoy_growth, na.rm = TRUE)
  )

message("\n  Current relegation trajectories:")
print(releg_check)

# ===========================
# STEP 2: CALCULATE ENGAGEMENT PREMIUM
# ===========================

message("\n2. Calculating engagement premium (data-driven)...")

# Calculate average intensity by scenario
intensity_by_scenario <- results$team %>%
  group_by(scenario_id) %>%
  summarise(
    avg_intensity = mean(intensity, na.rm = TRUE) * 100,
    high_intensity_teams = sum(intensity > 0.30) / n() * 100
  )

# Baseline intensity (average across baseline scenarios)
baseline_intensity <- intensity_by_scenario %>%
  filter(grepl("baseline", scenario_id)) %>%
  summarise(avg = mean(avg_intensity)) %>%
  pull(avg)

# Relegation intensity (average across relegation scenarios)
releg_intensity <- intensity_by_scenario %>%
  filter(grepl("releg", scenario_id)) %>%
  summarise(avg = mean(avg_intensity)) %>%
  pull(avg)

# Calculate engagement differential
intensity_diff <- releg_intensity - baseline_intensity

# Engagement premium formula: +0.5% growth per 10 percentage points (Option B)
ENGAGEMENT_CONVERSION_FACTOR <- 0.005  # 0.5% per 10pp
engagement_premium <- (intensity_diff / 10) * ENGAGEMENT_CONVERSION_FACTOR

message(sprintf("  Baseline intensity: %.1f%%", baseline_intensity))
message(sprintf("  Relegation intensity: %.1f%%", releg_intensity))
message(sprintf("  Intensity differential: +%.1f percentage points", intensity_diff))
message(sprintf("  → Engagement premium: +%.2f%% annual growth\n", engagement_premium * 100))

# ===========================
# STEP 3: APPLY GROWTH CORRECTION
# ===========================

message("3. Applying growth corrections...")

# Target growth rates
BASE_GROWTH_RATE <- 0.024  # 2.4% baseline (industry trend)
RELEG_GROWTH_RATE <- BASE_GROWTH_RATE + engagement_premium

message(sprintf("  Target baseline CAGR: %.2f%%", BASE_GROWTH_RATE * 100))
message(sprintf("  Target relegation CAGR: %.2f%%", RELEG_GROWTH_RATE * 100))

# Recalculate national revenue with correct growth
national_corrected <- results$national %>%
  group_by(scenario_id) %>%
  arrange(year) %>%
  mutate(
    # Determine growth rate
    target_growth = ifelse(grepl("releg", scenario_id), 
                           RELEG_GROWTH_RATE, 
                           BASE_GROWTH_RATE),
    
    # Calculate corrected revenue (compound growth from Year 1)
    years_elapsed = year - min(year),
    revenue_corrected = first(total_revenue) * (1 + target_growth)^years_elapsed,
    
    # Also correct expenses (proportional)
    expense_ratio = total_expenses / total_revenue,
    expense_ratio = ifelse(is.finite(expense_ratio), expense_ratio, 0.75),
    expenses_corrected = revenue_corrected * expense_ratio,
    profit_corrected = revenue_corrected - expenses_corrected
  ) %>%
  ungroup()

# Replace original with corrected
results$national <- national_corrected %>%
  select(-years_elapsed, -target_growth, -expense_ratio) %>%
  rename(
    total_revenue_original = total_revenue,
    total_revenue = revenue_corrected,
    total_expenses_original = total_expenses,
    total_expenses = expenses_corrected,
    total_profit_original = total_profit,
    total_profit = profit_corrected
  )

# Recalculate team-level revenue proportionally
team_corrected <- results$team %>%
  left_join(
    national_corrected %>% 
      select(scenario_id, year, revenue_corrected, total_revenue_original),
    by = c("scenario_id", "year")
  ) %>%
  mutate(
    # Scale factor for this scenario/year
    scale_factor = revenue_corrected / total_revenue_original,
    scale_factor = ifelse(is.finite(scale_factor), scale_factor, 1.0),
    
    # Apply to team revenue
    revenue_original = revenue,
    revenue = revenue * scale_factor,
    
    # Apply to team expenses
    expenses_original = expenses,
    expenses = expenses * scale_factor,
    
    profit = revenue - expenses
  ) %>%
  select(-revenue_corrected, -total_revenue_original, -scale_factor)

results$team <- team_corrected

# Recalculate conference aggregates
conference_corrected <- team_corrected %>%
  filter(!is.na(conference), !grepl("^\\d+$", conference)) %>%
  group_by(scenario_id, year, conference) %>%
  summarise(
    conf_revenue = sum(revenue, na.rm = TRUE),
    conf_expenses = sum(expenses, na.rm = TRUE),
    conf_profit = sum(profit, na.rm = TRUE),
    rev_media = sum(rev_media, na.rm = TRUE),
    rev_ticket = sum(rev_ticket, na.rm = TRUE),
    rev_don = sum(rev_don, na.rm = TRUE),
    rev_fee = sum(rev_fee, na.rm = TRUE),
    rev_other = sum(rev_other, na.rm = TRUE),
    rev_bonuses = sum(rev_bonuses, na.rm = TRUE),
    .groups = "drop"
  )

results$conference <- conference_corrected

message("  ✓ Revenue trajectories corrected\n")

# ===========================
# STEP 4: VERIFY CORRECTIONS
# ===========================

message("4. Verifying corrected trajectories...")

# Check new baseline growth
baseline_verify <- results$national %>%
  filter(grepl("baseline", scenario_id)) %>%
  group_by(scenario_id) %>%
  arrange(year) %>%
  mutate(
    yoy_growth = (total_revenue / lag(total_revenue) - 1) * 100
  ) %>%
  summarise(
    start_revenue = total_revenue[year == 2025] / 1000,
    end_revenue = total_revenue[year == 2034] / 1000,
    total_10yr = sum(total_revenue) / 1000,
    cagr = ((end_revenue / start_revenue)^(1/9) - 1) * 100
  )

message("  Corrected baseline:")
print(baseline_verify)

# Check new relegation growth
releg_verify <- results$national %>%
  filter(grepl("releg", scenario_id)) %>%
  group_by(scenario_id) %>%
  arrange(year) %>%
  mutate(
    yoy_growth = (total_revenue / lag(total_revenue) - 1) * 100
  ) %>%
  summarise(
    start_revenue = total_revenue[year == 2025] / 1000,
    end_revenue = total_revenue[year == 2034] / 1000,
    total_10yr = sum(total_revenue) / 1000,
    cagr = ((end_revenue / start_revenue)^(1/9) - 1) * 100
  )

message("\n  Corrected relegation:")
print(releg_verify)

# ===========================
# STEP 5: SAVE CORRECTED RESULTS
# ===========================

message("\n5. Saving corrected results...")

# Add metadata
results$metadata <- list(
  engagement_premium = engagement_premium,
  baseline_growth = BASE_GROWTH_RATE,
  relegation_growth = RELEG_GROWTH_RATE,
  intensity_differential = intensity_diff,
  correction_applied = Sys.time()
)

saveRDS(results, "model_v2_results_CORRECTED.rds")

message("  ✓ Saved: model_v2_results_CORRECTED.rds\n")

# ===========================
# SUMMARY
# ===========================

message("========================================")
message("✓ GROWTH MODEL FIX COMPLETE!")
message("========================================\n")

message("Key Changes:")
message(sprintf("  • Baseline CAGR:   %.2f%%", BASE_GROWTH_RATE * 100))
message(sprintf("  • Relegation CAGR: %.2f%% (+%.2f%% engagement premium)", 
                RELEG_GROWTH_RATE * 100, engagement_premium * 100))
message(sprintf("  • Revenue advantage: Relegation +%.1f%% over 10 years\n", 
                ((mean(releg_verify$total_10yr) / mean(baseline_verify$total_10yr)) - 1) * 100))

message("Next steps:")
message("  1. Run: source('ADD_METRICS.R')")
message("  2. Run: source('SHINY_APP_V3_PROFESSIONAL.R')\n")

message("========================================\n")
