# ============================================================================
# ADD METRICS - Gini, Mobility, Late-Season Intensity
# Purpose: Calculate comprehensive equity and competitive metrics
# Run this AFTER simulation completes
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

message("\n========================================")
message("ADDING COMPREHENSIVE METRICS")
message("========================================\n")

# Load results
if(!file.exists("model_v2_results.rds")) {
  stop("ERROR: Run simulation first to generate model_v2_results.rds")
}

results <- readRDS("model_v2_results.rds")

# ===========================
# GINI COEFFICIENT FUNCTION
# ===========================

gini_coefficient <- function(x) {
  # Remove NA and non-positive values
  x <- x[!is.na(x) & x > 0]
  if(length(x) == 0) return(NA_real_)
  
  # Sort values
  x <- sort(x)
  n <- length(x)
  
  # Calculate Gini
  gini <- 2 * sum((1:n) * x) / (n * sum(x)) - (n + 1) / n
  return(gini)
}

message("1. Calculating Gini coefficients...")

# ===========================
# NATIONAL GINI (Overall inequality)
# ===========================

national_gini <- results$team %>%
  group_by(scenario_id, year) %>%
  summarise(
    gini_national = gini_coefficient(revenue),
    n_teams = n(),
    .groups = "drop"
  )

message("  ✓ National Gini calculated")

# ===========================
# CONFERENCE GINI (Within-conference fairness)
# ===========================

conference_gini <- results$team %>%
  filter(!is.na(conference), !grepl("^\\d+$", conference)) %>%
  group_by(scenario_id, year, conference) %>%
  summarise(
    gini_conference = gini_coefficient(revenue),
    n_teams = n(),
    .groups = "drop"
  ) %>%
  # Average across conferences
  group_by(scenario_id, year) %>%
  summarise(
    gini_conf_avg = mean(gini_conference, na.rm = TRUE),
    gini_conf_min = min(gini_conference, na.rm = TRUE),
    gini_conf_max = max(gini_conference, na.rm = TRUE),
    .groups = "drop"
  )

message("  ✓ Conference Gini calculated")

# ===========================
# TIER GINI (Within-tier competition - relegation only)
# ===========================

# OPTION 1 (Safest - check column exists first):
# FIXED: Check if tier exists before filtering
if("tier" %in% names(results$team)) {
  tier_gini <- results$team %>%
    filter(grepl("releg", scenario_id)) %>%  # Filter scenarios FIRST
    filter(!is.na(tier)) %>%  # Then check tier
    group_by(scenario_id, year, tier) %>%
    summarise(
      gini_tier = gini_coefficient(revenue),
      n_teams = n(),
      .groups = "drop"
    )
  message("  ✓ Tier Gini calculated")
} else {
  tier_gini <- tibble()
  message("  ⚠ Tier column not found - skipping tier Gini")
}

# ===========================
# 2. MOBILITY METRICS
# ===========================

message("\n2. Calculating mobility metrics...")

# Team-level mobility
if("tier" %in% names(results$team)) {
  team_mobility <- results$team %>%
    filter(grepl("releg", scenario_id), !is.na(tier)) %>%  # Only relegation scenarios # Only relegation scenarios have tiers
    group_by(scenario_id, team) %>%
    arrange(year) %>%
    summarise(
      # Binary: Did they ever move?
      ever_moved = as.integer(n_distinct(tier, na.rm = TRUE) > 1),
    
      # Range: How far did they move?
      tier_range = max(tier, na.rm = TRUE) - min(tier, na.rm = TRUE),
    
      # Frequency: How many times?
      tier_changes = sum(tier != lag(tier), na.rm = TRUE),
    
      # Starting and ending tier
     start_tier = first(tier),
     end_tier = last(tier),
    
     # Average tier over 10 years
     avg_tier = mean(tier, na.rm = TRUE),
    
     .groups = "drop"
    )

  # Scenario-level mobility summary
  scenario_mobility <- team_mobility %>%
   group_by(scenario_id) %>%
   summarise(
      pct_ever_moved = mean(ever_moved) * 100,
      avg_tier_changes = mean(tier_changes, na.rm = TRUE),
      teams_promoted_net = sum(end_tier < start_tier),
      teams_relegated_net = sum(end_tier > start_tier),
      teams_stable = sum(end_tier == start_tier),
      .groups = "drop"
   )

  message("  ✓ Mobility metrics calculated")
}  else {
  team_mobility <- tibble()
  scenario_mobility <- tibble()
  message("  ⚠ Tier column not found - skipping mobility metrics")
}
message(sprintf("    → Average %.1f%% of teams change tiers", 
                mean(scenario_mobility$pct_ever_moved, na.rm = TRUE)))

# ===========================
# 3. LATE-SEASON INTENSITY
# ===========================

message("\n3. Calculating late-season intensity metrics...")

# Define "late season" as Years 4-10 (mature system)
late_season_intensity <- results$team %>%
  filter(year >= 2028) %>%  
  group_by(scenario_id) %>%
  summarise(
    # Teams with meaningful games (>30% intensity)
    pct_meaningful = mean(intensity > 0.30, na.rm = TRUE) * 100,
    
    # Teams with high-stakes games (>50% intensity)
    pct_high_stakes = mean(intensity > 0.50, na.rm = TRUE) * 100,
    
    # Teams with critical games (>70% intensity)
    pct_critical = mean(intensity > 0.70, na.rm = TRUE) * 100,
    
    # Average intensity across all teams
    avg_intensity = mean(intensity, na.rm = TRUE) * 100,
    
    # Median intensity
    median_intensity = median(intensity, na.rm = TRUE) * 100,
    
    # Count of teams with meaningful games
    teams_meaningful = sum(intensity > 0.30, na.rm = TRUE),
    
    .groups = "drop"
  )

message("  ✓ Late-season intensity calculated")

# ===========================
# 4. REVENUE DISTRIBUTION METRICS
# ===========================

message("\n4. Calculating revenue distribution metrics...")

revenue_distribution <- results$team %>%
  group_by(scenario_id, year) %>%
  arrange(desc(revenue)) %>%
  summarise(
    # Top concentration
    top5_revenue_share = sum(revenue[1:min(5, n())]) / sum(revenue) * 100,
    top10_revenue_share = sum(revenue[1:min(10, n())]) / sum(revenue) * 100,
    top25_revenue_share = sum(revenue[1:min(25, n())]) / sum(revenue) * 100,
    
    # Bottom protection
    bottom10_revenue_share = sum(tail(revenue, 10)) / sum(revenue) * 100,
    bottom25_revenue_share = sum(tail(revenue, 25)) / sum(revenue) * 100,
    
    # Revenue range
    max_revenue = max(revenue, na.rm = TRUE),
    min_revenue = min(revenue, na.rm = TRUE),
    revenue_ratio = max_revenue / min_revenue,
    
    # Median vs mean
    median_revenue = median(revenue, na.rm = TRUE),
    mean_revenue = mean(revenue, na.rm = TRUE),
    skewness_ratio = mean_revenue / median_revenue,
    
    .groups = "drop"
  )

message("  ✓ Revenue distribution metrics calculated")

# ===========================
# 5. TIER-SPECIFIC ANALYSIS
# ===========================

message("\n5. Calculating tier-specific metrics...")

if("tier" %in% names(results$team)) {
  tier_analysis <- results$team %>%
   filter(grepl("releg", scenario_id), !is.na(tier)) %>%
   group_by(scenario_id, year, tier) %>%
   summarise(
     n_teams = n(),
     avg_revenue = mean(revenue, na.rm = TRUE),
     median_revenue = median(revenue, na.rm = TRUE),
     sd_revenue = sd(revenue, na.rm = TRUE),
     avg_intensity = mean(intensity, na.rm = TRUE) * 100,
     gini = gini_coefficient(revenue),
     .groups = "drop"
    )

  message("  ✓ Tier-specific analysis calculated")
} else {
  tier_analysis <- tibble()
  message("  ⚠ Tier column not found - skipping tier analysis")
}

# ===========================
# 6. CONFERENCE-LEVEL METRICS
# ===========================

message("\n6. Calculating conference-level metrics...")

conference_metrics <- results$team %>%
  filter(!is.na(conference), !grepl("^\\d+$", conference)) %>%
  group_by(scenario_id, year, conference) %>%
  summarise(
    n_teams = n(),
    total_revenue = sum(revenue, na.rm = TRUE),
    avg_revenue = mean(revenue, na.rm = TRUE),
    gini = gini_coefficient(revenue),
    avg_intensity = mean(intensity, na.rm = TRUE) * 100,
    pct_high_intensity = mean(intensity > 0.50) * 100,
    .groups = "drop"
  )

message("  ✓ Conference-level metrics calculated")

# ===========================
# 7. CREATE SUMMARY STATS (for dashboard)
# ===========================

message("\n7. Creating summary statistics for dashboard...")

# Calculate CAGR for each scenario
cagr_data <- results$national %>%
  group_by(scenario_id) %>%
  arrange(year) %>%
  summarise(
    first_revenue = first(total_revenue),
    last_revenue = last(total_revenue),
    n_years = n() - 1,
    cagr = ((last_revenue / first_revenue)^(1/n_years) - 1) * 100,
    .groups = "drop"
  )

# Intensity comparison (already calculated, just rename for clarity)
intensity_comparison <- late_season_intensity %>%
  select(scenario_id, avg_intensity)

# Gini comparison (average across all years)
gini_comparison <- national_gini %>%
  group_by(scenario_id) %>%
  summarise(avg_gini = mean(gini_national, na.rm = TRUE), .groups = "drop")

# Overall summary (combines everything)
overall_summary <- cagr_data %>%
  left_join(gini_comparison, by = "scenario_id") %>%
  left_join(intensity_comparison, by = "scenario_id") %>%
  left_join(scenario_mobility, by = "scenario_id")

message("  ✓ Summary statistics created")

# ===========================
# 8. ADD ALL METRICS TO RESULTS
# ===========================

message("\n8. Adding all metrics to results object...")

results$metrics <- list(
  national_gini = national_gini,
  conference_gini = conference_gini,
  tier_gini = tier_gini,
  team_mobility = team_mobility,
  scenario_mobility = scenario_mobility,
  late_season_intensity = late_season_intensity,
  revenue_distribution = revenue_distribution,
  tier_analysis = tier_analysis,
  conference_metrics = conference_metrics,
  
  # Summary stats for dashboard
  summary_stats = list(
    overall_summary = overall_summary,
    intensity_comparison = intensity_comparison,
    gini_comparison = gini_comparison
  )
)

# ===========================
# 9. CREATE SYSTEM COMPARISON
# ===========================

message("\n9. Creating system comparison table...")

baseline_summary <- tibble(
  system = "Baseline",
  avg_gini = mean(national_gini$gini_national[grepl("baseline", national_gini$scenario_id)], na.rm = TRUE),
  avg_intensity = mean(late_season_intensity$avg_intensity[grepl("baseline", late_season_intensity$scenario_id)], na.rm = TRUE),
  pct_meaningful_games = mean(late_season_intensity$pct_meaningful[grepl("baseline", late_season_intensity$scenario_id)], na.rm = TRUE),
  mobility_pct = 0, 
  top10_revenue_share = mean(revenue_distribution$top10_revenue_share[grepl("baseline", revenue_distribution$scenario_id)], na.rm = TRUE)
)

relegation_summary <- tibble(
  system = "Relegation",
  avg_gini = mean(national_gini$gini_national[grepl("releg", national_gini$scenario_id)], na.rm = TRUE),
  avg_intensity = mean(late_season_intensity$avg_intensity[grepl("releg", late_season_intensity$scenario_id)], na.rm = TRUE),
  pct_meaningful_games = mean(late_season_intensity$pct_meaningful[grepl("releg", late_season_intensity$scenario_id)], na.rm = TRUE),
  mobility_pct = mean(scenario_mobility$pct_ever_moved, na.rm = TRUE),
  top10_revenue_share = mean(revenue_distribution$top10_revenue_share[grepl("releg", revenue_distribution$scenario_id)], na.rm = TRUE)
)

system_comparison <- bind_rows(baseline_summary, relegation_summary) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

results$metrics$system_comparison <- system_comparison

message("  ✓ System comparison created")

# ===========================
# 10. SAVE ENHANCED RESULTS
# ===========================

message("\n10. Saving enhanced results...")

saveRDS(results, "model_v2_results.rds")  # Overwrite with metrics added

message("  ✓ Saved: model_v2_results.rds (with metrics embedded)\n")

# ===========================
# SUMMARY OUTPUT
# ===========================

message("========================================")
message("✓ METRICS ADDED SUCCESSFULLY!")
message("========================================\n")

message("System Comparison:")
print(system_comparison)

message("\n========================================")
message("Metrics are now embedded in model_v2_results.rds")
message("Next step: Launch dashboard with source('SHINY_DASHBOARD_FINAL.R')")
message("========================================\n")