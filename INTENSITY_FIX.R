# ============================================================================
# INTENSITY FIX SCRIPT
# Purpose: Expand intensity calculation to include bubble teams
# Run this THIRD after PARAMETER_UPDATES.R
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

message("\n========================================")
message("INTENSITY FIX - EXPANDING DEFINITION")
message("========================================\n")

# Load updated parameters
if(!file.exists("updated_parameters.rds")) {
  stop("ERROR: Run PARAMETER_UPDATES.R first")
}

params <- readRDS("updated_parameters.rds")

# ===========================
# NEW INTENSITY FUNCTION
# ===========================

message("Creating new intensity calculation function...")

calculate_intensity_enhanced <- function(state_df, playoff_teams_per_tier, is_baseline = FALSE) {
  # state_df must have: team, tier, sim_rank
  # Returns: intensity value [0, 1] for each team
  
  if(is_baseline) {
    # BASELINE: Simple model - top teams in playoff hunt
    window <- params$INTENSITY_PLAYOFF_WINDOW
    cutoff <- max(playoff_teams_per_tier)  # e.g., 16 for 16-team playoff
    
    intensity <- case_when(
      state_df$sim_rank <= cutoff ~ 1.0,
      state_df$sim_rank <= (cutoff + window) ~ 
        pmax(0, 1 - (state_df$sim_rank - cutoff) / window),
      TRUE ~ 0.0
    )
    
    return(intensity)
  }
  
  # RELEGATION: Complex model with multiple stakes
  state_df <- state_df %>%
    group_by(tier) %>%
    mutate(
      n_in_tier = n(),
      rank_in_tier = rank(sim_rank, ties.method = "first")
    ) %>%
    ungroup()
  
  # 1. PLAYOFF STAKES (tier-specific)
  playoff_cutoff <- playoff_teams_per_tier
  playoff_window <- params$INTENSITY_PLAYOFF_WINDOW
  
  playoff_intensity <- pmax(0, pmin(1, case_when(
    state_df$rank_in_tier <= playoff_cutoff ~ 1.0,
    state_df$rank_in_tier <= (playoff_cutoff + playoff_window) ~ 
      1 - (state_df$rank_in_tier - playoff_cutoff) / playoff_window,
    TRUE ~ 0.0
  )))
  
  # 2. RELEGATION STAKES (bottom 4 + buffer zone)
  n_relegated <- 4
  releg_window <- params$INTENSITY_RELEG_WINDOW
  
  relegation_intensity <- pmax(0, pmin(1, case_when(
    # Not applicable to bottom tier
    state_df$tier == max(state_df$tier) ~ 0.0,
    # In relegation zone
    (state_df$n_in_tier - state_df$rank_in_tier + 1) <= n_relegated ~ 1.0,
    # In relegation battle zone
    (state_df$n_in_tier - state_df$rank_in_tier + 1) <= (n_relegated + releg_window) ~ 
      1 - ((state_df$n_in_tier - state_df$rank_in_tier + 1) - n_relegated) / releg_window,
    TRUE ~ 0.0
  )))
  
  # 3. PROMOTION STAKES (top 4 + buffer zone, only for Tier 2 and 3)
  n_promoted <- 4
  promo_window <- params$INTENSITY_PROMO_WINDOW
  
  promotion_intensity <- pmax(0, pmin(1, case_when(
    # Not applicable to top tier
    state_df$tier == 1 ~ 0.0,
    # In promotion zone
    state_df$rank_in_tier <= n_promoted ~ 1.0,
    # In promotion battle zone
    state_df$rank_in_tier <= (n_promoted + promo_window) ~ 
      1 - (state_df$rank_in_tier - n_promoted) / promo_window,
    TRUE ~ 0.0
  )))
  
  # COMBINED INTENSITY (max of all stakes)
  intensity <- pmax(playoff_intensity, relegation_intensity, promotion_intensity)
  
  return(intensity)
}

message("  ✓ New intensity function created")
message("  ✓ Includes: Playoff bubble, Relegation battle, Promotion battle")

# ===========================
# TEST THE FUNCTION
# ===========================

message("\nTesting intensity calculation on sample data...")

# Create sample tier structure
test_data <- tibble(
  team = paste0("Team_", 1:100),
  tier = c(rep(1, 25), rep(2, 41), rep(3, 34)),
  sim_rank = 1:100
)

# Test baseline intensity
test_data$intensity_baseline <- calculate_intensity_enhanced(
  test_data, 
  playoff_teams_per_tier = 16, 
  is_baseline = TRUE
)

# Test relegation intensity
test_data$intensity_relegation <- calculate_intensity_enhanced(
  test_data, 
  playoff_teams_per_tier = 12, 
  is_baseline = FALSE
)

# Summary statistics
baseline_summary <- test_data %>%
  summarise(
    scenario = "Baseline 16-team",
    pct_meaningful = mean(intensity_baseline > 0) * 100,
    avg_intensity = mean(intensity_baseline)
  )

relegation_summary <- test_data %>%
  summarise(
    scenario = "Relegation 3-tier",
    pct_meaningful = mean(intensity_relegation > 0) * 100,
    avg_intensity = mean(intensity_relegation)
  )

comparison <- bind_rows(baseline_summary, relegation_summary)

message("\nIntensity Test Results:")
print(comparison)

if(comparison$pct_meaningful[2] > 35) {
  message("\n✓ SUCCESS: Relegation intensity > 35% (vs baseline ~20%)")
} else {
  message("\n⚠ WARNING: Relegation intensity still low - may need to widen windows")
}

# ===========================
# VISUALIZATION
# ===========================

message("\nCreating intensity distribution plot...")

intensity_plot <- test_data %>%
  select(team, tier, sim_rank, intensity_baseline, intensity_relegation) %>%
  pivot_longer(starts_with("intensity"), names_to = "scenario", values_to = "intensity") %>%
  mutate(scenario = ifelse(scenario == "intensity_baseline", "Baseline", "Relegation")) %>%
  ggplot(aes(x = sim_rank, y = intensity, color = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~ scenario, ncol = 1) +
  labs(
    title = "Intensity by Rank Position",
    subtitle = "Proportion of games with meaningful stakes",
    x = "National Rank",
    y = "Intensity (0 = no stakes, 1 = championship stakes)",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("intensity_comparison.png", intensity_plot, width = 10, height = 7, dpi = 300)
message("  ✓ Saved: intensity_comparison.png")

# ===========================
# SAVE FUNCTION
# ===========================

message("\nSaving intensity function...")

intensity_functions <- list(
  calculate_intensity_enhanced = calculate_intensity_enhanced
)

saveRDS(intensity_functions, "intensity_functions.rds")

message("\n✓ INTENSITY FIX COMPLETE!")
message("  - Saved: intensity_functions.rds")
message("  - Saved: intensity_comparison.png")

message("\n========================================")
message("Next step: Run FULL_MODEL_V2.R")
message("========================================\n")
