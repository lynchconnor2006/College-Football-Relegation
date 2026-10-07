# ============================================================================
# DATA EXTRACTION SCRIPT
# Purpose: Calculate parameters from historical data (minimize assumptions)
# Run this FIRST before other scripts
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(zoo)
})

# ===========================
# LOAD DATA (reuse from Part 1)
# ===========================

source("cfb_enhanced_part1.R")  # Loads all ingestion functions
source("cfb_enhanced_part2.R")  # Loads estimation functions

message("\n========================================")
message("DATA EXTRACTION - CALCULATING PARAMETERS")
message("========================================\n")

# Load raw data
ing <- ingest_all(ncaa_path, europe_path, YEARS_HIST)
ncaa_wide   <- read_first_sheet(ncaa_path)
ncaa_long   <- ing$ncaa_long
euro_long   <- ing$euro_long
anchor_2024 <- ing$anchor_2024

# ===========================
# 1. CONFERENCE GROWTH (Time-Varying)
# ===========================

message("1. Extracting conference growth rates (year-by-year)...")

conf_growth_timeseries <- ncaa_long %>%
  filter(year %in% YEARS_HIST, year != 2020, !is.na(conference)) %>%
  arrange(team, year) %>%
  group_by(team, conference) %>%
  mutate(
    revenue_growth = (revenue / lag(revenue)) - 1
  ) %>%
  ungroup() %>%
  filter(is.finite(revenue_growth)) %>%
  group_by(conference, year) %>%
  summarise(
    conf_growth_rate = median(revenue_growth, na.rm = TRUE),
    n_teams = n(),
    .groups = "drop"
  ) %>%
  filter(n_teams >= 3)  # Only conferences with 3+ data points

# Fit linear trend for each conference to extrapolate 2025-2034
# Use recent 3-year average (2022-2024) for forward projection
conf_growth_forecast <- conf_growth_timeseries %>%
  group_by(conference) %>%
  summarise(
    # Use most recent 3 years (avoids COVID and captures current trajectory)
    recent_3yr = mean(tail(conf_growth_rate, 3), na.rm = TRUE),
    
    # Also calculate 5-year average for comparison
    avg_5yr = mean(tail(conf_growth_rate, 5), na.rm = TRUE),
    
    # Historical average (all years)
    avg_all = mean(conf_growth_rate, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Use recent 3-year, but floor at 0% (no declining conferences)
    baseline_2025 = pmax(-0.02, recent_3yr),
    
    # Cap at reasonable 8% growth
    baseline_2025 = pmin(0.08, baseline_2025)
  )

message(sprintf("  ✓ Calculated growth for %d conferences", nrow(conf_growth_forecast)))

# ===========================
# 2. TEAM & CONFERENCE VOLATILITY (from rank changes)
# ===========================

message("2. Calculating volatility from rank changes...")

# Team-level volatility
team_volatility <- ncaa_long %>%
  filter(year %in% YEARS_HIST, year != 2020) %>%
  arrange(team, year) %>%
  group_by(team) %>%
  mutate(
    rank_change = abs(rank - lag(rank)),
    revenue_pct_change = abs((revenue / lag(revenue)) - 1)
  ) %>%
  filter(is.finite(rank_change), is.finite(revenue_pct_change)) %>%
  summarise(
    rank_volatility = sd(rank_change, na.rm = TRUE),
    revenue_volatility = sd(revenue_pct_change, na.rm = TRUE),
    # Normalize: teams that swing 20+ ranks get high vol
    vol_from_rank = pmin(rank_volatility / 25, 1.0) * 0.08,
    # Combined metric
    team_vol = pmax(revenue_volatility, vol_from_rank),
    .groups = "drop"
  ) %>%
  mutate(
    # Floor at 2%, cap at 15%
    team_vol = pmax(0.02, pmin(0.15, team_vol))
  )

# Conference-level volatility
conf_volatility <- ncaa_long %>%
  filter(year %in% YEARS_HIST, year != 2020, !is.na(conference)) %>%
  group_by(conference) %>%
  summarise(
    conf_vol = sd(rank, na.rm = TRUE) / mean(rank, na.rm = TRUE),
    # Coefficient of variation, clamped
    conf_vol = pmax(0.015, pmin(0.10, conf_vol)),
    .groups = "drop"
  )

message(sprintf("  ✓ Calculated volatility for %d teams", nrow(team_volatility)))
message(sprintf("  ✓ Calculated volatility for %d conferences", nrow(conf_volatility)))

# ===========================
# 3. TIER REVENUE MULTIPLIERS (from Europe data)
# ===========================

message("3. Extracting tier revenue multipliers from Europe data...")

# Calculate media revenue ratios by tier
euro_media_ratios <- euro_long %>%
  filter(is.finite(tv_rev), tv_rev > 0, !is.na(tier)) %>%
  group_by(country, year, tier) %>%
  summarise(median_tv_rev = median(tv_rev, na.rm = TRUE), .groups = "drop") %>%
  group_by(country, year) %>%
  mutate(
    # Normalize to Tier 1 = 1.0
    ratio_to_tier1 = median_tv_rev / median_tv_rev[tier == 1]
  ) %>%
  ungroup() %>%
  filter(is.finite(ratio_to_tier1))

# Aggregate across all countries
tier_media_mult <- euro_media_ratios %>%
  group_by(tier) %>%
  summarise(
    media_multiplier = median(ratio_to_tier1, na.rm = TRUE),
    q25 = quantile(ratio_to_tier1, 0.25, na.rm = TRUE),
    q75 = quantile(ratio_to_tier1, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Apply dampening factor (Europe is more extreme than NCAA will be)
  mutate(
    # Dampening: move multipliers 60% toward 1.0 (less extreme)
    dampening = 0.60,
    ncaa_media_mult = 1 + dampening * (media_multiplier - 1),
    # Ensure Tier 1 stays at 1.0 baseline
    ncaa_media_mult = ncaa_media_mult / ncaa_media_mult[tier == 1]
  )

# Ticket/attendance ratios
euro_ticket_ratios <- euro_long %>%
  filter(is.finite(ticket_rev), ticket_rev > 0, !is.na(tier)) %>%
  group_by(country, year, tier) %>%
  summarise(median_ticket_rev = median(ticket_rev, na.rm = TRUE), .groups = "drop") %>%
  group_by(country, year) %>%
  mutate(ratio_to_tier1 = median_ticket_rev / median_ticket_rev[tier == 1]) %>%
  ungroup() %>%
  filter(is.finite(ratio_to_tier1))

tier_ticket_mult <- euro_ticket_ratios %>%
  group_by(tier) %>%
  summarise(
    ticket_multiplier = median(ratio_to_tier1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dampening = 0.70,  # Less extreme for tickets
    ncaa_ticket_mult = 1 + dampening * (ticket_multiplier - 1),
    ncaa_ticket_mult = ncaa_ticket_mult / ncaa_ticket_mult[tier == 1]
  )

# Total revenue ratios (for other components)
euro_revenue_ratios <- euro_long %>%
  filter(is.finite(revenue), revenue > 0, !is.na(tier)) %>%
  group_by(country, year, tier) %>%
  summarise(median_revenue = median(revenue, na.rm = TRUE), .groups = "drop") %>%
  group_by(country, year) %>%
  mutate(ratio_to_tier1 = median_revenue / median_revenue[tier == 1]) %>%
  ungroup() %>%
  filter(is.finite(ratio_to_tier1))

tier_revenue_mult <- euro_revenue_ratios %>%
  group_by(tier) %>%
  summarise(
    revenue_multiplier = median(ratio_to_tier1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dampening = 0.65,
    ncaa_revenue_mult = 1 + dampening * (revenue_multiplier - 1),
    ncaa_revenue_mult = ncaa_revenue_mult / ncaa_revenue_mult[tier == 1]
  )

message("  ✓ Tier media multipliers extracted from Europe data")
message("  ✓ Tier ticket multipliers extracted from Europe data")
message("  ✓ Tier revenue multipliers extracted from Europe data")

# ===========================
# 4. MOMENTUM EFFECTS (sustained success/failure)
# ===========================

message("4. Calculating momentum effects from performance streaks...")

# Identify sustained success (3+ years top 25) vs sustained failure (3+ years bottom 50)
momentum_patterns <- ncaa_long %>%
  filter(year %in% YEARS_HIST, year != 2020) %>%
  arrange(team, year) %>%
  group_by(team) %>%
  mutate(
    top25 = as.integer(rank <= 25),
    bottom50 = as.integer(rank >= 50),
    # Rolling 3-year streak
    success_streak = rollapply(top25, width = 3, FUN = sum, align = "right", fill = NA),
    failure_streak = rollapply(bottom50, width = 3, FUN = sum, align = "right", fill = NA),
    # Revenue growth
    revenue_growth = (revenue / lag(revenue)) - 1
  ) %>%
  ungroup() %>%
  filter(is.finite(revenue_growth))

# Measure revenue boost from sustained success
success_boost <- momentum_patterns %>%
  filter(success_streak >= 3) %>%
  summarise(
    avg_growth_sustained = mean(revenue_growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pull(avg_growth_sustained)

baseline_growth <- momentum_patterns %>%
  filter(success_streak <= 1, failure_streak <= 1) %>%
  summarise(avg_growth_baseline = mean(revenue_growth, na.rm = TRUE)) %>%
  pull(avg_growth_baseline)

failure_penalty <- momentum_patterns %>%
  filter(failure_streak >= 3) %>%
  summarise(avg_growth_sustained_fail = mean(revenue_growth, na.rm = TRUE)) %>%
  pull(avg_growth_sustained_fail)

momentum_boost_sustained <- success_boost - baseline_growth
momentum_penalty_sustained <- failure_penalty - baseline_growth

message(sprintf("  ✓ Sustained success boost: +%.2f%%", momentum_boost_sustained * 100))
message(sprintf("  ✓ Sustained failure penalty: %.2f%%", momentum_penalty_sustained * 100))

# ===========================
# 5. PROMOTION/RELEGATION EFFECTS (from Europe)
# ===========================

message("5. Extracting promotion/relegation revenue impacts from Europe...")

# Identify tier changes in Europe data
euro_tier_changes <- euro_long %>%
  arrange(club, year) %>%
  group_by(club) %>%
  mutate(
    tier_change = tier - lag(tier),
    revenue_growth = (revenue / lag(revenue)) - 1,
    tv_growth = (tv_rev / lag(tv_rev)) - 1
  ) %>%
  ungroup() %>%
  filter(is.finite(tier_change), abs(tier_change) >= 1)

# Promotion effects (tier decreased = promoted)
promotion_effects <- euro_tier_changes %>%
  filter(tier_change == -1, is.finite(revenue_growth), is.finite(tv_growth)) %>%
  summarise(
    avg_revenue_boost = median(revenue_growth, na.rm = TRUE),
    avg_tv_boost = median(tv_growth, na.rm = TRUE),
    .groups = "drop"
  )

# Relegation effects (tier increased = relegated)
relegation_effects <- euro_tier_changes %>%
  filter(tier_change == 1, is.finite(revenue_growth), is.finite(tv_growth)) %>%
  summarise(
    avg_revenue_penalty = median(revenue_growth, na.rm = TRUE),
    avg_tv_penalty = median(tv_growth, na.rm = TRUE),
    .groups = "drop"
  )

# Apply dampening (NCAA won't be as extreme as Europe)
promo_dampening <- 0.50
releg_dampening <- 0.50

promotion_spike <- promotion_effects$avg_tv_boost * promo_dampening
relegation_spike <- relegation_effects$avg_tv_penalty * releg_dampening

message(sprintf("  ✓ Promotion spike (media): +%.2f%%", promotion_spike * 100))
message(sprintf("  ✓ Relegation penalty (media): %.2f%%", relegation_spike * 100))

# ===========================
# 6. COMPILE ALL PARAMETERS
# ===========================

message("\n6. Compiling all extracted parameters...")

extracted_params <- list(
  # Conference growth (time-varying)
  conf_growth_forecast = conf_growth_forecast,
  conf_growth_timeseries = conf_growth_timeseries,
  
  # Volatility
  team_volatility = team_volatility,
  conf_volatility = conf_volatility,
  
  # Tier multipliers
  tier_media_mult = tier_media_mult %>% select(tier, ncaa_media_mult),
  tier_ticket_mult = tier_ticket_mult %>% select(tier, ncaa_ticket_mult),
  tier_revenue_mult = tier_revenue_mult %>% select(tier, ncaa_revenue_mult),
  
  # Momentum
  momentum_boost_sustained = momentum_boost_sustained,
  momentum_penalty_sustained = momentum_penalty_sustained,
  
  # Promotion/Relegation spikes
  promotion_spike_media = promotion_spike,
  relegation_spike_media = relegation_spike,
  
  # Summary statistics
  summary = tibble(
    parameter = c("Conference growth (avg)", "Team volatility (avg)", 
                  "Tier 1 media mult", "Tier 2 media mult", "Tier 3 media mult",
                  "Promotion spike", "Relegation penalty",
                  "Sustained success boost", "Sustained failure penalty"),
    value = c(
      mean(conf_growth_forecast$baseline_2025, na.rm = TRUE),
      mean(team_volatility$team_vol, na.rm = TRUE),
      tier_media_mult$ncaa_media_mult[tier_media_mult$tier == 1],
      tier_media_mult$ncaa_media_mult[tier_media_mult$tier == 2],
      tier_media_mult$ncaa_media_mult[tier_media_mult$tier == 3],
      promotion_spike,
      relegation_spike,
      momentum_boost_sustained,
      momentum_penalty_sustained
    )
  )
)

# ===========================
# 7. SAVE TO FILE
# ===========================

saveRDS(extracted_params, "extracted_parameters.rds")
write_csv(extracted_params$summary, "parameter_summary.csv")

message("\n✓ EXTRACTION COMPLETE!")
message("  - Saved: extracted_parameters.rds")
message("  - Saved: parameter_summary.csv")
message("\nParameter Summary:")
print(extracted_params$summary, n = 20)

message("\n========================================")
message("Next step: Run PARAMETER_UPDATES.R")
message("========================================\n")
