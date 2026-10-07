# ============================================================================
# PARAMETER UPDATES - DATA-DRIVEN VERSION
# Purpose: Load and apply data-driven parameters to model
# Run this AFTER DATA_DRIVEN_PARAMETERS.R
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

message("\n========================================")
message("LOADING DATA-DRIVEN PARAMETERS")
message("========================================\n")

# Load data-driven parameters
if(!file.exists("data_driven_parameters.rds")) {
  stop("ERROR: Run DATA_DRIVEN_PARAMETERS.R first to calculate parameters")
}

params <- readRDS("data_driven_parameters.rds")

message("??? Loaded data-driven parameters from:", params$calculation_date)
message(sprintf("??? Based on NCAA data: %d-%d\n", 
                params$data_years[1], params$data_years[2]))

# ===========================
# APPLY BASE GROWTH RATE
# ===========================

BASE_GROWTH <- params$BASE_GROWTH_RATE
BASE_GROWTH_SD <- params$BASE_GROWTH_SD

message(sprintf("Base Growth Rate: %.2f%% (??%.2f%%)", 
                BASE_GROWTH * 100, BASE_GROWTH_SD * 100))

# ===========================
# APPLY CONFERENCE GROWTH FLOOR
# ===========================

CONFERENCE_FLOOR <- params$CONFERENCE_GROWTH_FLOOR

message(sprintf("Conference Floor: %.2f%% (historical worst-case)", 
                CONFERENCE_FLOOR * 100))

# ===========================
# APPLY ENGAGEMENT PREMIUM
# ===========================

# This is the KEY data-driven finding!
ENGAGEMENT_PREMIUM_PER_INTENSITY <- params$ENGAGEMENT_PREMIUM_FINAL

message(sprintf("\n???? ENGAGEMENT PREMIUM: %.3f%% per high-intensity season", 
                ENGAGEMENT_PREMIUM_PER_INTENSITY * 100))
message("   Source: Playoff proximity + bowl eligibility regression")

# Convert to formula for intensity function
# Intensity ranges from 0-1, so we scale the premium
# If intensity = 1.0 (100%), apply full premium
# If intensity = 0.5 (50%), apply half premium
ENGAGEMENT_MULTIPLIER <- function(intensity) {
  # Intensity above baseline (11.9%) gets premium
  baseline_intensity <- 0.119
  intensity_diff <- pmax(0, intensity - baseline_intensity)
  premium <- intensity_diff * (ENGAGEMENT_PREMIUM_PER_INTENSITY / 0.30)
  return(premium)
}

# ===========================
# APPLY REVENUE FLOORS
# ===========================

REVENUE_FLOOR_BY_TIER <- params$REVENUE_FLOOR_MULTIPLIERS

message("\nRevenue Floors (5th percentile of historical data):")
print(REVENUE_FLOOR_BY_TIER)

# ===========================
# APPLY PLAYOFF PAYOUTS
# ===========================

PLAYOFF_PAYOUTS <- list(
  tier1 = params$PLAYOFF_PAYOUTS_T1,
  tier2 = params$PLAYOFF_PAYOUTS_T2,
  tier3 = params$PLAYOFF_PAYOUTS_T3
)

PLAYOFF_INFLATION <- params$PLAYOFF_INFLATION_RATE

message("\nPlayoff Payouts (from CFP data):")
message("  Tier 1 Championship: $16M")
message("  Tier 2 Championship: $9.6M")
message("  Tier 3 Championship: $6.4M")

# ===========================
# APPLY BOWL PAYOUTS
# ===========================

BOWL_PAYOUTS <- params$BOWL_PAYOUTS

message("\nBowl Payouts (non-playoff teams):")
print(BOWL_PAYOUTS)

# ===========================
# APPLY DONATION MOMENTUM
# ===========================

DONATION_MOMENTUM <- params$DONATION_MOMENTUM_COEF

message(sprintf("\nDonation Momentum: %.3f coefficient", DONATION_MOMENTUM))

# ===========================
# SELECT TIER MULTIPLIER SCENARIO
# ===========================

# Default to MODERATE scenario
# Change this to test sensitivity: CONSERVATIVE, MODERATE, AGGRESSIVE
TIER_SCENARIO <- "MODERATE"

message(sprintf("\n???????  Tier Multiplier Scenario: %s (0.6?? European dampening)", TIER_SCENARIO))

if(TIER_SCENARIO == "CONSERVATIVE") {
  tier_params <- params$TIER_MULTIPLIERS_CONSERVATIVE
} else if(TIER_SCENARIO == "MODERATE") {
  tier_params <- params$TIER_MULTIPLIERS_MODERATE
} else if(TIER_SCENARIO == "AGGRESSIVE") {
  tier_params <- params$TIER_MULTIPLIERS_AGGRESSIVE
} else {
  stop("Invalid TIER_SCENARIO. Use: CONSERVATIVE, MODERATE, or AGGRESSIVE")
}

TIER_MEDIA_MULT <- tier_params$TIER_MEDIA_MULT
TIER_TICKET_MULT <- tier_params$TIER_TICKET_MULT
TIER_REVENUE_MULT <- tier_params$TIER_REVENUE_MULT

message("\nTier Media Multipliers:")
print(TIER_MEDIA_MULT)

# ===========================
# LOAD ORIGINAL EXTRACTED PARAMETERS
# ===========================

# These are still data-driven from European/NCAA extraction
extracted <- readRDS("extracted_parameters.rds")

PROMO_MEDIA_SPIKE <- extracted$promo_spike$media_spike
RELEG_MEDIA_PENALTY <- extracted$releg_penalty$media_penalty
TIER_TRANSITION_RATES <- extracted$transition_matrices
MOMENTUM_EFFECTS <- extracted$momentum_effects

# ===========================
# HELPER FUNCTIONS FOR SIMULATION
# ===========================

# Calculate playoff bonus for a team
calc_playoff_bonus <- function(tier, round, year) {
  # Get base payout
  if(tier == 1) {
    base <- PLAYOFF_PAYOUTS$tier1[[round]]
  } else if(tier == 2) {
    base <- PLAYOFF_PAYOUTS$tier2[[round]]
  } else {
    base <- PLAYOFF_PAYOUTS$tier3[[round]]
  }
  
  # Apply inflation
  years_since_2025 <- year - 2025
  inflated <- base * (1 + PLAYOFF_INFLATION)^years_since_2025
  
  return(inflated)
}

# Calculate bowl bonus for non-playoff team
calc_bowl_bonus <- function(tier) {
  BOWL_PAYOUTS %>%
    filter(tier == !!tier) %>%
    pull(bowl_payout)
}

# Calculate revenue floor for a team
calc_revenue_floor <- function(tier, starting_revenue) {
  floor_mult <- REVENUE_FLOOR_BY_TIER %>%
    filter(tier == !!tier) %>%
    pull(floor_pct)
  
  return(starting_revenue * floor_mult)
}

# Calculate engagement premium
calc_engagement_premium <- function(intensity) {
  ENGAGEMENT_MULTIPLIER(intensity)
}

# ===========================
# SAVE UPDATED PARAMETERS
# ===========================

updated_params <- list(
  # Data-driven core
  BASE_GROWTH = BASE_GROWTH,
  BASE_GROWTH_SD = BASE_GROWTH_SD,
  CONFERENCE_FLOOR = CONFERENCE_FLOOR,
  ENGAGEMENT_PREMIUM_PER_INTENSITY = ENGAGEMENT_PREMIUM_PER_INTENSITY,
  
  # Revenue management
  REVENUE_FLOOR_BY_TIER = REVENUE_FLOOR_BY_TIER,
  PLAYOFF_PAYOUTS = PLAYOFF_PAYOUTS,
  PLAYOFF_INFLATION = PLAYOFF_INFLATION,
  BOWL_PAYOUTS = BOWL_PAYOUTS,
  DONATION_MOMENTUM = DONATION_MOMENTUM,
  
  # Tier effects
  TIER_SCENARIO = TIER_SCENARIO,
  TIER_MEDIA_MULT = TIER_MEDIA_MULT,
  TIER_TICKET_MULT = TIER_TICKET_MULT,
  TIER_REVENUE_MULT = TIER_REVENUE_MULT,
  
  # European-derived (still data-driven)
  PROMO_MEDIA_SPIKE = PROMO_MEDIA_SPIKE,
  RELEG_MEDIA_PENALTY = RELEG_MEDIA_PENALTY,
  TIER_TRANSITION_RATES = TIER_TRANSITION_RATES,
  MOMENTUM_EFFECTS = MOMENTUM_EFFECTS,
  
  # Helper functions
  calc_playoff_bonus = calc_playoff_bonus,
  calc_bowl_bonus = calc_bowl_bonus,
  calc_revenue_floor = calc_revenue_floor,
  calc_engagement_premium = calc_engagement_premium,
  
  # Metadata
  update_date = Sys.Date(),
  data_source = "NCAA 2015-2024 + European soccer leagues"
)

saveRDS(updated_params, "model_parameters_final.rds")

message("\n??? Saved: model_parameters_final.rds")

# ===========================
# VALIDATION CHECKS
# ===========================

message("\n========================================")
message("VALIDATION CHECKS")
message("========================================\n")

# Check 1: Base growth is reasonable
if(BASE_GROWTH < 0 || BASE_GROWTH > 0.10) {
  warning("??????  Base growth outside expected range (0-10%)")
} else {
  message("??? Base growth rate is reasonable")
}

# Check 2: Engagement premium is positive
if(ENGAGEMENT_PREMIUM_PER_INTENSITY <= 0) {
  warning("??????  Engagement premium is negative or zero!")
} else {
  message("??? Engagement premium is positive")
}

# Check 3: Revenue floors are realistic
if(any(REVENUE_FLOOR_BY_TIER$floor_pct < 0.2) || any(REVENUE_FLOOR_BY_TIER$floor_pct > 1.0)) {
  warning("??????  Revenue floors outside realistic range (20%-100%)")
} else {
  message("??? Revenue floors are realistic")
}

# Check 4: Tier multipliers sum correctly
tier_media_avg <- mean(TIER_MEDIA_MULT$ncaa_media_mult)
if(abs(tier_media_avg - 1.0) > 0.15) {
  warning("??????  Tier multipliers may be too extreme")
} else {
  message("??? Tier multipliers are balanced")
}

message("\n========================================")
message("??? PARAMETERS READY FOR SIMULATION!")
message("========================================\n")

message("Summary of Data-Driven Parameters:")
message(sprintf("  ??? Base Growth: %.2f%% (NCAA historical)", BASE_GROWTH * 100))
message(sprintf("  ??? Engagement Premium: %.3f%% (regression-derived)", 
                ENGAGEMENT_PREMIUM_PER_INTENSITY * 100))
message(sprintf("  ??? Conference Floor: %.2f%% (worst-case)", CONFERENCE_FLOOR * 100))
message(sprintf("  ??? Tier Scenario: %s", TIER_SCENARIO))

message("\nNext steps:")
message("  1. source('FULL_MODEL_V2_PART1.R')")
message("  2. source('FULL_MODEL_V2_PART2.R')")
message("  3. Run simulation")
message("  4. source('ADD_METRICS.R')")
message("  5. source('PROFESSIONAL_DASHBOARD.R')\n")
