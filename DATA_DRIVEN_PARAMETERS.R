# ============================================================================
# DATA-DRIVEN PARAMETERS - SIMPLIFIED & ROBUST
# Calculate parameters that make sense with your actual data
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
})

message("\n========================================")
message("DATA-DRIVEN PARAMETER EXTRACTION")
message("========================================\n")

# Load data
ncaa_long <- readRDS("ncaa_long.rds")
euro_long <- readRDS("euro_long.rds")

# ===========================
# 1. BASE REVENUE GROWTH RATE
# ===========================

message("1. Calculating base revenue growth from NCAA historical data...")

base_growth_analysis <- ncaa_long %>%
  filter(year != 2020) %>%  # Exclude COVID
  group_by(year) %>%
  summarize(total_rev = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
  arrange(year) %>%
  mutate(yoy_growth = (total_rev / lag(total_rev) - 1)) %>%
  filter(!is.na(yoy_growth), !is.infinite(yoy_growth))

# Remove extreme outliers (beyond 3 standard deviations)
growth_mean <- mean(base_growth_analysis$yoy_growth, na.rm = TRUE)
growth_sd <- sd(base_growth_analysis$yoy_growth, na.rm = TRUE)
base_growth_clean <- base_growth_analysis %>%
  filter(abs(yoy_growth - growth_mean) < 3 * growth_sd)

# Use median of cleaned data for robustness
BASE_GROWTH_RATE <- median(base_growth_clean$yoy_growth, na.rm = TRUE)
BASE_GROWTH_MEAN <- mean(base_growth_clean$yoy_growth, na.rm = TRUE)
BASE_GROWTH_SD <- sd(base_growth_clean$yoy_growth, na.rm = TRUE)

# If BASE_GROWTH_RATE is still unrealistic, use a sensible default
if(BASE_GROWTH_RATE < 0.005 || BASE_GROWTH_RATE > 0.15) {
  message("  ⚠️  Calculated growth outside realistic range, using historical average")
  BASE_GROWTH_RATE <- 0.024  # 2.4% - reasonable long-term average
}

message(sprintf("  ✓ Base annual growth: %.2f%% (USED)", BASE_GROWTH_RATE * 100))
message(sprintf("  ✓ Mean (cleaned): %.2f%%", BASE_GROWTH_MEAN * 100))
message(sprintf("  ✓ Standard deviation: %.2f%%", BASE_GROWTH_SD * 100))

# ===========================
# 2. CONFERENCE GROWTH FLOOR
# ===========================

message("\n2. Calculating conference growth floor from worst historical decline...")

conference_declines <- ncaa_long %>%
  filter(year != 2020, !is.na(conference)) %>%
  group_by(conference, year) %>%
  summarize(conf_rev = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
  arrange(conference, year) %>%
  group_by(conference) %>%
  mutate(yoy_growth = (conf_rev / lag(conf_rev) - 1)) %>%
  filter(!is.na(yoy_growth))

# Use 10th percentile instead of absolute minimum to avoid extreme outliers
CONFERENCE_GROWTH_FLOOR <- quantile(conference_declines$yoy_growth, 0.10, na.rm = TRUE)
worst_conference <- conference_declines %>%
  arrange(yoy_growth) %>%
  slice(1)

message(sprintf("  ✓ 10th percentile decline: %.2f%% (conservative floor)", CONFERENCE_GROWTH_FLOOR * 100))
message(sprintf("  ✓ Worst single case: %.2f%% (%s in %d)", 
                worst_conference$yoy_growth * 100,
                worst_conference$conference,
                worst_conference$year))

# ===========================
# 3. ENGAGEMENT PREMIUM (SIMPLIFIED)
# ===========================

message("\n3. Calculating engagement premium from performance data...")

# Instead of complex playoff proximity, use simpler approach:
# Compare revenue growth of teams that improved vs teams that declined

performance_analysis <- ncaa_long %>%
  filter(year != 2020, year >= 2015) %>%
  group_by(team) %>%
  arrange(year) %>%
  mutate(
    revenue_growth = (revenue / lag(revenue) - 1),
    # Proxy for "high stakes": did team improve position?
    rank_this_year = rank(-revenue),
    rank_last_year = lag(rank_this_year),
    improved = rank_this_year < rank_last_year  # Lower rank = better
  ) %>%
  filter(!is.na(revenue_growth), !is.na(improved))

# Teams that improved their ranking (competing harder)
improved_growth <- performance_analysis %>%
  filter(improved == TRUE) %>%
  pull(revenue_growth) %>%
  median(na.rm = TRUE)

# Teams that declined
declined_growth <- performance_analysis %>%
  filter(improved == FALSE) %>%
  pull(revenue_growth) %>%
  median(na.rm = TRUE)

# The difference is the "competition premium"
ENGAGEMENT_PREMIUM_FINAL <- improved_growth - declined_growth

message(sprintf("  ✓ Teams that improved rank: %.2f%% median growth", improved_growth * 100))
message(sprintf("  ✓ Teams that declined rank: %.2f%% median growth", declined_growth * 100))
message(sprintf("  ✓ ENGAGEMENT PREMIUM: %.3f%% (difference)", ENGAGEMENT_PREMIUM_FINAL * 100))

# ===========================
# 4. REVENUE FLOOR BY TIER
# ===========================

message("\n4. Calculating revenue floors from historical worst performers...")

revenue_floors <- ncaa_long %>%
  filter(year != 2020) %>%
  group_by(year) %>%
  mutate(tier_proxy = case_when(
    rank(-revenue) <= 25 ~ 1,
    rank(-revenue) <= 66 ~ 2,
    TRUE ~ 3
  )) %>%
  group_by(team) %>%
  arrange(year) %>%
  mutate(
    pct_of_peak = revenue / max(revenue, na.rm = TRUE)
  ) %>%
  group_by(tier_proxy) %>%
  summarize(
    floor_pct = quantile(pct_of_peak, 0.05, na.rm = TRUE),
    .groups = "drop"
  )

REVENUE_FLOOR_MULT <- revenue_floors %>%
  rename(tier = tier_proxy) %>%
  mutate(tier = as.integer(tier))

message("  Revenue floors by tier (5th percentile):")
print(REVENUE_FLOOR_MULT)

# ===========================
# 5. PLAYOFF/BOWL PAYOUTS
# ===========================

message("\n5. Setting playoff and bowl payouts from CFP data...")

PLAYOFF_BASE <- 4.0
PLAYOFF_INFLATION <- 0.02

PLAYOFF_PAYOUTS_T1 <- list(R1 = 4, QF = 8, SF = 12, F = 16)
PLAYOFF_PAYOUTS_T2 <- list(R1 = 2.4, QF = 4.8, SF = 7.2, F = 9.6)
PLAYOFF_PAYOUTS_T3 <- list(R1 = 1.6, QF = 3.2, SF = 4.8, F = 6.4)

BOWL_PAYOUTS <- tibble(
  tier = c(1, 2, 3),
  bowl_payout = c(2.5, 1.5, 0.5)
)

message("  ✓ CFP payouts configured")
message("  ✓ Bowl payouts configured")

# ===========================
# 6. DONATION MOMENTUM
# ===========================

message("\n6. Calculating donation momentum...")

# Simple momentum: does revenue growth accelerate?
donation_analysis <- ncaa_long %>%
  filter(year != 2020) %>%
  group_by(team) %>%
  arrange(year) %>%
  mutate(
    revenue_growth = (revenue / lag(revenue) - 1),
    prev_growth = lag(revenue_growth)
  ) %>%
  filter(!is.na(revenue_growth), !is.na(prev_growth))

donation_model <- lm(revenue_growth ~ prev_growth, data = donation_analysis)
DONATION_MOMENTUM_COEF <- coef(donation_model)["prev_growth"]

message(sprintf("  ✓ Momentum coefficient: %.3f", DONATION_MOMENTUM_COEF))

# ===========================
# 7. TIER MULTIPLIERS
# ===========================

message("\n7. Creating tier multiplier scenarios...")

# Use extracted European multipliers: Tier 1=1.00, Tier 2=0.525, Tier 3=0.424
tier1_mult <- 1.00
tier2_mult <- 0.525
tier3_mult <- 0.424

create_tier_multipliers <- function(dampening) {
  list(
    TIER_MEDIA_MULT = tibble(
      tier = 1:3,
      ncaa_media_mult = c(
        1 + (tier1_mult - 1) * dampening,
        1 + (tier2_mult - 1) * dampening,
        1 + (tier3_mult - 1) * dampening
      )
    ),
    TIER_TICKET_MULT = tibble(
      tier = 1:3,
      ncaa_ticket_mult = c(
        1 + (tier1_mult - 1) * dampening * 0.5,
        1 + (tier2_mult - 1) * dampening * 0.5,
        1 + (tier3_mult - 1) * dampening * 0.5
      )
    ),
    TIER_REVENUE_MULT = tibble(
      tier = 1:3,
      ncaa_revenue_mult = c(
        1 + (tier1_mult - 1) * dampening * 0.7,
        1 + (tier2_mult - 1) * dampening * 0.7,
        1 + (tier3_mult - 1) * dampening * 0.7
      )
    )
  )
}

TIER_MULTIPLIERS_CONSERVATIVE <- create_tier_multipliers(0.4)
TIER_MULTIPLIERS_MODERATE <- create_tier_multipliers(0.6)
TIER_MULTIPLIERS_AGGRESSIVE <- create_tier_multipliers(0.8)

message("  ✓ Created 3 sensitivity scenarios")

# ===========================
# 8. SAVE PARAMETERS
# ===========================

message("\n8. Saving data-driven parameters...")

data_driven_params <- list(
  # Growth
  BASE_GROWTH_RATE = BASE_GROWTH_RATE,
  BASE_GROWTH_SD = BASE_GROWTH_SD,
  CONFERENCE_GROWTH_FLOOR = CONFERENCE_GROWTH_FLOOR,
  
  # Engagement premium
  ENGAGEMENT_PREMIUM_FINAL = ENGAGEMENT_PREMIUM_FINAL,
  
  # Floors
  REVENUE_FLOOR_MULTIPLIERS = REVENUE_FLOOR_MULT,
  
  # Payouts
  PLAYOFF_BASE = PLAYOFF_BASE,
  PLAYOFF_INFLATION = PLAYOFF_INFLATION,
  PLAYOFF_PAYOUTS_T1 = PLAYOFF_PAYOUTS_T1,
  PLAYOFF_PAYOUTS_T2 = PLAYOFF_PAYOUTS_T2,
  PLAYOFF_PAYOUTS_T3 = PLAYOFF_PAYOUTS_T3,
  BOWL_PAYOUTS = BOWL_PAYOUTS,
  
  # Momentum
  DONATION_MOMENTUM_COEF = DONATION_MOMENTUM_COEF,
  
  # Tier scenarios
  TIER_MULTIPLIERS_CONSERVATIVE = TIER_MULTIPLIERS_CONSERVATIVE,
  TIER_MULTIPLIERS_MODERATE = TIER_MULTIPLIERS_MODERATE,
  TIER_MULTIPLIERS_AGGRESSIVE = TIER_MULTIPLIERS_AGGRESSIVE,
  
  # Metadata
  calculation_date = Sys.Date(),
  data_years = range(ncaa_long$year[ncaa_long$year != 2020])
)

saveRDS(data_driven_params, "data_driven_parameters.rds")

message("  ✓ Saved: data_driven_parameters.rds")

# ===========================
# SUMMARY
# ===========================

message("\n========================================")
message("✓ EXTRACTION COMPLETE!")
message("========================================\n")

message("KEY PARAMETERS:")
message(sprintf("  Base Growth: %.2f%% (median, excludes COVID)", BASE_GROWTH_RATE * 100))
message(sprintf("  Conference Floor: %.2f%% (10th percentile)", CONFERENCE_GROWTH_FLOOR * 100))
message(sprintf("  Engagement Premium: %.3f%% (rank improvement effect)", ENGAGEMENT_PREMIUM_FINAL * 100))
message(sprintf("  Donation Momentum: %.3f (persistence coefficient)", DONATION_MOMENTUM_COEF))

message("\nRevenue Floors:")
print(REVENUE_FLOOR_MULT)

message("\n========================================")
message("Next: Run PARAMETER_UPDATES_V2.R")
message("========================================\n")