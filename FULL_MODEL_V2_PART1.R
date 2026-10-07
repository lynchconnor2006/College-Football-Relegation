# ============================================================================
# FULL MODEL V2 - INTEGRATED VERSION
# Purpose: Complete simulation with all user-requested changes
# Run this FOURTH after INTENSITY_FIX.R
# ============================================================================
# This file integrates:
# - Data-driven parameters (from DATA_EXTRACTION.R)
# - Updated settings (from PARAMETER_UPDATES.R)  
# - Enhanced intensity (from INTENSITY_FIX.R)
# - All fixes from handoff document
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(stringi)
  library(scales)
  library(DT)
  library(shiny)
})

message("\n========================================")
message("CFB RELEGATION MODEL V2")
message("Data-Driven Simulation")
message("========================================\n")

# ===========================
# LOAD ALL DEPENDENCIES
# ===========================

message("Loading dependencies...")

# Check all prerequisite files exist
required_files <- c(
  "extracted_parameters.rds",
  "updated_parameters.rds",
  "intensity_functions.rds",
  "cfb_enhanced_part1.R",
  "cfb_enhanced_part2.R",
  "cfb_enhanced_part3.R"
)

missing <- required_files[!file.exists(required_files)]
if(length(missing) > 0) {
  stop(sprintf("ERROR: Missing files: %s\nRun scripts 1-3 first!", paste(missing, collapse=", ")))
}

# Load original functions (Part 1-3)
source("cfb_enhanced_part1.R")
source("cfb_enhanced_part2.R")
source("cfb_enhanced_part3.R")

# Load new parameters and functions
extracted_params <- readRDS("extracted_parameters.rds")
updated_params <- readRDS("updated_parameters.rds")
intensity_fns <- readRDS("intensity_functions.rds")

message("  ✓ All dependencies loaded\n")

# ===========================
# CONFIG
# ===========================

YEARS_HIST <- 2015:2024
YEARS_SIM  <- 2025:2034
DISCOUNT_RATE <- 0.07
RUN_SHINY <- TRUE
REVENUE_CONSERVATION <- FALSE
N_SIMULATIONS <- 100  # Run 100 simulations and average

# File paths (from Part 1)
ncaa_path   <- "C:\\Users\\lynch\\OneDrive\\Documents\\College Football Relegation Teams.xlsx"
europe_path <- "C:\\Users\\lynch\\OneDrive\\Documents\\European Soccer Teams.xlsx"

# ===========================
# LOAD DATA
# ===========================

message("Loading data...")
ing <- ingest_all(ncaa_path, europe_path, YEARS_HIST)
ncaa_wide   <- read_first_sheet(ncaa_path)
ncaa_long   <- ing$ncaa_long
euro_long   <- ing$euro_long
anchor_2024 <- ing$anchor_2024

# ===========================
# BUILD ENHANCED ESTIMATION OBJECT
# ===========================

message("Building enhanced estimation object...")

# Start with original estimation
est_original <- estimate_all(ncaa_long, euro_long, anchor_2024, YEARS_HIST, YEARS_SIM, ncaa_wide)

# Override with data-driven parameters
est <- est_original

# 1. Growth bounds
est$BASE_CLAMP <- updated_params$BASE_CLAMP

# 2. Macro model (adaptive rho)
est$MACRO$sd <- updated_params$MACRO_SD
est$MACRO$rho_success <- updated_params$MACRO_RHO_SUCCESS
est$MACRO$rho_sustained_success <- updated_params$MACRO_RHO_SUSTAINED_SUCCESS
est$MACRO$rho_failure <- updated_params$MACRO_RHO_FAILURE
est$MACRO$rho_sustained_failure <- updated_params$MACRO_RHO_SUSTAINED_FAILURE

# 3. Conference growth (time-varying)
est$CONF_G_FORECAST <- updated_params$conf_growth_forecast

# 4. Team & conference volatility (data-driven)
est$TEAM_VOL <- updated_params$team_volatility
est$CONF_VOL <- updated_params$conf_volatility

# 5. Tier multipliers (from Europe data)
est$TIER_MEDIA_MULT <- updated_params$TIER_MEDIA_MULT
est$TIER_TICKET_MULT <- updated_params$TIER_TICKET_MULT
est$TIER_REVENUE_MULT <- updated_params$TIER_REVENUE_MULT

# 6. Promotion/relegation spikes
est$PROMOTION_SPIKE <- updated_params$PROMOTION_SPIKE_MEDIA
est$RELEGATION_SPIKE <- updated_params$RELEGATION_SPIKE_MEDIA
est$promotion_decay <- updated_params$promotion_decay_function
est$relegation_decay <- updated_params$relegation_decay_function

# 7. Momentum
est$MOM$cap <- updated_params$MOMENTUM_CAP
est$MOM$decay <- updated_params$MOMENTUM_DECAY
est$MOMENTUM_BOOST_SUSTAINED <- updated_params$MOMENTUM_BOOST_SUSTAINED
est$MOMENTUM_PENALTY_SUSTAINED <- updated_params$MOMENTUM_PENALTY_SUSTAINED

# 8. Intensity
est$INTENSITY_FN <- intensity_fns$calculate_intensity_enhanced

# 9. Revenue floors (tier-dependent)
est$REV_FLOOR_MULT <- updated_params$REVENUE_FLOOR_MULTIPLIERS

message("  ✓ Enhanced estimation object created\n")

# ===========================
# ENHANCED STATE INITIALIZATION
# ===========================

build_state0_v2 <- function(anchor_2024, tier_breakpoints = c(0.19, 0.50)) {
  # Rank-based tier assignment (NOT revenue-based)
  st <- anchor_2024
  
  need_num <- c("revenue","expenses","media_rights","ticket_sales","donations",
                "student_fees","other","attendance","capacity","avg_ticket_price","rank")
  for (nm in need_num) if (!nm %in% names(st)) st[[nm]] <- NA_real_
  
  st <- st %>%
    mutate(across(all_of(need_num), ~ as.numeric(.))) %>%
    mutate(
      revenue0   = as.numeric(pmax(revenue, 1e-6)),
      base_media = as.numeric(coalesce(media_rights, 0)),
      base_ticket= as.numeric(coalesce(ticket_sales, 0)),
      base_don   = as.numeric(coalesce(donations, 0)),
      base_fee   = as.numeric(coalesce(student_fees, 0)),
      base_other = as.numeric(coalesce(other, 0)),
      expenses   = as.numeric(coalesce(expenses, 0.75 * revenue0)),
      attendance = as.numeric(coalesce(attendance, 0)),
      capacity   = as.numeric(ifelse(is.finite(capacity) & capacity>0, capacity, pmax(attendance, 1))),
      C          = 0,
      expansions = 0,
      years_since_promotion = 999,   # Track promotion/relegation timing
      years_since_relegation = 999
    )
  
  # TIER ASSIGNMENT (by rank percentile)
  n <- nrow(st)
  pct <- rank(st$rank, ties.method="first") / n
  
  tier_map <- tibble(
    team = st$team,
    tier = case_when(
      pct <= tier_breakpoints[1] ~ 1L,
      pct <= tier_breakpoints[2] ~ 2L,
      TRUE ~ 3L
    )
  )
  
  st %>% 
    left_join(tier_map, by="team") %>% 
    mutate(last_tier = tier) %>%
    select(-any_of("rank"))
}

# ===========================
# ADAPTIVE MACRO SERIES
# ===========================

build_macro_series_adaptive <- function(years, est, seed=1234L){
  # Adaptive rho based on streak direction
  set.seed(seed)
  n <- length(years)
  z <- numeric(n)
  e <- rnorm(n, 0, est$MACRO$sd)
  
  for(t in seq_along(years)) {
    if(t == 1) {
      z[t] <- e[t]
    } else {
      # Check for sustained streaks
      if(t >= 3) {
        last_2 <- z[(t-2):(t-1)]
        if(all(last_2 > 0)) {
          # Sustained success - higher persistence
          rho <- est$MACRO$rho_sustained_success
        } else if(all(last_2 < 0)) {
          # Sustained failure - higher persistence
          rho <- est$MACRO$rho_sustained_failure
        } else {
          # Mixed or first occurrence - lower persistence
          rho <- ifelse(z[t-1] > 0, est$MACRO$rho_success, est$MACRO$rho_failure)
        }
      } else {
        # Not enough history - use base rho
        rho <- est$MACRO$rho_success
      }
      
      z[t] <- rho * z[t-1] + e[t]
    }
  }
  
  tibble(year = years, macro = z)
}

# ===========================
# TIER-SPECIFIC REVENUE MULTIPLIERS
# ===========================

get_tier_multiplier <- function(tier, component, est) {
  # Get the correct lookup table
  lookup <- switch(component,
                   media = est$TIER_MEDIA_MULT,
                   ticket = est$TIER_TICKET_MULT,
                   other = est$TIER_REVENUE_MULT,
                   est$TIER_REVENUE_MULT)
  
  # Get the correct column name
  col_name <- switch(component,
                     media = "ncaa_media_mult",
                     ticket = "ncaa_ticket_mult",
                     other = "ncaa_revenue_mult",
                     "ncaa_revenue_mult")
  
  # Safety check
  if(!col_name %in% names(lookup)) {
    warning(sprintf("Column '%s' not found in lookup table. Available: %s", 
                    col_name, paste(names(lookup), collapse=", ")))
    return(rep(1.0, length(tier)))
  }
  
  # Match tiers
  mult <- lookup[[col_name]][match(tier, lookup$tier)]
  mult[is.na(mult)] <- 1.0
  
  return(mult)
}

# ===========================
# PROMOTION/RELEGATION TRACKING
# ===========================

track_tier_changes <- function(state_df) {
  # Update years_since_promotion and years_since_relegation
  state_df %>%
    mutate(
      promoted = as.integer(tier < last_tier),
      relegated = as.integer(tier > last_tier),
      years_since_promotion = case_when(
        promoted == 1 ~ 0L,
        years_since_promotion == 999 ~ 999L,
        TRUE ~ years_since_promotion + 1L
      ),
      years_since_relegation = case_when(
        relegated == 1 ~ 0L,
        years_since_relegation == 999 ~ 999L,
        TRUE ~ years_since_relegation + 1L
      )
    )
}

# ===========================
# ENHANCED BASELINE SIMULATION
# ===========================

simulate_baseline_year_v2 <- function(year, state_df, est, playoff_teams, seed, 
                                      team_history=NULL, macro_series=NULL){
  
  # Get macro shock
  if(is.null(macro_series)) {
    macro_series <- build_macro_series_adaptive(year:year, est, seed=seed+100)
  }
  macro <- macro_series$macro[macro_series$year == year]
  
  clamp_lo <- est$BASE_CLAMP[1]
  clamp_hi <- est$BASE_CLAMP[2]
  
  # Join conference growth
  conf_g <- est$CONF_G_FORECAST %>% 
    select(conference, baseline_2025) %>%
    rename(conf_g_rate = baseline_2025)
  
  team_vol <- est$TEAM_VOL %>% select(team, team_vol)
  
  out <- state_df %>%
    left_join(conf_g, by="conference") %>%
    left_join(team_vol, by="team") %>%
    mutate(
      conf_g_rate = coalesce(conf_g_rate, 0),
      team_vol = coalesce(team_vol, 0.02),
      base_rev = coalesce(revenue0, revenue)
    )
  
  # Growth rate
  set.seed(seed + year)
  out <- out %>%
    rowwise() %>%
    mutate(
      g = pmax(clamp_lo, pmin(clamp_hi, 
                              (1 + 0.025) * (1 + macro) * (1 + conf_g_rate + rnorm(1, 0, team_vol)) - 1))
    ) %>%
    ungroup()
  
  # Stadium expansion (Tier 1 only in baseline? No tiers in baseline - keep old logic)
  if(!is.null(team_history)){
    for(i in seq_len(nrow(out))){
      tm <- out$team[i]
      hist <- team_history %>% filter(team == tm)
      if(should_expand_stadium(hist, out$capacity[i], year)){
        out$capacity[i] <- expand_stadium_capacity(out$capacity[i])
        out$expansions[i] <- out$expansions[i] + 1
      }
    }
  }
  
  # Simulated strength for ranking
  set.seed(seed + 42)
  shock_seed <- rnorm(nrow(out), 0, 0.05)
  strength <- out$base_rev * (1 + shock_seed + out$C)
  out$sim_rank <- rank(-strength, ties.method="first")
  
  # Playoff bonuses
  bonus_vec <- assign_playoff_bonuses(
    out %>% select(team, conference, sim_rank, tier), 
    playoff_teams,
    rng_seed = seed + 501, 
    by_tier = FALSE
  )
  rev_bon <- bonus_vec[match(out$team, names(bonus_vec))]
  rev_bon[is.na(rev_bon)] <- 0
  
  # Intensity calculation
  intensity <- est$INTENSITY_FN(
    out %>% select(team, tier, sim_rank),
    playoff_teams_per_tier = playoff_teams,
    is_baseline = TRUE
  )
  
  # Revenue components
  # 1. Media
  media_next <- pmax(out$base_media, 0) * (1 + out$g)
  
  # 2. Tickets (with intensity boost)
  ticket_intensity_mult <- 1 + (est$ALPHA_ATT - 1) * intensity * 0.60  # Increased from 0.30
  sat <- soft_ticket_saturation(out$attendance, out$capacity, ticket_intensity_mult, k = est$TICKET_SAT_K)
  ticket_next <- pmax(out$base_ticket, 0) * (1 + out$g) * sat
  
  # 3. Donations (with exponential momentum effect)
  median_don <- median(out$base_don, na.rm=TRUE)
  donor_saturation <- pmax(0, (out$base_don - median_don) / 100)
  fatigue_factor <- pmax(1 - (donor_saturation * est$DON$fatigue_rate), 0.50)
  
  momentum_mult <- exp(out$C * updated_params$MOMENTUM_DONATION_EXP)
  momentum_mult <- pmin(pmax(momentum_mult, 
                             updated_params$MOMENTUM_DONATION_BOUNDS[1]),
                        updated_params$MOMENTUM_DONATION_BOUNDS[2])
  
  don_base_growth <- est$DON$base + est$DON$stakes * intensity + est$DON$macro * macro
  don_next <- pmax(out$base_don, 0) * (1 + don_base_growth * fatigue_factor * momentum_mult)
  
  # 4. Student fees
  fee_next <- pmax(out$base_fee, 0) * (1 + 0.015 + 0.05 * macro)
  
  # 5. Other
  other_next <- pmax(out$base_other, 0) * (1 + out$g)
  
  # Total revenue
  revenue_final <- media_next + ticket_next + don_next + fee_next + other_next + rev_bon
  revenue_final[!is.finite(revenue_final)] <- 1e-6
  
  # Apply revenue floor (40% of starting for baseline)
  revenue_floor <- out$base_rev * 0.40
  revenue_final <- pmax(revenue_final, revenue_floor)
  
  # Expenses
  gr <- (revenue_final / pmax(out$base_rev, 1e-6)) - 1
  expenses <- pmax(0, pmax(out$expenses, 0.75 * out$base_rev) * (1 + est$EXP$lambda * gr))
  
  tibble(
    team = out$team, conference = out$conference, year = year,
    tier = NA_integer_, sim_rank = out$sim_rank, last_tier = NA_integer_,
    revenue = revenue_final, expenses = expenses, profit = revenue_final - expenses,
    rev_media = media_next, rev_ticket = ticket_next, rev_don = don_next,
    rev_fee = fee_next, rev_other = other_next, rev_bonuses = as.numeric(rev_bon),
    attendance = out$attendance, capacity = out$capacity, expansions = out$expansions,
    intensity = intensity,
    revenue0 = revenue_final,
    base_media = media_next, base_ticket = ticket_next, base_don = don_next,
    base_fee = fee_next, base_other = other_next, C = out$C,
    years_since_promotion = 999L, years_since_relegation = 999L
  )
}

message("  ✓ Enhanced baseline simulation function created\n")

# File is getting long - continue in FULL_MODEL_V2_PART2.R
message("Continuing in FULL_MODEL_V2_PART2.R...")
