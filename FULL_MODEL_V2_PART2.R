# ============================================================================
# FULL MODEL V2 - PART 2 (FIXED)
# Relegation simulation, main loop, and Shiny app
# FIX: Prevents 3x revenue inflation in year 1 for relegation scenarios
# ============================================================================

# This continues from FULL_MODEL_V2_PART1.R
# Make updated_params available as 'params' for intensity function
params <- updated_params

# Fix promotion/relegation decay functions
est$promotion_decay <- function(years_since) {
  spike <- updated_params$PROMOTION_SPIKE_MEDIA
  spike * exp(-0.40 * years_since)
}

est$relegation_decay <- function(years_since) {
  spike <- updated_params$RELEGATION_SPIKE_MEDIA
  spike * exp(-0.40 * years_since)
}

# ===========================
# ENHANCED RELEGATION SIMULATION (FIXED)
# ===========================

simulate_relegation_year_v2 <- function(year, state_df, est, tiers, playoff_teams, seed,
                                        team_history=NULL, macro_series=NULL){
  
  n <- nrow(state_df)
  if(n==0) return(tibble())
  
  # Get macro shock
  if(is.null(macro_series)) {
    macro_series <- build_macro_series_adaptive(year:year, est, seed=seed+100)
  }
  macro <- macro_series$macro[macro_series$year == year]
  
  # Simulated strength for ranking
  set.seed(seed + 11)
  noise <- rnorm(n, 0, 0.06)
  base_rev <- pmax(1e-6, state_df$revenue0)
  strength <- base_rev * (1 + noise + state_df$C)
  sim_rank <- rank(-strength, ties.method="first")
  
  state_df$sim_rank <- sim_rank
  
  # Apply promotion/relegation
  state_df <- apply_promotion_relegation(state_df, tiers, playoff_teams)
  
  # Track tier changes
  state_df <- track_tier_changes(state_df)
  
  if(nrow(state_df) == 0) {
    stop("ERROR: All teams lost in promotion/relegation!")
  }
  
  new_tier <- state_df$tier
  
  # Stadium expansion (Tier 1 only)
  if(!is.null(team_history)){
    for(i in seq_len(nrow(state_df))){
      if(state_df$tier[i] == 1){
        tm <- state_df$team[i]
        hist <- team_history %>% filter(team == tm)
        if(should_expand_stadium(hist, state_df$capacity[i], year)){
          state_df$capacity[i] <- expand_stadium_capacity(state_df$capacity[i])
          state_df$expansions[i] <- state_df$expansions[i] + 1
        }
      }
    }
  }
  
  # Get tier multipliers
  tier_media_mult <- get_tier_multiplier(new_tier, "media", est)
  tier_ticket_mult <- get_tier_multiplier(new_tier, "ticket", est)
  tier_other_mult <- get_tier_multiplier(new_tier, "other", est)
  
  # Intensity calculation
  intensity <- est$INTENSITY_FN(
    state_df %>% select(team, tier, sim_rank),
    playoff_teams_per_tier = playoff_teams,
    is_baseline = FALSE
  )
  
  # === ENGAGEMENT PREMIUM (calculated from intensity) ===
  avg_intensity_current <- mean(intensity, na.rm = TRUE)
  baseline_intensity <- 0.119
  engagement_diff <- avg_intensity_current - baseline_intensity
  engagement_premium <- pmax(0, pmin((engagement_diff / 0.10) * 0.005, 0.025))
  
  # Get conference growth
  conf_g <- est$CONF_G_FORECAST %>%
    select(conference, baseline_2025) %>%
    rename(conf_g_rate = baseline_2025)
  
  state_df <- state_df %>% left_join(conf_g, by="conference")
  conf_growth <- coalesce(state_df$conf_g_rate, 0)
  
  # Base growth calculation
  base_growth_rate <- 0.024 + engagement_premium
  macro_multiplier <- 1 + macro
  conf_multiplier <- 1 + conf_growth
  
  base_g_raw <- (1 + base_growth_rate) * macro_multiplier * conf_multiplier - 1
  base_g <- pmax(0.01, base_g_raw)
  
  # Base revenue values
  base_media  <- coalesce(state_df$base_media, 0)
  base_ticket <- coalesce(state_df$base_ticket, 0)
  base_other  <- coalesce(state_df$base_other, 0)
  base_don    <- coalesce(state_df$base_don, 0)
  base_fee    <- coalesce(state_df$base_fee, 0)
  
  # ==================================================
  # CRITICAL FIX: Phase in tier effects over years
  # ==================================================
  
  if(year == 2025) {
    # Year 1: NO tier effects (all scenarios start equal)
    tier_phase_in <- 0
  } else if(year == 2026) {
    # Year 2: 33% tier effect
    tier_phase_in <- 0.33
  } else if(year == 2027) {
    # Year 3: 67% tier effect  
    tier_phase_in <- 0.67
  } else {
    # Year 4+: Full tier effect
    tier_phase_in <- 1.0
  }
  
  # ==================================================
  # 1. MEDIA REVENUE (Tier + Promotion/Relegation spikes)
  # ==================================================
  
  # FIXED: Apply tier effect with phase-in
  tier_media_adjustment <- (tier_media_mult - 1.0) * tier_phase_in
  
  # Promotion/relegation spikes (decay over time)
  promo_spike <- ifelse(
    state_df$years_since_promotion < 5,
    est$promotion_decay(state_df$years_since_promotion),
    0
  )
  
  releg_penalty <- ifelse(
    state_df$years_since_relegation < 5,
    est$relegation_decay(state_df$years_since_relegation),
    0
  )
  
  # Combined media growth
  media_growth <- base_g + tier_media_adjustment + promo_spike + releg_penalty + 0.5 * state_df$C
  media_new <- pmax(0, base_media * (1 + media_growth))
  
  # ==================================================
  # 2. TICKET REVENUE (Tier + Intensity)
  # ==================================================
  
  # FIXED: Apply tier effect with phase-in
  tier_ticket_adjustment <- (tier_ticket_mult - 1.0) * tier_phase_in
  
  intensity_effect <- (est$ALPHA_ATT - 1 + est$ALPHA_PRICE - 1) * intensity * 0.60
  
  ticket_growth <- base_g + tier_ticket_adjustment + intensity_effect
  
  # Saturation - with bounds to prevent collapse
  sat <- soft_ticket_saturation(
    state_df$attendance, 
    state_df$capacity, 
    1 + (est$ALPHA_ATT - 1) * intensity,
    k = est$TICKET_SAT_K
  )
  sat <- pmin(pmax(sat, 0.85), 1.15)
  
  ticket_new <- pmax(base_ticket * 0.90, base_ticket * (1 + ticket_growth) * sat)
  
  # ==================================================
  # 3. OTHER REVENUE (Tier effect)
  # ==================================================
  
  # FIXED: Apply tier effect with phase-in
  tier_other_adjustment <- (tier_other_mult - 1.0) * tier_phase_in
  
  other_growth <- base_g + tier_other_adjustment
  other_new <- pmax(0, base_other * (1 + other_growth))
  
  # ==================================================
  # 4. DONATIONS (Exponential momentum, no direct tier effect)
  # ==================================================
  
  median_don <- median(base_don, na.rm=TRUE)
  donor_saturation <- pmax(0, (base_don - median_don) / 100)
  fatigue_factor <- pmax(1 - (donor_saturation * est$DON$fatigue_rate), 0.50)
  
  momentum_mult <- exp(state_df$C * updated_params$MOMENTUM_DONATION_EXP)
  momentum_mult <- pmin(pmax(momentum_mult, 
                             updated_params$MOMENTUM_DONATION_BOUNDS[1]),
                        updated_params$MOMENTUM_DONATION_BOUNDS[2])
  
  don_base_growth <- 0.02 + 0.02 * intensity + 0.10 * macro
  don_next <- pmax(base_don, 0) * (1 + don_base_growth * fatigue_factor * momentum_mult)
  
  # ==================================================
  # 5. STUDENT FEES (Small tier adjustment)
  # ==================================================
  
  # FIXED: Apply tier effect with phase-in
  tier_fee_adjustment <- (tier_media_mult - 1.0) * 0.5 * tier_phase_in
  fee_growth <- 0.015 + 0.05 * macro + tier_fee_adjustment
  fee_next <- pmax(base_fee, 0) * (1 + fee_growth)
  
  # ==================================================
  # 6. PLAYOFF BONUSES (with inflation)
  # ==================================================
  
  year_mult <- (1 + updated_params$PLAYOFF_INFLATION_RATE)^(year - 2025)
  
  tmp <- tibble(team=state_df$team, conference=state_df$conference, 
                tier=new_tier, sim_rank=sim_rank)
  bonus_vec <- assign_playoff_bonuses(tmp, playoff_teams, rng_seed=seed+777, by_tier=TRUE)
  rev_bon <- bonus_vec[match(state_df$team, names(bonus_vec))]
  rev_bon[is.na(rev_bon)] <- 0
  rev_bon <- rev_bon * year_mult
  
  # ==================================================
  # TOTAL REVENUE & FLOORS
  # ==================================================
  
  rev_final <- media_new + ticket_new + other_new + don_next + fee_next + rev_bon
  
  # Tier-dependent revenue floors
  floor_pct <- est$REV_FLOOR_MULT$floor_pct[match(new_tier, est$REV_FLOOR_MULT$tier)]
  floor_pct[is.na(floor_pct)] <- 0.40
  revenue_floor <- state_df$revenue0 * floor_pct
  
  rev_final <- pmax(rev_final, revenue_floor)
  
  # ==================================================
  # EXPENSES
  # ==================================================
  
  gr <- (rev_final / pmax(state_df$revenue0, 1e-6)) - 1
  exp0 <- coalesce(state_df$expenses, 0.75 * state_df$revenue0)
  expenses <- pmax(0, pmax(exp0, 0.75 * state_df$revenue0) * (1 + est$EXP$lambda * gr))
  
  # ==================================================
  # RETURN
  # ==================================================
  
  tibble(
    team = state_df$team, conference = state_df$conference, year = year,
    tier = new_tier, sim_rank = sim_rank, last_tier = new_tier,
    revenue = rev_final, expenses = expenses, profit = rev_final - expenses,
    rev_media = media_new, rev_ticket = ticket_new, rev_don = don_next,
    rev_fee = fee_next, rev_other = other_new, rev_bonuses = as.numeric(rev_bon),
    attendance = state_df$attendance, capacity = state_df$capacity, 
    expansions = state_df$expansions,
    intensity = intensity,
    revenue0 = rev_final,
    base_media = media_new, base_ticket = ticket_new, base_don = don_next,
    base_fee = fee_next, base_other = other_new, C = state_df$C,
    years_since_promotion = state_df$years_since_promotion,
    years_since_relegation = state_df$years_since_relegation
  )
}

message("  ??? Enhanced relegation simulation function created (FIXED)\n")

# ===========================
# MAIN SIMULATION LOOP
# ===========================

simulate_all_v2 <- function(anchor_2024, ncaa_long, euro_long, YEARS_SIM, est,
                            n_simulations = 100, seed_base = 4242){
  
  message(sprintf("Running %d simulations...", n_simulations))
  
  all_runs <- map_dfr(1:n_simulations, function(sim_id) {
    
    if(sim_id %% 10 == 0) message(sprintf("  - Simulation %d/%d", sim_id, n_simulations))
    
    seed <- seed_base + sim_id * 1000
    
    macro_series <- build_macro_series_adaptive(YEARS_SIM, est, seed=seed+100)
    
    # ===========================
    # BASELINE SCENARIOS
    # ===========================
    
    baseline_results <- map_dfr(updated_params$BASELINE_SCENARIOS, function(playoff_size) {
      
      state <- build_state0_v2(anchor_2024, tier_breakpoints = updated_params$TIER_BREAKPOINTS)
      last_outcomes <- tibble(team=state$team, bowl_win=0, ny6_app=0, nat_app=0, nat_win=0, rank=50)
      history <- list()
      results <- list()
      
      for(yy in YEARS_SIM) {
        state <- update_momentum(state, est$MOM, last_outcomes)
        
        results[[as.character(yy)]] <- simulate_baseline_year_v2(
          yy, state, est, playoff_teams=playoff_size, seed=seed + yy,
          team_history=if(length(history)>0) bind_rows(history) else NULL,
          macro_series=macro_series
        )
        
        history[[as.character(yy)]] <- results[[as.character(yy)]]
        last_outcomes <- derive_outcomes(results[[as.character(yy)]])
        state <- results[[as.character(yy)]] %>% select(-year)
      }
      
      bind_rows(results) %>%
        mutate(scenario_id = paste0("baseline_", playoff_size, "team"))
    })
    
    # ===========================
    # RELEGATION SCENARIOS
    # ===========================
    
    rel_grid <- expand.grid(tiers=3L, playoff_teams=c(4L, 8L, 12L)) %>% as_tibble()
    
    relegation_results <- map2_dfr(rel_grid$tiers, rel_grid$playoff_teams, function(TI, PO) {
      
      state <- build_state0_v2(anchor_2024, tier_breakpoints = updated_params$TIER_BREAKPOINTS)
      last_outcomes <- tibble(team=state$team, bowl_win=0, ny6_app=0, nat_app=0, nat_win=0, rank=50)
      history <- list()
      results <- list()
      
      for(yy in YEARS_SIM) {
        state <- update_momentum(state, est$MOM, last_outcomes)
        
        results[[as.character(yy)]] <- simulate_relegation_year_v2(
          yy, state, est, tiers=TI, playoff_teams=PO,
          seed=seed + yy + TI*100 + PO,
          team_history=if(length(history)>0) bind_rows(history) else NULL,
          macro_series=macro_series
        )
        
        history[[as.character(yy)]] <- results[[as.character(yy)]]
        last_outcomes <- derive_outcomes(results[[as.character(yy)]])
        state <- results[[as.character(yy)]] %>% select(-year)
      }
      
      bind_rows(results) %>%
        mutate(scenario_id = paste0("releg_T", TI, "_P", PO))
    })
    
    bind_rows(baseline_results, relegation_results) %>%
      mutate(sim_run = sim_id)
    
  }, .progress = FALSE)
  
  message("  ??? All simulations complete\n")
  
  # Aggregate across simulation runs
  message("Aggregating results across simulation runs...")
  
  aggregated <- all_runs %>%
    group_by(scenario_id, year, team, conference) %>%
    summarise(
      tier = if(all(is.na(tier))) NA_integer_ else as.integer(round(median(tier, na.rm = TRUE))),
      revenue = mean(revenue, na.rm=TRUE),
      expenses = mean(expenses, na.rm=TRUE),
      profit = mean(profit, na.rm=TRUE),
      rev_media = mean(rev_media, na.rm=TRUE),
      rev_ticket = mean(rev_ticket, na.rm=TRUE),
      rev_don = mean(rev_don, na.rm=TRUE),
      rev_fee = mean(rev_fee, na.rm=TRUE),
      rev_other = mean(rev_other, na.rm=TRUE),
      rev_bonuses = mean(rev_bonuses, na.rm=TRUE),
      attendance = mean(attendance, na.rm=TRUE),
      capacity = mean(capacity, na.rm=TRUE),
      expansions = mean(expansions, na.rm=TRUE),
      intensity = mean(intensity, na.rm=TRUE),
      sim_rank = median(sim_rank, na.rm=TRUE),
      C = mean(C, na.rm=TRUE),
      revenue_sd = sd(revenue, na.rm=TRUE),
      profit_sd = sd(profit, na.rm=TRUE),
      .groups = "drop"
    )
  
  message("  ??? Aggregation complete\n")
  
  return(aggregated)
}

# ===========================
# AGGREGATION FUNCTION
# ===========================

build_aggregates <- function(sims){
  sims <- sims %>% mutate(across(c(revenue, expenses, profit, rev_media, rev_ticket, 
                                   rev_don, rev_fee, rev_other, rev_bonuses, attendance),
                                 ~ suppressWarnings(as.numeric(.))))
  
  national <- sims %>% group_by(scenario_id, year) %>%
    summarise(
      total_revenue  = sum(revenue, na.rm=TRUE),
      total_expenses = sum(expenses, na.rm=TRUE),
      total_profit   = sum(profit, na.rm=TRUE),
      total_attendance = sum(attendance, na.rm=TRUE),
      rev_media=sum(rev_media,na.rm=TRUE),
      rev_ticket=sum(rev_ticket,na.rm=TRUE),
      rev_don=sum(rev_don,na.rm=TRUE),
      rev_fee=sum(rev_fee,na.rm=TRUE),
      rev_other=sum(rev_other,na.rm=TRUE),
      rev_bonuses=sum(rev_bonuses,na.rm=TRUE),
      .groups="drop"
    ) %>%
    group_by(scenario_id) %>%
    mutate(npv_profit_7 = present_value(total_profit, year, DISCOUNT_RATE)) %>% ungroup()
  
  conference <- sims %>% filter(!is.na(conference), !grepl("^\\d+$", conference)) %>%
    group_by(scenario_id, year, conference) %>%
    summarise(
      conf_revenue=sum(revenue,na.rm=TRUE),
      conf_expenses=sum(expenses,na.rm=TRUE),
      conf_profit=sum(profit,na.rm=TRUE),
      rev_media=sum(rev_media,na.rm=TRUE),
      rev_ticket=sum(rev_ticket,na.rm=TRUE),
      rev_don=sum(rev_don,na.rm=TRUE),
      rev_fee=sum(rev_fee,na.rm=TRUE),
      rev_other=sum(rev_other,na.rm=TRUE),
      rev_bonuses=sum(rev_bonuses,na.rm=TRUE),
      .groups="drop"
    )
  
  team <- sims %>% select(scenario_id, year, team, conference, tier, sim_rank,
                          revenue, expenses, profit, attendance, capacity, expansions,
                          rev_media, rev_ticket, rev_don, rev_fee, rev_other, rev_bonuses, 
                          C, intensity)
  
  list(national=national, conference=conference, team=team)
}

present_value <- function(values, years, rate){
  t <- years - min(years) + 1
  sum(values / (1 + rate)^t, na.rm=TRUE)
}

# ===========================
# EXECUTION INSTRUCTIONS
# ===========================

message("\n========================================")
message("PART 2 LOADED - FUNCTIONS READY (FIXED)")
message("========================================\n")

message("CRITICAL FIX APPLIED:")
message("  ??? Tier effects now phase in over 3 years")
message("  ??? Year 1 (2025): 0% tier effect")
message("  ??? Year 2 (2026): 33% tier effect")
message("  ??? Year 3 (2027): 67% tier effect")
message("  ??? Year 4+ (2028+): 100% tier effect\n")

message("This prevents the 3x revenue inflation bug in relegation scenarios.\n")

message("To run simulation:")
message("  sims_v2 <- simulate_all_v2(anchor_2024, ncaa_long, euro_long, YEARS_SIM, est, n_simulations = 100, seed_base = 4242)")
message("  agg_v2 <- build_aggregates(sims_v2)")
message("  saveRDS(list(sims=sims_v2, national=agg_v2$national, conference=agg_v2$conference, team=agg_v2$team), 'model_v2_results.rds')\n")