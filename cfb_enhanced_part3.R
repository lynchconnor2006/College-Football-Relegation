# ============================================================================
# COLLEGE FOOTBALL RELEGATION MODEL - ENHANCED VERSION
# PART 3 of 4: Playoff Bonuses, Stadium Expansion, and Simulation Functions
# ============================================================================
# Run Parts 1 and 2 before this file

# =========================
# PLAYOFF BONUSES & SHARING
# =========================

PLAYOFF_R1_12_16    <- 10
PLAYOFF_R2_12_16    <- 13
PLAYOFF_R3_12_16    <- 16
PLAYOFF_FINAL_12_16 <- 18

PLAYOFF_R1_8        <- 12
PLAYOFF_R2_8        <- 15
PLAYOFF_FINAL_8     <- 18

PLAYOFF_R1_4        <- 15
PLAYOFF_FINAL_4     <- 18

round_payouts <- function(playoff_teams){
  if(playoff_teams==16L) c(PLAYOFF_R1_12_16, PLAYOFF_R2_12_16, PLAYOFF_R3_12_16, PLAYOFF_FINAL_12_16)
  else if(playoff_teams==12L) c(PLAYOFF_R1_12_16, PLAYOFF_R2_12_16, PLAYOFF_R3_12_16, PLAYOFF_FINAL_12_16)
  else if(playoff_teams==8L)  c(PLAYOFF_R1_8, PLAYOFF_R2_8, PLAYOFF_FINAL_8)
  else if(playoff_teams==4L)  c(PLAYOFF_R1_4, PLAYOFF_FINAL_4)
  else stop("Unsupported playoff size.")
}

win_prob_higher_seed <- function(seed_hi, seed_lo, k=0.45){
  delta <- pmax(1, seed_lo - seed_hi)
  1 / (1 + exp(-k * delta))
}

play_game <- function(seedA, seedB, round_pay){
  if(seedA > seedB){ tmp<-seedA; seedA<-seedB; seedB<-tmp }
  p_hi <- win_prob_higher_seed(seedA, seedB)
  w_hi <- as.logical(rbinom(1,1,prob=p_hi))
  winner <- if(w_hi) seedA else seedB
  list(winner=winner, pay=setNames(rep(round_pay,2), c(as.character(seedA), as.character(seedB))))
}

simulate_playoff_with_bonuses <- function(playoff_teams, seed_vec_named, rng_seed=1L){
  set.seed(rng_seed); rp <- round_payouts(playoff_teams)
  bonus <- numeric(length(seed_vec_named)); names(bonus) <- names(seed_vec_named)
  pay_round <- function(pay_list){ 
    for(pl in pay_list){ 
      nm <- names(pl)
      for(i in seq_along(pl)) bonus[nm[i]] <<- bonus[nm[i]] + pl[i] 
    } 
  }
  
  if(playoff_teams==16L){
    pairs <- list(c(1,16),c(8,9),c(5,12),c(4,13),c(6,11),c(3,14),c(7,10),c(2,15))
    winners <- integer(); paylist <- list()
    for(pr in pairs){ 
      res<-play_game(pr[1],pr[2],rp[1])
      winners<-c(winners,res$winner)
      paylist<-c(paylist,list(res$pay)) 
    }
    pay_round(paylist)
    
    pairs_qf <- list(c(winners[1],winners[2]),c(winners[3],winners[4]),
                     c(winners[5],winners[6]),c(winners[7],winners[8]))
    winners_qf <- integer(); paylist <- list()
    for(pr in pairs_qf){ 
      res<-play_game(pr[1],pr[2],rp[2])
      winners_qf<-c(winners_qf,res$winner)
      paylist<-c(paylist,list(res$pay)) 
    }
    pay_round(paylist)
    
    pairs_sf <- list(c(winners_qf[1],winners_qf[2]), c(winners_qf[3],winners_qf[4]))
    winners_sf <- integer(); paylist <- list()
    for(pr in pairs_sf){ 
      res<-play_game(pr[1],pr[2],rp[3])
      winners_sf<-c(winners_sf,res$winner)
      paylist<-c(paylist,list(res$pay)) 
    }
    pay_round(paylist)
    
    res <- play_game(winners_sf[1], winners_sf[2], rp[4])
    pay_round(list(res$pay))
    
  } else if(playoff_teams==12L){
    winners_r1 <- integer(); paylist <- list()
    for(pr in list(c(5,12),c(8,9),c(6,11),c(7,10))){
      res<-play_game(pr[1],pr[2],rp[1])
      winners_r1<-c(winners_r1,res$winner)
      paylist<-c(paylist,list(res$pay))
    }
    pay_round(paylist)
    
    map <- list(c(1,winners_r1[2]), c(4,winners_r1[1]), c(2,winners_r1[4]), c(3,winners_r1[3]))
    winners_qf <- integer(); paylist <- list()
    for(pr in map){ 
      res<-play_game(pr[1],pr[2],rp[2])
      winners_qf<-c(winners_qf,res$winner)
      paylist<-c(paylist,list(res$pay)) 
    }
    pay_round(paylist)
    
    pairs_sf <- list(c(winners_qf[1],winners_qf[2]), c(winners_qf[3],winners_qf[4]))
    winners_sf <- integer(); paylist <- list()
    for(pr in pairs_sf){ 
      res<-play_game(pr[1],pr[2],rp[3])
      winners_sf<-c(winners_sf,res$winner)
      paylist<-c(paylist,list(res$pay)) 
    }
    pay_round(paylist)
    
    res <- play_game(winners_sf[1], winners_sf[2], rp[4])
    pay_round(list(res$pay))
    
  } else if(playoff_teams==8L){
    winners_qf <- integer(); paylist <- list()
    for(pr in list(c(1,8),c(4,5),c(3,6),c(2,7))){
      res<-play_game(pr[1],pr[2],rp[1])
      winners_qf<-c(winners_qf,res$winner)
      paylist<-c(paylist,list(res$pay))
    }
    pay_round(paylist)
    
    pairs_sf <- list(c(winners_qf[1],winners_qf[2]), c(winners_qf[3],winners_qf[4]))
    winners_sf <- integer(); paylist <- list()
    for(pr in pairs_sf){ 
      res<-play_game(pr[1],pr[2],rp[2])
      winners_sf<-c(winners_sf,res$winner)
      paylist<-c(paylist,list(res$pay)) 
    }
    pay_round(paylist)
    
    res <- play_game(winners_sf[1], winners_sf[2], rp[3])
    pay_round(list(res$pay))
    
  } else if(playoff_teams==4L){
    res1 <- play_game(1,4,rp[1])
    res2 <- play_game(2,3,rp[1])
    pay_round(list(res1$pay,res2$pay))
    resf <- play_game(res1$winner, res2$winner, rp[2])
    pay_round(list(resf$pay))
  } else stop("Unsupported playoff size.")
  
  out <- setNames(numeric(length(seed_vec_named)), unname(seed_vec_named))
  for(s in names(seed_vec_named)){ 
    tm <- seed_vec_named[[s]]
    out[tm] <- out[tm] + (bonus[s] %||% 0) 
  }
  out
}

# ENHANCED: Tier-based sharing (higher tiers share more)
apply_conference_sharing <- function(df_year, raw_bonus_by_team, tier_info=NULL){
  bonus <- raw_bonus_by_team
  confs <- df_year %>% select(team, conference)
  
  # ENHANCEMENT: Different sharing fractions by tier
  if(!is.null(tier_info)){
    confs <- confs %>% left_join(tier_info %>% select(team, tier), by="team")
    confs <- confs %>% mutate(
      share_frac = case_when(
        tier == 1 ~ 0.30,  # Tier 1: 30% shared
        tier == 2 ~ 0.25,  # Tier 2: 25% shared
        TRUE ~ 0.20        # Tier 3+: 20% shared
      )
    )
  } else {
    confs$share_frac <- 0.25
  }
  
  out <- setNames(rep(0, nrow(confs)), confs$team)
  for(i in seq_len(nrow(confs))){
    tm <- confs$team[i]
    cf <- confs$conference[i] %||% NA_character_
    b <- bonus[tm] %||% 0
    share_frac <- confs$share_frac[i]
    
    if(!is.na(cf) && tolower(cf)!="independent"){
      keep <- b * (1 - share_frac)
      pool <- b * share_frac
      peers <- confs$team[confs$conference==cf & confs$team!=tm]
      if(length(peers)>0){
        out[peers] <- out[peers] + pool/length(peers)
      } else {
        keep <- b
      }
      out[tm] <- out[tm] + keep
    } else {
      out[tm] <- out[tm] + b
    }
  }
  out
}

assign_playoff_bonuses <- function(df, playoff_teams, rng_seed, by_tier=FALSE){
  if(!by_tier){
    sub <- df %>% arrange(sim_rank)
    K <- min(nrow(sub), playoff_teams)
    if(K<=0) return(setNames(rep(0, nrow(df)), df$team))
    seeds_named <- setNames(sub$team[seq_len(K)], as.character(seq_len(K)))
    raw_add <- simulate_playoff_with_bonuses(playoff_teams, seed_vec_named=seeds_named, rng_seed=rng_seed)
    shared <- apply_conference_sharing(df_year = df %>% select(team, conference), 
                                      raw_bonus_by_team = raw_add,
                                      tier_info = df %>% select(team, tier))
    out <- setNames(rep(0, nrow(df)), df$team)
    out[names(shared)] <- shared
    out
  } else {
    total <- setNames(rep(0, nrow(df)), df$team)
    for(tt in sort(unique(df$tier))){
      sub <- df %>% filter(tier==tt) %>% arrange(sim_rank)
      K <- min(nrow(sub), playoff_teams)
      if(K<=0){ next }
      seeds_named <- setNames(sub$team[seq_len(K)], as.character(seq_len(K)))
      raw_add <- simulate_playoff_with_bonuses(playoff_teams, seed_vec_named=seeds_named, 
                                               rng_seed=rng_seed + tt*1000)
      shared <- apply_conference_sharing(df_year = sub %>% select(team, conference), 
                                        raw_bonus_by_team = raw_add,
                                        tier_info = sub %>% select(team, tier))
      total[names(shared)] <- total[names(shared)] + shared
    }
    total
  }
}

# =========================
# STADIUM EXPANSION MODEL
# =========================

# ENHANCED: Realistic stadium expansion with multiple criteria
should_expand_stadium <- function(team_history, current_capacity, year){
  if(nrow(team_history) < 3) return(FALSE)
  
  # Check consistent success (top 25 for 3+ years)
  recent_ranks <- tail(team_history$sim_rank, 3)
  consistent_success <- mean(recent_ranks <= 25, na.rm=TRUE) >= 0.67
  
  # Check attendance at capacity (92%+ for 2+ years)
  recent_att <- tail(team_history$attendance, 2)
  recent_cap <- tail(team_history$capacity, 2)
  at_capacity <- mean(recent_att / recent_cap, na.rm=TRUE) >= 0.92
  
  # Check expansion count (max 2 expansions per team)
  expansion_count <- sum(team_history$capacity > lag(team_history$capacity), na.rm=TRUE)
  under_limit <- expansion_count < 2
  
  # Random factor: 10% chance per year if all conditions met
  if(consistent_success && at_capacity && under_limit){
    return(runif(1) < 0.10)
  }
  FALSE
}

expand_stadium_capacity <- function(current_capacity){
  # Expand by 5-15% (realistic range)
  expansion_pct <- runif(1, 0.05, 0.15)
  new_capacity <- current_capacity * (1 + expansion_pct)
  # Cap at 110,000 (largest college stadiums)
  pmin(new_capacity, 110000)
}

# =========================
# PROMOTION/RELEGATION
# =========================

# ENHANCED: Fixed promotion/relegation system
apply_promotion_relegation <- function(state_df, tiers, playoff_teams){
  n_promote <- 3   # Top 3 teams per tier promoted
  n_relegate <- 3  # Bottom 3 teams per tier relegated
  
  df <- state_df %>% arrange(tier, sim_rank)
  new_tier <- state_df$tier
  
  # Promote top teams from each tier (except tier 1)
  for(t in 2:tiers){
    tier_teams <- which(df$tier == t)
    if(length(tier_teams) == 0) next
    promote_idx <- tier_teams[1:min(n_promote, length(tier_teams))]
    new_tier[promote_idx] <- t - 1
  }
  
  # Relegate bottom teams from each tier (except last tier)
  for(t in 1:(tiers-1)){
    tier_teams <- which(df$tier == t)
    if(length(tier_teams) == 0) next
    relegate_idx <- tail(tier_teams, min(n_relegate, length(tier_teams)))
    new_tier[relegate_idx] <- t + 1
  }
  
  df$tier <- new_tier
  df
}

# =========================
# STATE & MOMENTUM
# =========================

build_state0 <- function(anchor_2024, tiers){
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
      expansions = 0  # Track stadium expansions
    )
  
  n <- nrow(st); cuts <- seq(0,1,length.out=tiers+1)
  pct <- rank(st$rank, ties.method="first") / n
  tier_map <- tibble(team = st$team,
                     tier = pmax(1L, pmin(tiers, findInterval(pct, cuts, rightmost.closed=TRUE))))
  st %>% 
    left_join(tier_map, by="team") %>% 
    mutate(last_tier = tier) %>%
    select(-any_of("rank"))  # Remove rank to avoid conflicts with momentum updates
}

update_momentum <- function(state_df, mom_w, last_year_outcomes){
  # Ensure last_year_outcomes has all required columns
  required_cols <- c("team", "bowl_win", "ny6_app", "nat_app", "nat_win", "rank")
  for(col in required_cols) {
    if(!col %in% names(last_year_outcomes)) {
      if(col == "team") stop("last_year_outcomes must have 'team' column")
      last_year_outcomes[[col]] <- 0
    }
  }
  
  df <- state_df %>% left_join(last_year_outcomes, by="team")
  
  # Handle duplicate column names from join (e.g., rank.x, rank.y)
  if("rank.y" %in% names(df)) {
    df <- df %>% rename(rank = rank.y) %>% select(-any_of("rank.x"))
  }
  
  dC <- (mom_w$bowl_win * replace_na(df$bowl_win,0)) +
    (mom_w$ny6_app  * replace_na(df$ny6_app,0)) +
    (mom_w$nat_app  * replace_na(df$nat_app,0)) +
    (mom_w$nat_win  * replace_na(df$nat_win,0)) +
    (mom_w$top10    * as.integer(replace_na(df$rank,999) <= 10)) +
    (mom_w$bottom10 * as.integer(replace_na(df$rank,999) >= 100))
  
  C_new <- mom_w$decay * df$C + dC
  C_new <- pmin(pmax(C_new, -mom_w$cap), mom_w$cap)
  df$C <- C_new
  
  # Clean up joined columns
  df %>% select(-any_of(c("bowl_win","ny6_app","nat_app","nat_win","rank")))
}

derive_outcomes <- function(sim_year_df){
  tibble(
    team = sim_year_df$team,
    bowl_win = as.integer(sim_year_df$sim_rank <= 40),
    ny6_app  = as.integer(sim_year_df$sim_rank <= 12),
    nat_app  = as.integer(sim_year_df$sim_rank <= 4),
    nat_win  = as.integer(sim_year_df$sim_rank == 1),
    rank     = sim_year_df$sim_rank
  )
}

message("Part 3 loaded: Playoff bonuses, stadium expansion, and simulation setup complete")
