# ============================================================================
# COLLEGE FOOTBALL RELEGATION MODEL - ENHANCED VERSION
# PART 2 of 4: Estimation Functions
# ============================================================================
# Run Part 1 before this file

# =========================
# ESTIMATION FUNCTIONS
# =========================

estimate_macro <- function(ncaa_long, YEARS_HIST){
  nat <- ncaa_long %>%
    filter(year %in% YEARS_HIST, year != 2020) %>%
    group_by(year) %>% summarise(rev=sum(revenue,na.rm=TRUE), .groups="drop") %>%
    arrange(year) %>% mutate(g = log(pmax(rev,1)) - dplyr::lag(log(pmax(rev,1)))) %>% 
    filter(is.finite(g))
  if(nrow(nat) < 3) return(list(rho=0.55, sd=0.015))
  g <- nat$g[-1]; g_lag <- nat$g[-length(nat$g)]
  fit <- lm(g ~ g_lag)
  rho <- max(min(unname(coef(fit)[2]), 0.95), 0.0)
  sdev <- sd(residuals(fit), na.rm=TRUE); sdev <- ifelse(is.finite(sdev), sdev, 0.015)
  list(rho=rho, sd=sdev)
}

learn_team_vol <- function(ncaa_long, YEARS_HIST){
  ncaa_long %>%
    filter(year %in% YEARS_HIST, year != 2020) %>%
    arrange(team, year) %>%
    group_by(team) %>%
    summarise(vol = {
      rv <- as.numeric(revenue)
      ok <- which(is.finite(rv)&rv>0)
      if(length(ok) < 3) NA_real_ else sd(diff(log(rv[ok])), na.rm=TRUE)
    }, .groups="drop") %>%
    mutate(vol = ifelse(!is.finite(vol)|vol<=0, 0.02, pmax(vol, 0.02)))
}

learn_conf_growth <- function(ncaa_long, YEARS_HIST){
  ncaa_long %>%
    filter(year %in% YEARS_HIST, year != 2020, !is.na(conference)) %>%
    arrange(team, year) %>% group_by(team, conference) %>%
    summarise(g = {
      rv <- revenue; yr <- year; ok <- which(is.finite(rv)&rv>0)
      if(length(ok)>=2){ 
        t<-max(yr[ok])-min(yr[ok])
        if(t>0) (rv[ok][which.max(yr[ok])]/rv[ok][which.min(yr[ok])])^(1/t)-1 else NA_real_ 
      } else NA_real_
    }, .groups="drop_last") %>% ungroup() %>%
    group_by(conference) %>% summarise(conf_g = median(g, na.rm=TRUE), .groups="drop") %>%
    mutate(conf_g = ifelse(is.finite(conf_g), conf_g, 0))
}

late_stakes_ncaaproxy <- function(rank, cutoff=12, window=8){
  ifelse(!is.finite(rank), 0, ifelse(rank<=cutoff, 1, pmax(0, 1-(rank-cutoff)/window)))
}

estimate_attn_price <- function(ncaa_long, YEARS_HIST){
  df <- ncaa_long %>%
    filter(year %in% YEARS_HIST, year != 2020) %>%
    transmute(
      team, year,
      att   = suppressWarnings(as.numeric(attendance)),
      price = suppressWarnings(as.numeric(avg_ticket_price)),
      cap   = suppressWarnings(as.numeric(capacity)),
      rk    = suppressWarnings(as.numeric(rank)),
      ticket_sales = suppressWarnings(as.numeric(ticket_sales))
    ) %>% mutate(late = late_stakes_ncaaproxy(rk))
  
  df <- df %>% filter(is.finite(att) & att>0, is.finite(price) & price>0)
  if (nrow(df) == 0) return(list(ALPHA_ATT = 1.05, ALPHA_PRICE = 1.03))
  
  uplift_att <- df %>%
    group_by(team) %>%
    summarise(m1 = suppressWarnings(median(att[late > 0.5], na.rm=TRUE)),
              m0 = suppressWarnings(median(att[late <= 0.1], na.rm=TRUE)), .groups="drop") %>%
    transmute(mult = ifelse(is.finite(m1)&is.finite(m0)&m0>0, m1/m0, NA_real_)) %>%
    summarise(alpha_att = median(mult, na.rm=TRUE)) %>% pull(alpha_att)
  
  df <- df %>%
    mutate(yield = dplyr::case_when(
      is.finite(ticket_sales) & ticket_sales>0 & is.finite(att) & att>0 ~ ticket_sales/att,
      TRUE ~ price))
  
  uplift_price <- df %>%
    group_by(team) %>%
    summarise(m1 = suppressWarnings(median(yield[late > 0.5], na.rm=TRUE)),
              m0 = suppressWarnings(median(yield[late <= 0.1], na.rm=TRUE)), .groups="drop") %>%
    transmute(mult = ifelse(is.finite(m1)&is.finite(m0)&m0>0, m1/m0, NA_real_)) %>%
    summarise(alpha_price = median(mult, na.rm=TRUE)) %>% pull(alpha_price)
  
  list(
    ALPHA_ATT   = ifelse(is.finite(uplift_att),   pmin(pmax(uplift_att,   1.02), 1.15), 1.05),
    ALPHA_PRICE = ifelse(is.finite(uplift_price), pmin(pmax(uplift_price, 0.98), 1.10), 1.03)
  )
}

# ENHANCED: Donor fatigue model with calculated saturation
estimate_donations <- function(ncaa_long, YEARS_HIST){
  df <- ncaa_long %>%
    filter(year %in% YEARS_HIST, year != 2020) %>% arrange(team, year) %>%
    group_by(team) %>%
    mutate(don = as.numeric(donations),
           don_lag = dplyr::lag(don),
           rk = as.numeric(rank),
           top25_last = as.integer(dplyr::lag(rk) <= 25),
           top12_last = as.integer(dplyr::lag(rk) <= 12),
           late_last  = late_stakes_ncaaproxy(dplyr::lag(rk))) %>%
    ungroup() %>%
    mutate(gr_don = log(pmax(don,1)) - log(pmax(don_lag,1))) %>%
    filter(is.finite(gr_don))
  
  if (nrow(df) < 200) {
    return(list(base=0.02, stakes=0.015, macro=0.10, saturation_level=500, fatigue_rate=0.05))
  }
  
  fit <- lm(gr_don ~ top25_last + top12_last + late_last + factor(team) + factor(year), data=df)
  co <- coef(fit); base <- median(df$gr_don, na.rm=TRUE)
  stakes <- sum(co[names(co) %in% c("top25_last","top12_last","late_last")], na.rm=TRUE)
  
  # ENHANCED: Calculate saturation level from data (95th percentile), excluding COVID year
  saturation_level <- quantile(ncaa_long$donations[ncaa_long$year %in% YEARS_HIST & ncaa_long$year != 2020], 
                               0.95, na.rm=TRUE)
  
  list(
    base = pmin(pmax(base, 0.00), 0.05), 
    stakes = pmin(pmax(stakes,0.005), 0.03), 
    macro = 0.10,
    saturation_level = saturation_level,
    fatigue_rate = 0.05  # 5% reduction per $100M above median
  )
}

estimate_expense_leverage <- function(ncaa_long, YEARS_HIST){
  df <- ncaa_long %>% filter(year %in% YEARS_HIST, year != 2020, revenue>0, expenses>0) %>%
    arrange(team, year) %>% group_by(team) %>%
    mutate(gr_rev = log(revenue) - dplyr::lag(log(revenue)),
           gr_exp = log(expenses) - dplyr::lag(log(expenses))) %>%
    ungroup() %>% filter(is.finite(gr_rev) & is.finite(gr_exp))
  
  if (nrow(df) < 300) return(list(lambda=0.90, er_meanrev=0.20, er_sd=0.01))
  fit <- lm(gr_exp ~ gr_rev + factor(team) + factor(year), data=df)
  lambda <- as.numeric(coef(fit)["gr_rev"])
  list(lambda = pmin(pmax(lambda,0.70),1.05), er_meanrev=0.20, er_sd=0.01)
}

estimate_europe_tier_weights <- function(euro_long){
  tm_rev <- euro_long %>% group_by(country, year, tier) %>% 
    summarise(val=median(revenue,na.rm=TRUE), .groups="drop") %>%
    group_by(country, year) %>% mutate(m = val/val[tier==1]) %>% ungroup() %>%
    group_by(country, tier) %>% summarise(m_rev = median(m, na.rm=TRUE), .groups="drop")
  
  tm_att <- euro_long %>% group_by(country, year, tier) %>% 
    summarise(val=median(attendance,na.rm=TRUE), .groups="drop") %>%
    group_by(country, year) %>% mutate(m = val/val[tier==1]) %>% ungroup() %>%
    group_by(country, tier) %>% summarise(m_att = median(m, na.rm=TRUE), .groups="drop")
  
  base <- full_join(tm_rev, tm_att, by=c("country","tier")) %>% 
    filter(is.finite(m_rev)|is.finite(m_att))
  
  med <- base %>% group_by(tier) %>% 
    summarise(m_rev=median(m_rev,na.rm=TRUE), m_att=median(m_att,na.rm=TRUE), .groups="drop")
  
  tiers <- 1:5
  mrev <- approx(med$tier, med$m_rev, xout=tiers, rule=2)$y
  matt <- approx(med$tier, med$m_att, xout=tiers, rule=2)$y
  
  if(max(med$tier,na.rm=TRUE) < 5){
    for(t in (max(med$tier,na.rm=TRUE)+1):5){ 
      mrev[t] <- mrev[t-1]*0.90
      matt[t] <- matt[t-1]*0.90 
    }
  }
  
  # ENHANCED: Increased dampening to reduce tier transition spikes
  s_rev <- 0.65; s_att <- 0.65  # Was 0.5, now 0.65 (less extreme tier differences)
  tibble(tier=tiers,
         m_rev = 1 - s_rev*(1 - mrev),
         m_att = 1 - s_att*(1 - matt))
}

# ENHANCED: Momentum with data-driven cap
estimate_momentum_weights <- function(ncaa_long, ncaa_wide){
  df <- ncaa_long %>%
    dplyr::filter(year %in% YEARS_HIST, year != 2020) %>%
    dplyr::transmute(team, year,
                     rk  = as.numeric(rank),
                     att = as.numeric(attendance),
                     don = as.numeric(donations)) %>%
    dplyr::arrange(team, year) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(att_lag = dplyr::lag(att),
                  don_lag = dplyr::lag(don),
                  g_att = log(pmax(att,1)) - log(pmax(att_lag,1)),
                  g_don = log(pmax(don,1)) - log(pmax(don_lag,1))) %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::mutate(
      top10    = as.integer(is.finite(rk) & rk <= 10),
      top12    = as.integer(is.finite(rk) & rk <= 12),
      top4     = as.integer(is.finite(rk) & rk <= 4),
      champ    = as.integer(is.finite(rk) & rk == 1),
      bottom10 = as.integer(is.finite(rk) & rk >= 100)
    )
  
  flags <- build_bowl_flags_long(ncaa_wide)
  df <- dplyr::left_join(df, flags, by=c("team","year"))
  
  if(!"bowl_win" %in% names(df)) df$bowl_win <- 0L
  if(!"nat_app"  %in% names(df)) df$nat_app  <- as.integer(df$top4==1)
  if(!"nat_win"  %in% names(df)) df$nat_win  <- as.integer(df$champ==1)
  df$ny6_app <- dplyr::coalesce(df$ny6_app, as.integer(df$top12==1))
  
  delta_med <- function(flag, y){
    m1 <- suppressWarnings(median(y[flag==1], na.rm=TRUE))
    m0 <- suppressWarnings(median(y[flag==0], na.rm=TRUE))
    if(is.finite(m1) && is.finite(m0)) m1 - m0 else NA_real_
  }
  
  d_bw   <- delta_med(df$bowl_win, df$g_att)
  d_ny6  <- delta_med(df$ny6_app,  df$g_att)
  d_app  <- delta_med(df$nat_app,  df$g_att)
  d_win  <- delta_med(df$nat_win,  df$g_att)
  d_t10  <- delta_med(df$top10,    df$g_att)
  d_bot  <- delta_med(df$bottom10, df$g_att)
  
  shrink <- function(z, s=0.25){ ifelse(is.finite(z), s*z, 0) }
  
  # ENHANCED: Data-driven momentum cap based on performance differentials
  max_swing <- abs(d_t10) + abs(d_bot)
  momentum_cap <- pmin(pmax(max_swing * 2, 0.08), 0.15)  # Cap between 8% and 15%
  
  list(
    bowl_win = pmin(pmax(shrink(d_bw),  0.001), 0.010),
    ny6_app  = pmin(pmax(shrink(d_ny6), 0.002), 0.015),
    nat_app  = pmin(pmax(shrink(d_app), 0.003), 0.020),
    nat_win  = pmin(pmax(shrink(d_win), 0.005), 0.030),
    top10    = pmin(pmax(shrink(d_t10), 0.002), 0.010),
    bottom10 = -pmin(pmax(abs(shrink(d_bot)), 0.002), 0.015),
    decay    = 0.70,
    cap      = momentum_cap
  )
}

estimate_discount_range <- function(ncaa_long){
  nat <- ncaa_long %>%
    filter(year %in% YEARS_HIST, year != 2020) %>%
    group_by(year) %>% summarise(rev=sum(revenue,na.rm=TRUE), .groups="drop") %>%
    arrange(year) %>% mutate(g = log(pmax(rev,1)) - dplyr::lag(log(pmax(rev,1)))) %>% 
    filter(is.finite(g))
  if(nrow(nat) < 4) return(c(0.055, 0.085))
  vol <- sd(nat$g, na.rm=TRUE)
  ce_rate_low  <- 0.06 + 0.5*vol
  ce_rate_high <- 0.08 + 1.0*vol
  c(max(0.03, ce_rate_low), min(0.12, ce_rate_high))
}

# ENHANCED: Calculate team-specific revenue floors
calculate_revenue_floors <- function(anchor_2024){
  anchor_2024 %>%
    transmute(
      team = team,
      revenue_floor = pmax(revenue * 0.40, 10)  # Floor at 40% of starting, min $10M
    )
}

estimate_all <- function(ncaa_long, euro_long, anchor_2024, YEARS_HIST, YEARS_SIM, ncaa_wide){
  macro <- estimate_macro(ncaa_long, YEARS_HIST)
  team_vol <- learn_team_vol(ncaa_long, YEARS_HIST)
  conf_g   <- learn_conf_growth(ncaa_long, YEARS_HIST)
  ap       <- estimate_attn_price(ncaa_long, YEARS_HIST)
  don      <- estimate_donations(ncaa_long, YEARS_HIST)
  exp_lev  <- estimate_expense_leverage(ncaa_long, YEARS_HIST)
  eu_w     <- estimate_europe_tier_weights(euro_long)
  mom_w    <- estimate_momentum_weights(ncaa_long, ncaa_wide)
  dsc_rng  <- estimate_discount_range(ncaa_long)
  rev_floor <- calculate_revenue_floors(anchor_2024)
  
  list(
    MACRO = macro,
    TEAM_VOL = team_vol,
    CONF_G = conf_g,
    ALPHA_ATT = ap$ALPHA_ATT,
    ALPHA_PRICE = ap$ALPHA_PRICE,
    DON = don,
    EXP = exp_lev,
    EU_WEIGHTS = eu_w,
    MOM = mom_w,
    DISC_RANGE = dsc_rng,
    TICKET_SAT_K = 0.65,
    BASE_CLAMP = c(0.00, 0.05),  # Reduced: 0% minimum, 5% maximum (was 8%)
    REV_FLOOR = rev_floor
  )
}

message("Part 2 loaded: Estimation functions complete")
