# ============================================================================
# COLLEGE FOOTBALL RELEGATION MODEL - ENHANCED VERSION
# PART 1 of 4: Setup, Utilities, and Data Ingestion
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(stringi)
  library(scales)
  library(DT)
  library(shiny)
})

# =========================
# CONFIG
# =========================
YEARS_HIST <- 2015:2024
YEARS_SIM  <- 2025:2034
DISCOUNT_RATE <- 0.07
RUN_SHINY <- TRUE
REVENUE_CONSERVATION <- FALSE

# IMPORTANT: Update these paths to your file locations
ncaa_path   <- "C:\\Users\\lynch\\Downloads\\collegerelegation\\College Football Relegation Teams.xlsx"
europe_path <- "C:\\Users\\lynch\\Downloads\\collegerelegation\\European Soccer Teams.xlsx"

# =========================
# UTILITIES
# =========================
`%||%` <- function(a, b) if (!is.null(a)) a else b

norm_name <- function(x){
  x %>% stringi::stri_replace_all_regex("\\u00A0"," ") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[_/]+"," / ") %>% str_replace_all("\\s+"," ") %>% str_trim()
}

parse_num <- function(x){
  x <- as.character(x); x <- gsub("\\$|,","",x); x <- gsub("^\\((.*)\\)$","-\\1",x)
  suppressWarnings(as.numeric(trimws(x)))
}

mode_chr <- function(v){
  vv <- v[!is.na(v) & v!=""]; if(!length(vv)) return(NA_character_)
  tb <- table(vv); names(tb)[which.max(tb)]
}

clean_conf <- function(x){ 
  y <- as.character(x); y <- str_trim(y); y[grepl("^\\d+$",y)] <- NA_character_; y 
}

ensure_cols <- function(df, cols){ 
  for(nm in cols) if(!nm %in% names(df)) df[[nm]] <- NA_real_; df 
}

lab_num <- function(){
  if ("cut_short_scale" %in% getNamespaceExports("scales")) {
    scales::label_number(accuracy=0.1, scale_cut = scales::cut_short_scale())
  } else {
    scales::label_number(accuracy=0.1)
  }
}

soft_ticket_saturation <- function(att_now, cap, mult, k=0.65){
  att_now <- ifelse(is.finite(att_now), att_now, 0)
  cap     <- ifelse(is.finite(cap) & cap>0, cap, pmax(att_now,1))
  mult    <- ifelse(is.finite(mult), mult, 1)
  load <- pmin((att_now * pmax(mult,0))/cap, 1.5)
  out <- (1 - exp(-k*load)) / (1 - exp(-k))
  out[!is.finite(out)] <- 1
  out
}

build_macro_series <- function(years, rho=0.55, sd=0.015, seed=1234L){
  set.seed(seed); z <- numeric(length(years)); e <- rnorm(length(years),0,sd)
  for(t in seq_along(years)) z[t] <- if(t==1) e[t] else rho*z[t-1] + e[t]
  tibble(year=years, macro=z)
}

# =========================
# DATA INGESTION FUNCTIONS
# =========================

read_first_sheet <- function(path, na = c("N/A","n/a","NA","na","-","","NULL")){
  sh <- readxl::excel_sheets(path)
  readxl::read_excel(path, sheet = sh[[1]], na = na)
}

# ENHANCED: Keep more teams - only exclude clearly lower-tier divisions
normalize_tier <- function(country, division_raw){
  n <- length(division_raw); out <- rep(NA_integer_, n)
  for(i in seq_len(n)){
    div <- tolower(stringi::stri_trans_general(as.character(division_raw[i]) %||% "", "Latin-ASCII"))
    if(div=="" || is.na(div)){ out[i] <- NA_integer_; next }
    # Only drop leagues clearly below tier 3
    if(grepl("below\\s*tier\\s*3|tier\\s*4|tier\\s*5|national\\s*2|national\\s*3|segunda\\s*federacion|tercera", div, perl=TRUE)){ 
      out[i] <- NA_integer_; next 
    }
    if(grepl("\\b3\\.?\\s*liga\\b", div, perl=TRUE)){ out[i] <- 3L; next }
    if(grepl("\\b2\\.?\\s*bundesliga\\b", div, perl=TRUE)){ out[i] <- 2L; next }
    if(grepl("\\bbundesliga\\b", div, perl=TRUE)){ out[i] <- 1L; next }
    if(grepl("\\bsegunda\\s*division\\s*b\\b|\\bsegunda\\s*b\\b|\\bprimera\\s*(federacion|rfef)\\b", div, perl=TRUE)){ out[i] <- 3L; next }
    if(grepl("\\bsegunda\\s*division\\b", div, perl=TRUE)){ out[i] <- 2L; next }
    if(grepl("\\bla\\s*liga\\b", div, perl=TRUE)){ out[i] <- 1L; next }
    if(grepl("\\bleague\\s*one\\b", div, perl=TRUE)){ out[i] <- 3L; next }
    if(grepl("\\bchampionship\\b", div, perl=TRUE)){ out[i] <- 2L; next }
    if(grepl("\\bpremier\\s*league\\b", div, perl=TRUE)){ out[i] <- 1L; next }
    if(grepl("\\bchampionnat\\s*national\\b", div, perl=TRUE)){ out[i] <- 3L; next }
    if(grepl("\\bligue\\s*2\\b", div, perl=TRUE)){ out[i] <- 2L; next }
    if(grepl("\\bligue\\s*1\\b", div, perl=TRUE)){ out[i] <- 1L; next }
    if(grepl("\\bserie\\s*c\\b", div, perl=TRUE)){ out[i] <- 3L; next }
    if(grepl("\\bserie\\s*b\\b", div, perl=TRUE)){ out[i] <- 2L; next }
    if(grepl("\\bserie\\s*a\\b", div, perl=TRUE)){ out[i] <- 1L; next }
    # Keep teams we can't classify - assign to tier 3 by default
    out[i] <- 3L
  }
  out
}

ncaa_to_long <- function(ncaa_wide){
  stopifnot("FBS Team" %in% names(ncaa_wide))
  names(ncaa_wide) <- norm_name(names(ncaa_wide))
  df_raw <- ncaa_wide %>%
    pivot_longer(-`FBS Team`, names_to="raw", values_to="value",
                 values_transform=list(value=as.character),
                 values_ptypes=list(value=character()))
  
  # UPDATED: Handle integer year columns (e.g., "2015 rank" or "2015rank")
  year_any_rx <- "\\b(201[5-9]|202[0-4])\\b"
  df_raw <- df_raw %>%
    mutate(raw_norm = str_squish(tolower(raw)),
           raw_norm = str_replace_all(raw_norm, "\\bfake\\b",""),
           year = suppressWarnings(as.integer(str_extract(raw_norm, year_any_rx))),
           key  = str_squish(str_trim(str_replace_all(raw_norm, year_any_rx, ""))))
  
  classify_key <- function(s){
    case_when(
      str_detect(s,"\\brank\\b") ~ "rank",
      str_detect(s,"total\\s*revenue") ~ "revenue",
      str_detect(s,"total\\s*expense|expenses?\\b") ~ "expenses",
      str_detect(s,"media\\s*rights|conf.*ncaa.*distrib") ~ "media_rights",
      str_detect(s,"ticket\\s*sales\\b") ~ "ticket_sales",
      str_detect(s,"avg\\s*ticket\\s*price") ~ "avg_ticket_price",
      str_detect(s,"avg\\s*attendance") ~ "attendance",
      str_detect(s,"stadium\\s*capacity") ~ "capacity",
      str_detect(s,"profit\\s*/?\\s*loss|\\bprofit\\b") ~ "profit",
      str_detect(s,"(total\\s*)?football\\s*salar") ~ "football_wages",
      str_detect(s,"nil\\s*spending") ~ "nil_spend",
      str_detect(s,"contributions|donations") ~ "donations",
      str_detect(s,"student\\s*fees?") ~ "student_fees",
      str_detect(s,"\\bconference\\b") ~ "conference",
      str_detect(s,"\\boppg\\b") ~ "oppg",
      str_detect(s,"\\bdppg\\b") ~ "dppg",
      TRUE ~ NA_character_
    )
  }
  
  keep <- df_raw %>% mutate(var = classify_key(key)) %>% filter(!is.na(year), !is.na(var))
  
  agg_for_var <- function(v, x_chr){
    xn <- suppressWarnings(as.numeric(x_chr))
    if(v=="conference") return(NA_real_)
    if(all(is.na(xn))) return(NA_real_)
    if(v %in% c("rank")) return(min(xn, na.rm=TRUE))
    if(v %in% c("avg_ticket_price","oppg","dppg")) return(mean(xn, na.rm=TRUE))
    return(max(xn, na.rm=TRUE))
  }
  
  collapsed <- keep %>% filter(var!="conference") %>%
    group_by(team=`FBS Team`, year, var) %>% 
    summarise(value=agg_for_var(first(var), value), .groups="drop")
  
  df_main <- collapsed %>% pivot_wider(names_from=var, values_from=value) %>%
    mutate(across(
      any_of(c("revenue","expenses","media_rights","ticket_sales","avg_ticket_price",
               "attendance","capacity","profit","rank","football_wages","nil_spend",
               "donations","student_fees","oppg","dppg")),
      ~ suppressWarnings(as.numeric(.))
    ))
  
  conf_long <- keep %>% filter(var=="conference") %>% 
    transmute(team=`FBS Team`, year, conference=value)
  
  df_main %>% left_join(conf_long, by=c("team","year")) %>%
    mutate(conference=clean_conf(conference),
           is_fcs=ifelse(!is.na(conference) & str_detect(tolower(conference),"fcs"), TRUE, FALSE)) %>%
    filter(is_fcs==FALSE | is.na(is_fcs)) %>% select(-is_fcs) %>% arrange(team, year)
}

# FIXED: Bowl flags extractor with proper vectorized operators
build_bowl_flags_long <- function(ncaa_wide){
  norm <- function(x){
    x %>% stringi::stri_trans_general("Latin-ASCII") %>% tolower() %>%
      gsub("\\s+"," ",., perl=TRUE) %>% trimws()
  }
  names(ncaa_wide) <- norm(names(ncaa_wide))
  stopifnot("fbs team" %in% names(ncaa_wide))
  
  wide_chr <- ncaa_wide %>%
    dplyr::mutate(dplyr::across(-`fbs team`, ~ as.character(.)))
  
  # UPDATED: Match year at word boundary (handles integer columns)
  year_rx_end <- "\\b(201[5-9]|202[0-4])$"
  
  is_bowl_win <- function(k) grepl("^bowl win\\s+\\d{4}$", k, perl=TRUE)
  is_nat_app  <- function(k) grepl("^nat champ appearance\\s+\\d{4}$", k, perl=TRUE)
  is_nat_win  <- function(k) grepl("^nat champ win\\s+\\d{4}$", k, perl=TRUE)
  is_cfp_app  <- function(k) grepl("^cfp .* appearance\\s+\\d{4}$", k, perl=TRUE)
  
  long <- wide_chr %>%
    tidyr::pivot_longer(
      cols = -`fbs team`,
      names_to = "raw",
      values_to = "value",
      values_transform = list(value = as.character),
      values_ptypes = list(value = character())
    ) %>%
    dplyr::mutate(
      raw  = norm(raw),
      year = suppressWarnings(as.integer(stringr::str_extract(raw, year_rx_end))),
      key  = trimws(gsub(year_rx_end, "", raw))
    ) %>%
    dplyr::filter(!is.na(year))
  
  flags <- long %>%
    dplyr::mutate(
      var = dplyr::case_when(
        is_bowl_win(raw) ~ "bowl_win",
        is_nat_app(raw)  ~ "nat_app",
        is_nat_win(raw)  ~ "nat_win",
        is_cfp_app(raw)  ~ "ny6_app",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(var)) %>%
    # FIXED: Use vectorized & operators (not &&)
    dplyr::mutate(flag = as.integer(!is.na(value) & value != "" & value != "0")) %>%
    dplyr::transmute(team = `fbs team`, year, var, flag) %>%
    tidyr::pivot_wider(names_from=var, values_from=flag, values_fill = list(flag = 0))
  
  for(nm in c("bowl_win","nat_app","nat_win","ny6_app")){
    if(!(nm %in% names(flags))) flags[[nm]] <- 0L
  }
  flags
}

read_europe_workbook <- function(path, na = c("N/A","n/a","NA","na","-","","NULL")){
  sheets <- readxl::excel_sheets(path)
  dfs <- vector("list", length(sheets))
  for(i in seq_along(sheets)){
    sh <- sheets[i]
    df <- readxl::read_excel(path, sheet = sh, na = na)
    nm <- names(df)
    drop_idx <- which(tolower(trimws(nm)) %in% c("fake"))
    if(length(drop_idx)) df <- df[,-drop_idx, drop=FALSE]
    if(!("Country" %in% names(df))) df$Country <- sh
    if(!("Club Name" %in% names(df))) stop(paste0("Sheet '", sh, "' lacks 'Club Name' column."))
    df$.__sheet <- sh
    dfs[[i]] <- df
  }
  bind_rows(dfs)
}

europe_to_long <- function(eu_wide){
  base_cols <- c("Country","Club Name")
  eu_wide <- eu_wide %>% select(any_of(base_cols), everything())
  names(eu_wide) <- norm_name(names(eu_wide))
  
  eu_raw <- eu_wide %>%
    pivot_longer(cols = setdiff(names(eu_wide), c("Country","Club Name")),
                 names_to = "raw", values_to = "value",
                 values_transform = list(value = as.character),
                 values_ptypes = list(value = character()))
  
  # UPDATED: Handle integer year columns with word boundaries
  year_any_rx <- "\\b(201[5-9]|202[0-4])\\b"
  eu_raw <- eu_raw %>%
    mutate(raw_norm = str_squish(tolower(raw)),
           raw_norm = str_replace_all(raw_norm, "\\bfake\\b",""),
           year = suppressWarnings(as.integer(str_extract(raw_norm, year_any_rx))),
           key  = str_squish(str_trim(str_replace_all(raw_norm, year_any_rx, ""))))
  
  classify_key <- function(s){
    case_when(
      str_detect(s,"\\bdivision\\b") ~ "division",
      str_detect(s,"\\brank\\b") ~ "rank",
      str_detect(s,"\\bpts\\b|\\bpoints\\b") ~ "pts",
      str_detect(s,"total\\s*revenue") ~ "revenue",
      str_detect(s,"\\btv\\b.*revenue|broadcast") ~ "tv_rev",
      str_detect(s,"ticket\\s*revenue") ~ "ticket_rev",
      str_detect(s,"sponsorship\\s*revenue|sponsor") ~ "sponsor_rev",
      str_detect(s,"total\\s*wages|wage\\s*bill") ~ "wages",
      str_detect(s,"wage\\s*%|%\\s*wage") ~ "wage_share",
      str_detect(s,"avg\\s*ticket") ~ "avg_ticket_price",
      str_detect(s,"avg\\s*attendance|\\battendance\\b") ~ "attendance",
      str_detect(s,"\\bcapacity\\b") ~ "capacity",
      TRUE ~ NA_character_
    )
  }
  
  keep <- eu_raw %>%
    filter(!is.na(year)) %>%
    mutate(var = classify_key(key)) %>%
    filter(!is.na(var))
  
  collapsed <- keep %>%
    group_by(country = .data[["Country"]],
             club    = .data[["Club Name"]],
             year, var) %>%
    summarise(value = {
      xn <- suppressWarnings(as.numeric(value))
      if(any(is.finite(xn))) as.character(max(xn, na.rm=TRUE)) else mode_chr(value)
    }, .groups="drop")
  
  eu <- collapsed %>%
    pivot_wider(names_from=var, values_from=value) %>%
    mutate(across(any_of(c("rank","pts","revenue","tv_rev","ticket_rev","sponsor_rev",
                           "wages","wage_share","avg_ticket_price","attendance","capacity")),
                  ~ suppressWarnings(as.numeric(.))),
           division_str = as.character(division),
           tier = normalize_tier(country, division_str)) %>%
    filter(!is.na(tier)) %>% arrange(country, club, year)
  
  eu
}

extract_2024_from_wide <- function(ncaa_wide){
  names(ncaa_wide) <- norm_name(names(ncaa_wide))
  stopifnot("FBS Team" %in% names(ncaa_wide))
  
  # UPDATED: Simpler matching for "2024 field" or "field 2024" patterns
  pick_any <- function(field_rx){
    # Try "2024 field" pattern - ALLOW text after field name
    yr_first  <- paste0("^2024\\s+", field_rx)  # Removed the $ anchor
    # Try "field 2024" pattern - ALLOW text before and after
    fld_first <- paste0(field_rx, ".*2024")
    m <- grep(paste0("(", yr_first, ")|(", fld_first, ")"), names(ncaa_wide),
              perl=TRUE, ignore.case=TRUE, value=TRUE)
    if(length(m)) m[1] else NA_character_
  }
  
  c_rank   <- pick_any("rank")
  c_rev    <- pick_any("total\\s+revenue")
  c_exp    <- pick_any("total\\s+expense(?:s)?")
  c_media  <- pick_any("(media\\s+rights|conf.*ncaa\\s+distributions?)")
  c_tix    <- pick_any("ticket\\s+sales")
  c_att    <- pick_any("avg\\s+attendance")
  c_cap    <- pick_any("stadium\\s+capacity")
  c_price  <- pick_any("avg\\s+ticket\\s+price")
  c_profit <- pick_any("(profit|loss)")
  c_conf   <- pick_any("conference")
  c_don    <- pick_any("(contributions?|donations?)")
  c_fees   <- pick_any("student\\s+fees?")
  c_wages  <- pick_any("(total\\s+)?football\\s+salaries?")
  c_nil    <- pick_any("nil\\s+spending")
  
  out <- ncaa_wide %>%
    transmute(
      team           = str_squish(`FBS Team`),
      conference     = if(!is.na(c_conf)) clean_conf(.data[[c_conf]]) else NA_character_,
      rank           = if(!is.na(c_rank))   suppressWarnings(as.numeric(.data[[c_rank]])) else NA_real_,
      revenue        = if(!is.na(c_rev))    parse_num(.data[[c_rev]])    else NA_real_,
      expenses       = if(!is.na(c_exp))    parse_num(.data[[c_exp]])    else NA_real_,
      media_rights   = if(!is.na(c_media))  parse_num(.data[[c_media]])  else NA_real_,
      ticket_sales   = if(!is.na(c_tix))    parse_num(.data[[c_tix]])    else NA_real_,
      attendance     = if(!is.na(c_att))    parse_num(.data[[c_att]])    else NA_real_,
      capacity       = if(!is.na(c_cap))    parse_num(.data[[c_cap]])    else NA_real_,
      avg_ticket_price = if(!is.na(c_price)) parse_num(.data[[c_price]]) else NA_real_,
      profit         = if(!is.na(c_profit)) parse_num(.data[[c_profit]]) else NA_real_,
      donations      = if(!is.na(c_don))    parse_num(.data[[c_don]])    else NA_real_,
      student_fees   = if(!is.na(c_fees))   parse_num(.data[[c_fees]])   else NA_real_,
      football_wages = if(!is.na(c_wages))  parse_num(.data[[c_wages]])  else NA_real_,
      nil_spending   = if(!is.na(c_nil))    parse_num(.data[[c_nil]])    else NA_real_
    ) %>%
    mutate(
      across(c(revenue, expenses, media_rights, ticket_sales, attendance, capacity,
               avg_ticket_price, profit, donations, student_fees, football_wages, nil_spending), 
             ~ as.numeric(.)),
      other = pmax(coalesce(revenue,0) - coalesce(media_rights,0) - coalesce(ticket_sales,0) -
                     coalesce(donations,0) - coalesce(student_fees,0), 0)
    )
  
  if (!("capacity" %in% names(out)) || all(!is.finite(out$capacity))) {
    out$capacity <- pmax(out$attendance, 1, na.rm = TRUE)
    out$capacity[!is.finite(out$capacity)] <- 50000
  }
  out
}

ingest_all <- function(ncaa_path, europe_path, YEARS_HIST){
  ncaa_wide <- read_first_sheet(ncaa_path)
  euro_all  <- read_europe_workbook(europe_path)
  ncaa_long <- ncaa_to_long(ncaa_wide)
  euro_long <- europe_to_long(euro_all)
  anchor_2024 <- extract_2024_from_wide(ncaa_wide)
  list(ncaa_wide=ncaa_wide, ncaa_long=ncaa_long, euro_long=euro_long, anchor_2024=anchor_2024)
}

message("Part 1 loaded: Setup, utilities, and data ingestion complete")
