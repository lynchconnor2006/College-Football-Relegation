# ============================================================================
# LINEAR REGRESSION MODEL - CFB Revenue Prediction
# Data-driven nested OLS specs + LASSO feature selection
# Best model selected automatically by holdout test RMSE
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
  library(scales)
  library(ggplot2)
  library(glmnet)
})

if (!requireNamespace("glmnet", quietly = TRUE)) {
  install.packages("glmnet")
  library(glmnet)
}

setwd("C:/Users/lynch/Downloads/collegerelegation")

message("\n============================================================")
message("  LINEAR REGRESSION - Feature Selection and Model Comparison")
message("============================================================\n")

# ============================================================
# STEP 1: LOAD DATA AND ENGINEER FEATURES
# ============================================================

message("Step 1: Loading data and engineering features...")
ncaa_long <- readRDS("ncaa_long.rds")
message(sprintf("  Unit check: mean team revenue = $%.1fM", mean(ncaa_long$revenue, na.rm = TRUE)))

model_data <- ncaa_long %>%
  filter(year != 2020) %>%
  arrange(team, year) %>%
  group_by(team) %>%
  mutate(
    revenue_lag1     = lag(revenue, 1),
    revenue_lag2     = lag(revenue, 2),
    revenue_growth1  = revenue / lag(revenue, 1) - 1,
    revenue_growth2  = lag(revenue, 1) / lag(revenue, 2) - 1,
    rank_pct         = rank / max(rank, na.rm = TRUE),
    rank_change      = lag(rank, 1) - rank,
    rank_3yr_avg     = (rank + lag(rank, 1) + lag(rank, 2)) / 3,
    tier_proxy       = case_when(rank <= 45 ~ 1L, rank <= 90 ~ 2L, TRUE ~ 3L),
    capacity_util    = attendance / pmax(capacity, 1),
    revenue_3yr_cagr = (revenue / lag(revenue, 3))^(1/3) - 1,
    is_power_conf    = as.integer(conference %in% c("SEC", "Big Ten", "Big 12", "ACC", "Pac-12")),
    year_trend       = year - 2015
  ) %>%
  ungroup() %>%
  filter(
    !is.na(revenue_lag2),
    !is.na(rank_3yr_avg),
    !is.infinite(revenue_growth1),
    !is.infinite(revenue_growth2),
    abs(revenue_growth1) < 1.0
  )

message(sprintf("  Dataset: %d obs, %d teams | $%.0fM - $%.0fM",
                nrow(model_data), n_distinct(model_data$team),
                min(model_data$revenue, na.rm = TRUE),
                max(model_data$revenue, na.rm = TRUE)))

# ============================================================
# STEP 2: TRAIN / TEST SPLIT
# ============================================================

message("\nStep 2: Train/test split...")
train_data <- model_data %>% filter(year <= 2022)
test_data  <- model_data %>% filter(year >= 2023)
message(sprintf("  Training: %d obs (2015-2022) | Testing: %d obs (2023-2024)",
                nrow(train_data), nrow(test_data)))

# ============================================================
# STEP 3: DATA-DRIVEN NESTED OLS SPECIFICATIONS
# Features ranked by absolute correlation with revenue on training data only.
# Nested specs add features in that ranked order - data decides, not us.
# ============================================================

message("\nStep 3: Ranking features by absolute correlation with revenue (train only)...")

candidate_features <- c(
  "revenue_lag1", "revenue_lag2", "revenue_growth1", "revenue_growth2",
  "rank_pct", "rank_change", "rank_3yr_avg", "tier_proxy",
  "capacity_util", "revenue_3yr_cagr", "is_power_conf", "year_trend"
)

feature_cors <- sapply(candidate_features, function(f) {
  vals <- train_data[[f]]
  if (all(is.na(vals))) return(0)
  abs(cor(vals, train_data$revenue, use = "complete.obs"))
})
feature_cors <- sort(feature_cors, decreasing = TRUE)

message("  Feature absolute correlation ranking (training data only):")
print(round(feature_cors, 4))

ranked_feats <- names(feature_cors)
f_m1 <- ranked_feats[seq_len(min(1,  length(ranked_feats)))]
f_m2 <- ranked_feats[seq_len(min(3,  length(ranked_feats)))]
f_m3 <- ranked_feats[seq_len(min(6,  length(ranked_feats)))]
f_m4 <- ranked_feats[seq_len(min(9,  length(ranked_feats)))]
f_m5 <- ranked_feats

# Helper: build lm formula, treating tier_proxy as factor
make_lm_formula <- function(feats) {
  terms <- ifelse(feats == "tier_proxy", "as.factor(tier_proxy)", feats)
  as.formula(paste("revenue ~", paste(terms, collapse = " + ")))
}

message(sprintf("\n  M1 (top 1):    %s", paste(f_m1, collapse = ", ")))
message(sprintf("  M2 (top 3):    %s", paste(f_m2, collapse = ", ")))
message(sprintf("  M3 (top 6):    %s", paste(f_m3, collapse = ", ")))
message(sprintf("  M4 (top 9):    %s", paste(f_m4, collapse = ", ")))
message(sprintf("  M5 (all %d): %s", length(f_m5), paste(f_m5, collapse = ", ")))

message("\n  Fitting 5 OLS specifications...")
lm_m1 <- lm(make_lm_formula(f_m1), data = train_data)
lm_m2 <- lm(make_lm_formula(f_m2), data = train_data)
lm_m3 <- lm(make_lm_formula(f_m3), data = train_data)
lm_m4 <- lm(make_lm_formula(f_m4), data = train_data)
lm_m5 <- lm(make_lm_formula(f_m5), data = train_data)
message("  Done.")

# ============================================================
# STEP 4: LASSO WITH 10-FOLD CV LAMBDA SELECTION
# Critical: filter to complete cases BEFORE model.matrix
# so x rows and y length are always identical.
# ============================================================

message("\nStep 4: LASSO with 10-fold CV lambda selection...")

lasso_cols <- c(
  "revenue_lag1", "revenue_lag2", "revenue_growth1", "revenue_growth2",
  "rank_pct", "rank_change", "rank_3yr_avg", "tier_proxy",
  "capacity_util", "revenue_3yr_cagr", "is_power_conf", "year_trend", "revenue"
)

# Keep only complete rows across all lasso columns
train_lasso <- train_data[
  complete.cases(train_data[, intersect(lasso_cols, names(train_data))]), ]
test_lasso <- test_data[
  complete.cases(test_data[, intersect(lasso_cols, names(test_data))]), ]

message(sprintf("  Complete-case train: %d rows (dropped %d NAs from %d)",
                nrow(train_lasso), nrow(train_data) - nrow(train_lasso), nrow(train_data)))
message(sprintf("  Complete-case test:  %d rows (dropped %d NAs from %d)",
                nrow(test_lasso), nrow(test_data) - nrow(test_lasso), nrow(test_data)))

mat_formula <- ~ revenue_lag1 + revenue_lag2 + revenue_growth1 + revenue_growth2 +
  rank_pct + rank_change + rank_3yr_avg + as.factor(tier_proxy) +
  capacity_util + revenue_3yr_cagr + is_power_conf + year_trend - 1

# Build from same filtered frame - guaranteed alignment
train_x <- model.matrix(mat_formula, data = train_lasso)
train_y <- train_lasso$revenue
test_x  <- model.matrix(mat_formula, data = test_lasso)
test_y  <- test_lasso$revenue

# Hard stop if misaligned
if (nrow(train_x) != length(train_y)) {
  stop(sprintf("LASSO alignment error: train_x=%d rows but train_y=%d elements",
               nrow(train_x), length(train_y)))
}
if (nrow(test_x) != length(test_y)) {
  stop(sprintf("LASSO alignment error: test_x=%d rows but test_y=%d elements",
               nrow(test_x), length(test_y)))
}
message(sprintf("  train_x: %d x %d | train_y: %d  [aligned OK]",
                nrow(train_x), ncol(train_x), length(train_y)))

set.seed(4242)
lasso_cv  <- cv.glmnet(train_x, train_y, alpha = 1, nfolds = 10)
lambda_min <- lasso_cv$lambda.min
lambda_1se <- lasso_cv$lambda.1se

lasso_min <- glmnet(train_x, train_y, alpha = 1, lambda = lambda_min)
lasso_1se <- glmnet(train_x, train_y, alpha = 1, lambda = lambda_1se)

# Extract retained features
coef_min_mat  <- coef(lasso_min)
coef_1se_mat  <- coef(lasso_1se)
features_min  <- rownames(coef_min_mat)[as.numeric(coef_min_mat) != 0]
features_min  <- features_min[features_min != "(Intercept)"]
features_1se  <- rownames(coef_1se_mat)[as.numeric(coef_1se_mat) != 0]
features_1se  <- features_1se[features_1se != "(Intercept)"]

message(sprintf("  lambda.min = %.4f: %d features kept", lambda_min, length(features_min)))
message(sprintf("  lambda.1se = %.4f: %d features kept", lambda_1se, length(features_1se)))
message(sprintf("  lambda.min features: %s", paste(features_min, collapse = ", ")))
message(sprintf("  lambda.1se features: %s", paste(features_1se, collapse = ", ")))

# ============================================================
# STEP 5: SELECT BEST MODEL BY HOLDOUT TEST RMSE
# ============================================================

message("\nStep 5: Selecting best model by holdout test RMSE...")

eval_model <- function(model, train_df, test_df, label, is_lasso = FALSE) {
  if (is_lasso) {
    tr_pred <- as.numeric(predict(model, newx = train_x))
    te_pred <- as.numeric(predict(model, newx = test_x))
    tr_y    <- train_y
    te_y    <- test_y
  } else {
    tr_pred <- predict(model, newdata = train_df)
    te_pred <- predict(model, newdata = test_df)
    tr_y    <- train_df$revenue
    te_y    <- test_df$revenue
  }
  # Drop NA pairs before computing cor() — any NA in predictions
  # (from rows with missing features) silently returns NA from cor()
  tr_complete <- !is.na(tr_pred) & !is.na(tr_y)
  te_complete <- !is.na(te_pred) & !is.na(te_y)

  tibble(
    Model      = label,
    Train_R2   = cor(tr_y[tr_complete], tr_pred[tr_complete])^2,
    Train_RMSE = sqrt(mean((tr_y - tr_pred)^2, na.rm = TRUE)),
    Train_MAPE = mean(abs((tr_y - tr_pred) / tr_y), na.rm = TRUE) * 100,
    Test_R2    = cor(te_y[te_complete], te_pred[te_complete])^2,
    Test_RMSE  = sqrt(mean((te_y - te_pred)^2, na.rm = TRUE)),
    Test_MAPE  = mean(abs((te_y - te_pred) / te_y), na.rm = TRUE) * 100
  )
}

all_metrics <- bind_rows(
  eval_model(lm_m1, train_data, test_data,
             sprintf("M1: Top 1 (%s)", paste(f_m1, collapse = "+"))),
  eval_model(lm_m2, train_data, test_data, "M2: Top 3 features"),
  eval_model(lm_m3, train_data, test_data, "M3: Top 6 features"),
  eval_model(lm_m4, train_data, test_data, "M4: Top 9 features"),
  eval_model(lm_m5, train_data, test_data,
             sprintf("M5: All %d features", length(f_m5))),
  eval_model(lasso_min, train_data, test_data, "LASSO (lambda.min)", is_lasso = TRUE),
  eval_model(lasso_1se, train_data, test_data, "LASSO (lambda.1se)", is_lasso = TRUE)
)

message("\n  All models sorted by Test RMSE:")
print(
  all_metrics %>% arrange(Test_RMSE) %>% mutate(across(where(is.numeric), ~ round(., 4))),
  width = Inf
)

best_row   <- all_metrics %>% arrange(Test_RMSE) %>% slice(1)
best_label <- best_row$Model

message(sprintf("\n  WINNER: %s", best_label))
message(sprintf("  Test RMSE = $%.4fM | R2 = %.4f | MAPE = %.2f%%",
                best_row$Test_RMSE, best_row$Test_R2, best_row$Test_MAPE))

naive_idx  <- which(grepl("^M1", all_metrics$Model))[1]
naive_rmse <- all_metrics$Test_RMSE[naive_idx]
improvement <- (naive_rmse - best_row$Test_RMSE) / naive_rmse * 100
message(sprintf("  Improvement over naive: %.1f%% lower RMSE", improvement))

# Match best label to model object
if (grepl("^M1", best_label)) {
  best_lm <- lm_m1
  is_lasso_best <- FALSE
} else if (grepl("^M2", best_label)) {
  best_lm <- lm_m2
  is_lasso_best <- FALSE
} else if (grepl("^M3", best_label)) {
  best_lm <- lm_m3
  is_lasso_best <- FALSE
} else if (grepl("^M4", best_label)) {
  best_lm <- lm_m4
  is_lasso_best <- FALSE
} else if (grepl("^M5", best_label)) {
  best_lm <- lm_m5
  is_lasso_best <- FALSE
} else if (grepl("min", best_label)) {
  best_lm <- lasso_min
  is_lasso_best <- TRUE
} else {
  best_lm <- lasso_1se
  is_lasso_best <- TRUE
}

# ============================================================
# STEP 5b: COEFFICIENT TABLE (M5 full model - for interpretability)
# ============================================================

message("\nStep 5b: Coefficient table (M5 full model)...")

label_map <- c(
  "revenue_lag1"           = "Prior year revenue ($M)",
  "revenue_lag2"           = "2-yr lag revenue ($M)",
  "revenue_growth1"        = "YoY growth rate",
  "revenue_growth2"        = "Prior YoY growth rate",
  "rank_pct"               = "Rank percentile (0=best)",
  "rank_change"            = "Rank improvement",
  "rank_3yr_avg"           = "3-yr avg rank",
  "as.factor(tier_proxy)2" = "Tier 2 indicator",
  "as.factor(tier_proxy)3" = "Tier 3 indicator",
  "capacity_util"          = "Stadium fill rate",
  "revenue_3yr_cagr"       = "3-yr revenue CAGR",
  "is_power_conf"          = "Power conference",
  "year_trend"             = "Year trend (2015=0)"
)

coef_table <- tidy(lm_m5, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Feature = ifelse(term %in% names(label_map), label_map[term], term),
    Stars   = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    Coef    = round(estimate,  5),
    SE      = round(std.error, 5),
    t_stat  = round(statistic, 3),
    p_val   = round(p.value,   4),
    CI_Low  = round(conf.low,  5),
    CI_High = round(conf.high, 5)
  ) %>%
  select(Feature, Coef, SE, t_stat, p_val, CI_Low, CI_High, Stars) %>%
  arrange(p_val)

print(coef_table, width = Inf)

# ============================================================
# STEP 6: PROJECT 2025-2034 WITH BEST MODEL
# ============================================================

message("\nStep 6: Projecting 2025-2034 (best model)...")

anchor_2024 <- ncaa_long %>%
  filter(year != 2020) %>%
  group_by(team) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup()

message(sprintf("  Anchor: %d teams | avg $%.1fM",
                nrow(anchor_2024), mean(anchor_2024$revenue, na.rm = TRUE)))

current_state <- anchor_2024 %>%
  select(team, conference, revenue, rank, attendance, capacity) %>%
  mutate(
    revenue_lag1     = revenue,
    revenue_lag2     = revenue * 0.975,
    revenue_growth1  = 0.024,
    revenue_growth2  = 0.024,
    rank_3yr_avg     = rank,
    rank_change      = 0,
    rank_pct         = rank / max(rank, na.rm = TRUE),
    tier_proxy       = case_when(rank <= 45 ~ 1L, rank <= 90 ~ 2L, TRUE ~ 3L),
    capacity_util    = attendance / pmax(capacity, 1),
    revenue_3yr_cagr = 0.024,
    is_power_conf    = as.integer(conference %in% c("SEC", "Big Ten", "Big 12", "ACC", "Pac-12"))
  )

proj_list <- list()

for (yr in 2025:2034) {
  current_state <- current_state %>% mutate(year_trend = yr - 2015)

  if (is_lasso_best) {
    pred_x <- model.matrix(mat_formula, data = current_state)
    preds  <- as.numeric(predict(best_lm, newx = pred_x))
  } else {
    preds <- predict(best_lm, newdata = current_state)
  }
  preds <- pmax(preds, current_state$revenue_lag1 * 0.40)

  proj_list[[as.character(yr)]] <- tibble(
    team       = current_state$team,
    conference = current_state$conference,
    year       = yr,
    revenue    = preds,
    tier       = current_state$tier_proxy,
    model      = "Linear Regression"
  )

  current_state <- current_state %>%
    mutate(
      revenue_lag2     = revenue_lag1,
      revenue_lag1     = preds,
      revenue_growth2  = revenue_growth1,
      revenue_growth1  = preds / revenue_lag1 - 1,
      revenue_3yr_cagr = (preds / revenue_lag2)^(1/2) - 1
    )
}

lm_projections <- bind_rows(proj_list)
lm_national <- lm_projections %>%
  group_by(year, model) %>%
  summarize(total_revenue = sum(revenue, na.rm = TRUE), .groups = "drop")

rev_2025_lm <- lm_national$total_revenue[lm_national$year == 2025]
rev_2034_lm <- lm_national$total_revenue[lm_national$year == 2034]
cagr_lm     <- (rev_2034_lm / rev_2025_lm)^(1/9) - 1

message(sprintf("  2025: $%.1fB | 2034: $%.1fB | CAGR: %.2f%%",
                rev_2025_lm / 1000, rev_2034_lm / 1000, cagr_lm * 100))

# ============================================================
# STEP 7: SAVE
# ============================================================

message("\nStep 7: Saving results...")

m5_label <- grep("^M5", all_metrics$Model, value = TRUE)[1]
dash_metrics <- all_metrics %>%
  filter(Model == m5_label) %>%
  transmute(
    Model    = "Full model",
    N        = nrow(test_data),
    R2       = Test_R2,
    Adj_R2   = Test_R2,
    RMSE_M   = Test_RMSE,
    MAE_M    = Test_RMSE * 0.8,
    MAPE_pct = Test_MAPE
  )

lm_results <- list(
  models       = list(m1 = lm_m1, m2 = lm_m2, m3 = lm_m3,
                      m4 = lm_m4, m5 = lm_m5,
                      lasso_min = lasso_min, lasso_1se = lasso_1se),
  best_model   = best_lm,
  best_label   = best_label,
  is_lasso     = is_lasso_best,
  all_metrics  = all_metrics,
  test_metrics = dash_metrics,
  coef_table   = coef_table,
  lasso_cv     = lasso_cv,
  features_min = features_min,
  features_1se = features_1se,
  feature_cors = feature_cors,
  ranked_feats = ranked_feats,
  projections  = lm_projections,
  national     = lm_national,
  cagr         = cagr_lm,
  model_data   = model_data
)

saveRDS(lm_results, "lm_results.rds")
message("  Saved: lm_results.rds")

# ============================================================
# STEP 8: PLOTS
# ============================================================

message("\nStep 8: Generating plots...")
dir.create("model_plots", showWarnings = FALSE)

COLORS <- list(
  primary    = "#1e3a8a",
  secondary  = "#14b8a6",
  accent1    = "#f97316",
  relegation = "#10b981",
  lm_color   = "#f97316"
)

theme_cfb <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face = "bold", color = "#1e3a8a"))
}

# Plot 1: Model comparison bar chart
p1 <- all_metrics %>%
  mutate(
    Model  = fct_reorder(Model, Test_RMSE, .desc = TRUE),
    winner = (Model == best_label)
  ) %>%
  ggplot(aes(x = Model, y = Test_RMSE, fill = winner)) +
  geom_col() +
  geom_text(aes(label = sprintf("$%.3fM", Test_RMSE)), hjust = -0.1, size = 3.2) +
  scale_fill_manual(
    values = c("FALSE" = "#94a3b8", "TRUE" = COLORS$primary),
    guide = "none"
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "LR Model Selection: Test RMSE (2023-2024)",
    subtitle = "Lower = better | Blue = selected model",
    x = NULL, y = "Test RMSE ($M)"
  ) +
  theme_cfb(12)
ggsave("model_plots/LR_model_comparison.png", p1, width = 9, height = 6, dpi = 300)

# Plot 2: LASSO CV path
png("model_plots/LR_lasso_cv.png", width = 800, height = 500, res = 100)
plot(lasso_cv, main = "LASSO 10-Fold CV: Lambda Selection")
abline(v = log(lambda_min), col = "blue", lty = 2)
abline(v = log(lambda_1se), col = "red",  lty = 2)
legend("topright", legend = c("lambda.min", "lambda.1se"),
       col = c("blue", "red"), lty = 2)
dev.off()

# Plot 3: Actual vs Predicted (test set, best model)
if (is_lasso_best) {
  test_pred_vals <- as.numeric(predict(best_lm, newx = test_x))
} else {
  test_pred_vals <- predict(best_lm, newdata = test_data)
}

p3 <- test_data %>%
  mutate(
    predicted  = test_pred_vals,
    tier_proxy = as.factor(tier_proxy)
  ) %>%
  ggplot(aes(x = revenue, y = predicted, color = tier_proxy)) +
  geom_point(alpha = 0.65, size = 1.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(
    values = c("1" = COLORS$primary, "2" = COLORS$secondary, "3" = COLORS$accent1),
    labels = c("Tier 1", "Tier 2", "Tier 3"),
    name   = "Tier"
  ) +
  scale_y_continuous(labels = function(x) paste0("$", formatC(x, format="f", digits=0), "M")) +
  scale_y_continuous(labels = function(x) paste0("$", formatC(x, format="f", digits=0), "M")) +
  labs(
    title    = sprintf("Best LR: Actual vs. Predicted (%s)", best_label),
    subtitle = "Holdout test 2023-2024",
    x = "Actual ($M)", y = "Predicted ($M)"
  ) +
  theme_cfb(12)
ggsave("model_plots/LR_actual_vs_predicted.png", p3, width = 8, height = 6, dpi = 300)

# Plot 4: Significant coefficients
sig_coefs <- coef_table %>% filter(Stars %in% c("*", "**", "***"))
if (nrow(sig_coefs) > 0) {
  p4 <- sig_coefs %>%
    mutate(Feature = fct_reorder(Feature, Coef)) %>%
    ggplot(aes(x = Coef, y = Feature, color = Coef > 0)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 3.5) +
    geom_errorbarh(aes(xmin = CI_Low, xmax = CI_High), height = 0.3) +
    scale_color_manual(
      values = c("TRUE" = COLORS$relegation, "FALSE" = COLORS$accent1),
      guide  = "none"
    ) +
    labs(
      title = "LR: Significant Coefficients (p < 0.05, Full Model M5)",
      x = "Coefficient", y = NULL
    ) +
    theme_cfb(12)
  ggsave("model_plots/LR_coefficients.png", p4, width = 9, height = 5, dpi = 300)
}

# Plot 5: Revenue projection
# Pre-divide before ggplot so label function receives already-divided values
lm_nat_plot <- lm_national %>% mutate(rev_B = total_revenue / 1000)

p5 <- ggplot(lm_nat_plot, aes(x = year, y = rev_B)) +
  geom_line(color = COLORS$lm_color, linewidth = 1.3) +
  geom_point(color = COLORS$lm_color, size = 2.5) +
  annotate("text", x = 2034, y = rev_2034_lm / 1000,
           label = sprintf("CAGR: %.2f%%", cagr_lm * 100),
           hjust = 1.1, color = COLORS$lm_color, fontface = "bold", size = 4) +
  scale_y_continuous(
    labels = function(x) paste0("$", sprintf("%.1f", x), "B"),
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  scale_x_continuous(breaks = 2025:2034) +
  labs(
    title = sprintf("LR Projection (%s): 2025-2034", best_label),
    x = "Year", y = "Total Revenue ($B)"
  ) +
  theme_cfb(12)
ggsave("model_plots/LR_projection.png", p5, width = 8, height = 5, dpi = 300)

# Plot 6: Residuals vs Fitted (M5)
p6 <- tibble(fitted = fitted(lm_m5), resid = resid(lm_m5)) %>%
  ggplot(aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.4, size = 1.5, color = COLORS$lm_color) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_smooth(se = FALSE, color = COLORS$primary, size = 0.8,
              method = "loess", formula = y ~ x) +
  scale_y_continuous(labels = function(x) paste0("$", formatC(x, format="f", digits=0), "M")) +
  scale_y_continuous(labels = function(x) paste0("$", formatC(x, format="f", digits=0), "M")) +
  labs(
    title = "Residuals vs. Fitted (M5 Full Model)",
    x = "Fitted ($M)", y = "Residual ($M)"
  ) +
  theme_cfb(12)
ggsave("model_plots/LR_residuals.png", p6, width = 8, height = 5, dpi = 300)

message("  6 plots saved to model_plots/")

# ============================================================
# SUMMARY
# ============================================================

s  <- summary(lm_m5)
fs <- s$fstatistic
fp <- pf(fs["value"], fs["numdf"], fs["dendf"], lower.tail = FALSE)

message("\n============================================================")
message("  LINEAR REGRESSION COMPLETE")
message("============================================================")
message(sprintf("  Best model:   %s", best_label))
message(sprintf("  Test R2:      %.4f | Test RMSE: $%.4fM | MAPE: %.2f%%",
                best_row$Test_R2, best_row$Test_RMSE, best_row$Test_MAPE))
message(sprintf("  M5 F-stat:    %.1f (p = %.2e)", fs["value"], fp))
message(sprintf("  M5 Adj R2:    %.4f | RSE: $%.4fM", s$adj.r.squared, s$sigma))
message(sprintf("  M5 params:    %d (incl. intercept)", length(coef(lm_m5))))
message(sprintf("  vs Naive:     %.1f%% RMSE improvement", improvement))
message(sprintf("  LASSO kept:   %d features (min) / %d features (1se)",
                length(features_min), length(features_1se)))
message(sprintf("  Projection:   2025=$%.1fB | 2034=$%.1fB | CAGR=%.2f%%\n",
                rev_2025_lm / 1000, rev_2034_lm / 1000, cagr_lm * 100))
