# ============================================================================
# RANDOM FOREST MODEL - CFB Revenue Prediction
# Programmatically derived hyperparameter grid search (OOB RMSE selection)
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ranger)
  library(scales)
  library(ggplot2)
})

setwd("C:/Users/lynch/Downloads/collegerelegation")

message("\n============================================================")
message("  RANDOM FOREST - Hyperparameter Grid Search and Tuning")
message("============================================================\n")

# ============================================================
# STEP 1: LOAD DATA AND ENGINEER FEATURES
# Same feature set as LR for direct comparison
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
  ) %>%
  mutate(
    conference = as.factor(conference),
    tier_proxy = as.factor(tier_proxy)
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

FEATURES <- c(
  "revenue_lag1", "revenue_lag2", "revenue_growth1", "revenue_growth2",
  "rank_pct", "rank_change", "rank_3yr_avg", "tier_proxy",
  "capacity_util", "revenue_3yr_cagr", "is_power_conf", "conference", "year_trend"
)

train_rf <- train_data %>% select(revenue, all_of(FEATURES))
test_rf  <- test_data  %>% select(revenue, all_of(FEATURES))

p       <- length(FEATURES)
n_train <- nrow(train_rf)

message(sprintf("  Training: %d obs | Testing: %d obs | Features: %d",
                n_train, nrow(test_rf), p))
message(sprintf("  sqrt(p) = %.2f | p/3 = %.2f", sqrt(p), p / 3))

# ============================================================
# STEP 3: PROGRAMMATICALLY DERIVED HYPERPARAMETER GRID
# Grid values computed from p and n - no manual hardcoding.
#   num.trees : 5 log-spaced values 200 to 1200
#   mtry      : 5 values anchored at sqrt(p) and p/3 with surrounding range
#   min.node  : 4 log-spaced values from 1 to max(10, n/500)
# Selection criterion: OOB RMSE (no test data touched during tuning)
# ============================================================

message("\nStep 3: Building programmatic hyperparameter grid...")

tree_grid <- unique(round(exp(seq(log(200), log(1200), length.out = 5))))

mtry_candidates <- unique(sort(c(
  2,
  floor(sqrt(p)),
  round(sqrt(p)),
  floor(p / 3),
  floor(p / 2),
  p - 1
)))
mtry_grid <- mtry_candidates[mtry_candidates >= 1 & mtry_candidates <= p]

node_max      <- max(10, floor(n_train / 500))
nodesize_grid <- unique(round(exp(seq(log(1), log(node_max), length.out = 4))))

total_runs <- length(tree_grid) * length(mtry_grid) * length(nodesize_grid)

message(sprintf("  Trees:    %s", paste(tree_grid, collapse = ", ")))
message(sprintf("  mtry:     %s  [sqrt(p)=%.1f, p/3=%.1f]",
                paste(mtry_grid, collapse = ", "), sqrt(p), p / 3))
message(sprintf("  min.node: %s", paste(nodesize_grid, collapse = ", ")))
message(sprintf("  Total combinations: %d", total_runs))
message(sprintf("  Estimated time: ~%.0f minutes\n", total_runs * 0.035))

# ============================================================
# STEP 4: RUN GRID SEARCH
# ============================================================

message("Step 4: Running grid search...")

run_counter  <- 0
grid_results <- vector("list", total_runs)

for (nt in tree_grid) {
  for (mt in mtry_grid) {
    for (ns in nodesize_grid) {
      run_counter <- run_counter + 1
      set.seed(4242)

      rf_tmp <- ranger(
        formula       = revenue ~ .,
        data          = train_rf,
        num.trees     = nt,
        mtry          = mt,
        min.node.size = ns,
        importance    = "none",
        seed          = 4242
      )

      oob_rmse  <- sqrt(rf_tmp$prediction.error)
      oob_r2    <- rf_tmp$r.squared
      te_pred   <- predict(rf_tmp, data = test_rf)$predictions
      test_rmse <- sqrt(mean((test_rf$revenue - te_pred)^2, na.rm = TRUE))
      test_r2   <- cor(test_rf$revenue, te_pred)^2
      test_mape <- mean(abs((test_rf$revenue - te_pred) / test_rf$revenue), na.rm = TRUE) * 100

      grid_results[[run_counter]] <- list(
        num_trees     = nt,
        mtry          = mt,
        min_node_size = ns,
        oob_r2        = oob_r2,
        oob_rmse      = oob_rmse,
        test_r2       = test_r2,
        test_rmse     = test_rmse,
        test_mape     = test_mape
      )

      report_every <- max(1, floor(total_runs / 4))
      if (run_counter %% report_every == 0 || run_counter == total_runs) {
        current_best <- min(sapply(grid_results[seq_len(run_counter)],
                                   function(x) x$oob_rmse))
        message(sprintf("  ... %d/%d done | best OOB RMSE so far: $%.4fM",
                        run_counter, total_runs, current_best))
      }
    }
  }
}

grid_df <- bind_rows(lapply(grid_results, as_tibble))

message(sprintf("\n  Grid search complete: %d combinations evaluated", nrow(grid_df)))

# Best hyperparameters by OOB RMSE
best <- grid_df %>% arrange(oob_rmse) %>% slice(1)

message("\n  BEST HYPERPARAMETERS (lowest OOB RMSE):")
message(sprintf("    num.trees     = %d", best$num_trees))
message(sprintf("    mtry          = %d  [sqrt(p)=%.1f, p/3=%.1f]",
                best$mtry, sqrt(p), p / 3))
message(sprintf("    min.node.size = %d", best$min_node_size))
message(sprintf("    OOB R2        = %.4f | OOB RMSE = $%.4fM",
                best$oob_r2, best$oob_rmse))
message(sprintf("    Test R2       = %.4f | Test RMSE = $%.4fM | MAPE = %.2f%%",
                best$test_r2, best$test_rmse, best$test_mape))

message("\n  Top 10 combinations (by OOB RMSE):")
print(
  grid_df %>% arrange(oob_rmse) %>% head(10) %>%
    mutate(across(where(is.numeric), ~ round(., 4))),
  width = Inf
)

# Marginal effects of each hyperparameter
mtry_effect <- grid_df %>%
  group_by(mtry) %>%
  summarize(avg_oob = mean(oob_rmse), min_oob = min(oob_rmse), .groups = "drop")

trees_effect <- grid_df %>%
  group_by(num_trees) %>%
  summarize(avg_oob = mean(oob_rmse), min_oob = min(oob_rmse), .groups = "drop")

nodesize_effect <- grid_df %>%
  group_by(min_node_size) %>%
  summarize(avg_oob = mean(oob_rmse), min_oob = min(oob_rmse), .groups = "drop")

message("\n  mtry marginal effect (avg OOB RMSE across all other combos):")
print(mtry_effect %>% mutate(across(where(is.numeric), ~ round(., 4))))
message("  num.trees marginal effect:")
print(trees_effect %>% mutate(across(where(is.numeric), ~ round(., 4))))
message("  min.node.size marginal effect:")
print(nodesize_effect %>% mutate(across(where(is.numeric), ~ round(., 4))))

# ============================================================
# STEP 5: FIT FINAL OPTIMAL MODEL WITH PERMUTATION IMPORTANCE
# ============================================================

message("\nStep 5: Fitting final optimal model with permutation importance...")

set.seed(4242)
rf_optimal <- ranger(
  formula       = revenue ~ .,
  data          = train_rf,
  num.trees     = best$num_trees,
  mtry          = best$mtry,
  min.node.size = best$min_node_size,
  importance    = "permutation",
  seed          = 4242
)

# Default RF for comparison (sqrt(p) mtry, 500 trees, node=5)
mtry_default <- floor(sqrt(p))
set.seed(4242)
rf_default <- ranger(
  formula       = revenue ~ .,
  data          = train_rf,
  num.trees     = 500,
  mtry          = mtry_default,
  min.node.size = 5,
  importance    = "impurity",
  seed          = 4242
)

message(sprintf("  Default RF  (500 trees, mtry=%d, node=5): OOB RMSE = $%.4fM",
                mtry_default, sqrt(rf_default$prediction.error)))
message(sprintf("  Optimal RF  (%d trees, mtry=%d, node=%d): OOB RMSE = $%.4fM",
                best$num_trees, best$mtry, best$min_node_size,
                sqrt(rf_optimal$prediction.error)))

# ============================================================
# STEP 6: HOLDOUT TEST EVALUATION
# ============================================================

message("\nStep 6: Holdout test evaluation...")

eval_rf <- function(model, test_df, label) {
  pred   <- predict(model, data = test_df)$predictions
  actual <- test_df$revenue
  resid  <- actual - pred
  tibble(
    Model    = label,
    N        = nrow(test_df),
    R2       = cor(actual, pred)^2,
    RMSE_M   = sqrt(mean(resid^2, na.rm = TRUE)),
    MAE_M    = mean(abs(resid),   na.rm = TRUE),
    MAPE_pct = mean(abs(resid / actual), na.rm = TRUE) * 100
  )
}

rf_metrics <- bind_rows(
  eval_rf(rf_default, test_rf,
          sprintf("RF Default (500 trees, mtry=%d, node=5)", mtry_default)),
  eval_rf(rf_optimal, test_rf,
          sprintf("RF Optimal (%d trees, mtry=%d, node=%d)",
                  best$num_trees, best$mtry, best$min_node_size))
)

message("  Test Set Performance:")
print(rf_metrics %>% mutate(across(where(is.numeric), ~ round(., 4))), width = Inf)

tuning_gain_rmse <- (rf_metrics$RMSE_M[1] - rf_metrics$RMSE_M[2]) /
                     rf_metrics$RMSE_M[1] * 100
tuning_gain_mape <- (rf_metrics$MAPE_pct[1] - rf_metrics$MAPE_pct[2]) /
                     rf_metrics$MAPE_pct[1] * 100

message(sprintf("\n  Tuning gain: RMSE %.2f%% better | MAPE %.2f%% better",
                tuning_gain_rmse, tuning_gain_mape))

# ============================================================
# STEP 7: VARIABLE IMPORTANCE
# ============================================================

message("\nStep 7: Variable importance (permutation, optimal model)...")

feat_label_map <- c(
  "revenue_lag1"     = "Prior year revenue",
  "revenue_lag2"     = "2-year lag revenue",
  "revenue_growth1"  = "YoY growth rate",
  "revenue_growth2"  = "Prior YoY growth",
  "rank_pct"         = "Rank percentile",
  "rank_change"      = "Rank change vs prior yr",
  "rank_3yr_avg"     = "3-yr avg rank",
  "tier_proxy"       = "Tier (1/2/3)",
  "capacity_util"    = "Stadium fill rate",
  "revenue_3yr_cagr" = "3-yr revenue CAGR",
  "is_power_conf"    = "Power conference",
  "conference"       = "Conference",
  "year_trend"       = "Year trend"
)

vi_df <- tibble(
  raw_name   = names(rf_optimal$variable.importance),
  Importance = rf_optimal$variable.importance
) %>%
  mutate(
    Feature = ifelse(raw_name %in% names(feat_label_map),
                     feat_label_map[raw_name], raw_name)
  ) %>%
  arrange(desc(Importance)) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Feature, Importance)

print(vi_df %>% mutate(Importance = round(Importance, 1)), n = 13)

# ============================================================
# STEP 8: PARTIAL DEPENDENCE (rank_pct)
# ============================================================

message("\nStep 8: Partial dependence (rank percentile)...")

rank_seq <- seq(0.01, 1.0, by = 0.05)
pd_rank  <- lapply(rank_seq, function(r) {
  temp  <- train_rf %>% mutate(rank_pct = r)
  preds <- predict(rf_optimal, data = temp)$predictions
  data.frame(rank_pct = r, avg_revenue = mean(preds))
})
pd_rank <- bind_rows(pd_rank)

message(sprintf("  Best teams avg: $%.1fM | Worst: $%.1fM | Spread: $%.1fM",
                pd_rank$avg_revenue[1],
                pd_rank$avg_revenue[nrow(pd_rank)],
                pd_rank$avg_revenue[1] - pd_rank$avg_revenue[nrow(pd_rank)]))

# ============================================================
# STEP 9: PROJECT 2025-2034 WITH OPTIMAL MODEL
# ============================================================

message("\nStep 9: Projecting 2025-2034 (optimal model)...")

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
    rank_pct         = rank / max(rank, na.rm = TRUE),
    rank_change      = 0,
    rank_3yr_avg     = rank,
    tier_proxy       = as.factor(case_when(rank <= 45 ~ 1L, rank <= 90 ~ 2L, TRUE ~ 3L)),
    capacity_util    = attendance / pmax(capacity, 1),
    revenue_3yr_cagr = 0.024,
    is_power_conf    = as.integer(conference %in% c("SEC", "Big Ten", "Big 12", "ACC", "Pac-12")),
    conference       = as.factor(conference)
  )

proj_list_rf <- list()

for (yr in 2025:2034) {
  pred_data <- current_state %>%
    mutate(year_trend = yr - 2015) %>%
    select(all_of(FEATURES))

  preds <- predict(rf_optimal, data = pred_data)$predictions
  preds <- pmax(preds, current_state$revenue_lag1 * 0.40)

  proj_list_rf[[as.character(yr)]] <- tibble(
    team       = current_state$team,
    conference = as.character(current_state$conference),
    year       = yr,
    revenue    = preds,
    tier       = as.integer(as.character(current_state$tier_proxy)),
    model      = "Random Forest"
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

rf_projections <- bind_rows(proj_list_rf)
rf_national <- rf_projections %>%
  group_by(year, model) %>%
  summarize(total_revenue = sum(revenue, na.rm = TRUE), .groups = "drop")

rev_2025_rf <- rf_national$total_revenue[rf_national$year == 2025]
rev_2034_rf <- rf_national$total_revenue[rf_national$year == 2034]
cagr_rf     <- (rev_2034_rf / rev_2025_rf)^(1/9) - 1

message(sprintf("  2025: $%.1fB | 2034: $%.1fB | CAGR: %.2f%%",
                rev_2025_rf / 1000, rev_2034_rf / 1000, cagr_rf * 100))

# ============================================================
# STEP 10: SAVE
# ============================================================

message("\nStep 10: Saving results...")

rf_results <- list(
  models          = list(default = rf_default, optimal = rf_optimal),
  best_model      = rf_optimal,
  best_params     = best,
  grid_results    = grid_df,
  rf_metrics      = rf_metrics,
  vi_df           = vi_df,
  pd_rank         = pd_rank,
  projections     = rf_projections,
  national        = rf_national,
  cagr            = cagr_rf,
  features        = FEATURES,
  mtry_effect     = mtry_effect,
  trees_effect    = trees_effect,
  nodesize_effect = nodesize_effect,
  tuning_gain_rmse = tuning_gain_rmse
)

saveRDS(rf_results, "rf_results.rds")
message("  Saved: rf_results.rds")

# ============================================================
# STEP 11: PLOTS
# ============================================================

message("\nStep 11: Generating plots...")
dir.create("model_plots", showWarnings = FALSE)

COLORS <- list(
  primary    = "#1e3a8a",
  secondary  = "#14b8a6",
  accent1    = "#f97316",
  rf_color   = "#8b5cf6",
  relegation = "#10b981"
)

theme_cfb <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face = "bold", color = "#1e3a8a"))
}

# Plot 1: Grid search heatmap (slice at best node size)
best_ns   <- best$min_node_size
heat_data <- grid_df %>%
  filter(min_node_size == best_ns) %>%
  mutate(num_trees = factor(num_trees), mtry = factor(mtry))

p1 <- ggplot(heat_data, aes(x = mtry, y = num_trees, fill = oob_rmse)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = sprintf("%.3f", oob_rmse)), size = 2.8,
            color = "white", fontface = "bold") +
  scale_fill_gradient(low = COLORS$rf_color, high = "#e5e7eb",
                      name = "OOB RMSE ($M)") +
  annotate("text",
           x = as.character(best$mtry),
           y = as.character(best$num_trees),
           label = "*", size = 10, color = COLORS$accent1) +
  labs(
    title    = sprintf("Grid Search Heatmap: OOB RMSE (min.node.size = %d)", best_ns),
    subtitle = "Darker = lower error (better) | * = selected combination",
    x = "mtry (features per split)", y = "Number of Trees"
  ) +
  theme_cfb(11)
ggsave("model_plots/RF_grid_search_heatmap.png", p1, width = 9, height = 6, dpi = 300)

# Plot 2: Trees convergence
p2 <- ggplot(trees_effect, aes(x = num_trees, y = avg_oob)) +
  geom_line(color = COLORS$rf_color, size = 1.2) +
  geom_point(color = COLORS$rf_color, size = 2.5) +
  geom_vline(xintercept = best$num_trees, color = COLORS$accent1, linetype = "dashed") +
  labs(
    title    = "OOB RMSE vs Number of Trees",
    subtitle = "Dashed = selected value",
    x = "Number of Trees", y = "Avg OOB RMSE ($M)"
  ) +
  theme_cfb(12)
ggsave("model_plots/RF_trees_convergence.png", p2, width = 7, height = 5, dpi = 300)

# Plot 3: mtry sensitivity
p3 <- ggplot(mtry_effect, aes(x = factor(mtry), y = avg_oob)) +
  geom_col(fill = COLORS$rf_color, alpha = 0.85) +
  geom_vline(xintercept = as.character(best$mtry),
             color = COLORS$accent1, linetype = "dashed", size = 1.2) +
  labs(
    title    = "mtry Sensitivity: Effect on OOB RMSE",
    subtitle = sprintf("p = %d | sqrt(p) = %.1f | p/3 = %.1f", p, sqrt(p), p / 3),
    x = "mtry (features sampled per split)", y = "Avg OOB RMSE ($M)"
  ) +
  theme_cfb(12)
ggsave("model_plots/RF_mtry_sensitivity.png", p3, width = 7, height = 5, dpi = 300)

# Plot 4: Variable importance bar
p4 <- vi_df %>%
  head(10) %>%
  mutate(Feature = fct_reorder(Feature, Importance)) %>%
  ggplot(aes(x = Importance, y = Feature, fill = Importance)) +
  geom_col() +
  scale_fill_gradient(low = COLORS$secondary, high = COLORS$rf_color, guide = "none") +
  labs(
    title    = "RF Optimal: Variable Importance (Top 10)",
    subtitle = "Permutation importance with tuned hyperparameters",
    x = "Permutation Importance", y = NULL
  ) +
  theme_cfb(12)
ggsave("model_plots/RF_variable_importance.png", p4, width = 9, height = 6, dpi = 300)

# Plot 5: Partial dependence
p5 <- ggplot(pd_rank, aes(x = rank_pct, y = avg_revenue)) +
  geom_line(color = COLORS$rf_color, size = 1.3) +
  geom_ribbon(aes(ymin = avg_revenue * 0.93, ymax = avg_revenue * 1.07),
              alpha = 0.15, fill = COLORS$rf_color) +
  geom_vline(xintercept = c(45/134, 90/134),
             linetype = "dashed", color = COLORS$accent1, alpha = 0.8) +
  annotate("text", x = 45/134, y = max(pd_rank$avg_revenue) * 0.96,
           label = "T1|T2", hjust = -0.1, color = COLORS$accent1, size = 3) +
  annotate("text", x = 90/134, y = max(pd_rank$avg_revenue) * 0.90,
           label = "T2|T3", hjust = -0.1, color = COLORS$accent1, size = 3) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = function(x) paste0("$", formatC(x, format="f", digits=0), "M")) +
  labs(
    title    = "Partial Dependence: Rank Percentile -> Revenue (Optimal RF)",
    subtitle = "Dashed lines = tier boundaries",
    x = "Rank Percentile (0% = best)", y = "Avg Predicted Revenue ($M)"
  ) +
  theme_cfb(12)
ggsave("model_plots/RF_partial_dependence.png", p5, width = 8, height = 5, dpi = 300)

# Plot 6: Actual vs Predicted
p6 <- test_rf %>%
  mutate(
    predicted  = predict(rf_optimal, data = test_rf)$predictions,
    tier_proxy = as.character(tier_proxy)
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
    title    = "RF Optimal: Actual vs. Predicted Revenue",
    subtitle = "Holdout test 2023-2024",
    x = "Actual ($M)", y = "Predicted ($M)"
  ) +
  theme_cfb(12)
ggsave("model_plots/RF_actual_vs_predicted.png", p6, width = 8, height = 6, dpi = 300)

message("  6 plots saved to model_plots/")

# ============================================================
# SUMMARY
# ============================================================

message("\n============================================================")
message("  RANDOM FOREST TUNING COMPLETE")
message("============================================================")
message(sprintf("  Optimal:   %d trees | mtry=%d | min.node.size=%d",
                best$num_trees, best$mtry, best$min_node_size))
message(sprintf("  OOB:       R2=%.4f | RMSE=$%.4fM",
                best$oob_r2, best$oob_rmse))
message(sprintf("  Test:      R2=%.4f | RMSE=$%.4fM | MAPE=%.2f%%",
                best$test_r2, best$test_rmse, best$test_mape))
message(sprintf("  vs Default: RMSE %.2f%% better | MAPE %.2f%% better",
                tuning_gain_rmse, tuning_gain_mape))
message(sprintf("  Projection: 2025=$%.1fB | 2034=$%.1fB | CAGR=%.2f%%\n",
                rev_2025_rf / 1000, rev_2034_rf / 1000, cagr_rf * 100))
message("  Next: Run MODEL_COMPARISON.R\n")
