# NCAA Football Relegation — Economic Simulation Analysis

A sports analytics research project investigating whether replacing the NCAA Football Bowl Subdivision (FBS) playoff system with a European-style three-tier promotion and relegation structure would accelerate total revenue growth across all 134 FBS programs.

![R](https://img.shields.io/badge/R-4.4-276DC3?style=flat&logo=r&logoColor=white)
![Monte Carlo](https://img.shields.io/badge/Monte%20Carlo-Simulation-1e3a8a?style=flat)
![Random Forest](https://img.shields.io/badge/Random%20Forest-ranger-0f766e?style=flat)
![Shiny](https://img.shields.io/badge/Shiny-Dashboard-6d28d9?style=flat)

---

## Overview

This project combines Monte Carlo simulation, Linear Regression, and Random Forest models trained on a decade of NCAA financial data to quantify the 10-year revenue impact of structural change in college football. The central finding is that a three-tier relegation system would generate a projected **$10.5B revenue premium** over 10 years compared to the current playoff format, driven by an empirically measured **9.80% engagement premium** from historical NCAA data.

---

## Key Results

| Model | 2025 | 2034 | CAGR | Role |
|---|---|---|---|---|
| Linear Regression | $7.5B | $7.5B | 0.08% | Feature interpretability |
| Random Forest | $7.4B | $7.4B | -0.10% | Predictive accuracy |
| Monte Carlo (Baseline) | $6.0B | $7.6B | 2.59% | Status quo simulation |
| **Monte Carlo (Relegation)** | **$7.8B** | **$18.1B** | **9.80%** | **Relegation simulation** |

- LR Test R² = 0.996, RMSE = $2.80M per team | RF Test R² = 0.998, RMSE = $1.91M per team
- Prior year revenue is the single strongest predictor (coef = 0.991, p < 0.001)
- LASSO retained only 3 of 12 features — revenue is highly self-perpetuating under the status quo
- RF optimal hyperparameters: 1,200 trees, mtry = 4, min.node.size = 1 (100-combination grid search)
- Relegation expands programs in meaningful competition from 11.9% to 31.3% annually

---

## Data Sources

- **USA Today** NCAA Athletic Department Revenue Database
- **Knight Newhouse** College Athletics Database
- **College Football Reference** — win-loss records, rankings, strength of schedule
- **247Sports** — recruiting class ratings and composite rankings
- Individual athletic department pages — gap filling for missing programs
- **UEFA** individual club financial reports — England, Germany, Spain, France, Italy; three tiers per country, 2015–2023
- **Transfermarkt** — supplementary European club financial data

> Missing data: programs with incomplete financials were assigned the average revenue of their conference positional third (top, middle, bottom) for that year, preserving the known conference-level revenue structure.

---

## Repository Structure

```
collegerelegation/
│
├── cfb_enhanced_part1.R          # Data ingestion and cleaning
├── cfb_enhanced_part2.R          # Feature engineering
├── cfb_enhanced_part3.R          # EDA and parameter extraction
├── DATA_EXTRACTION.R             # Parameter extraction pipeline
├── DATA_DRIVEN_PARAMETERS.R      # Engagement premium calculation
├── PARAMETER_UPDATES.R           # Model configuration
├── FULL_MODEL_V2_PART1.R         # Monte Carlo helper functions
├── FULL_MODEL_V2_PART2.R         # Monte Carlo simulation engine
├── ADD_METRICS.R                 # Gini, mobility, intensity metrics
├── LINEAR_REGRESSION_MODEL.R     # LR with data-driven feature selection
├── RANDOM_FOREST_MODEL.R         # RF with programmatic grid search
├── MODEL_COMPARISON.R            # Cross-model comparison and plots
├── SHINY_DASHBOARD_FINAL.R       # 11-tab interactive Shiny dashboard
├── COMPLETE_PIPELINE.R           # Master runner (Phases 1-13)
│
├── ncaa_long.rds                 # Cleaned NCAA data ($M units)
├── euro_long.rds                 # European soccer tier data
├── extracted_parameters.rds      # Historical parameters
├── data_driven_parameters.rds    # Engagement premium and params
├── model_v2_results.rds          # Monte Carlo results
├── lm_results.rds                # Linear Regression results
├── rf_results.rds                # Random Forest results
├── model_comparison_table.rds    # Cross-model comparison
└── all_model_projections.rds     # Combined 2025-2034 projections
```

---

## How to Run

### Full Pipeline (Phases 1–13)
```r
setwd("path/to/collegerelegation")
source("COMPLETE_PIPELINE.R")
```

### Run Individual Phases
```r
# Phase 9: Linear Regression (~2 min)
source("LINEAR_REGRESSION_MODEL.R")

# Phase 10: Random Forest grid search (~2-3 min)
source("RANDOM_FOREST_MODEL.R")

# Phase 11: Model comparison
source("MODEL_COMPARISON.R")

# Phase 13: Launch Shiny dashboard
source("SHINY_DASHBOARD_FINAL.R")
shinyApp(ui, server)
```

### Required R Packages
```r
install.packages(c(
  "tidyverse", "ranger", "glmnet", "broom",
  "shiny", "shinydashboard", "DT", "plotly",
  "scales", "ggplot2", "patchwork", "viridis"
))
```

---

## Model Details

### Monte Carlo Simulation
- 100 independent runs per scenario, 5 scenarios total (500 simulations)
- Hierarchical growth: macro AR(1) shock → conference-level rates → team idiosyncratic volatility
- Relegation adds: 9.80% engagement premium, tier multipliers (60% of European levels), 3-year phase-in, revenue floors (65% / 88% / 95% by tier)
- Tier structure: Tier 1 = top 45 programs, Tier 2 = middle 45, Tier 3 = bottom 44; bottom/top 5 swap annually

### Linear Regression
- Features ranked by absolute correlation with revenue on training data only (no leakage)
- Five nested OLS specifications (M1: top 1 feature → M5: all 12) plus LASSO at lambda.min and lambda.1se
- 10-fold cross-validation for LASSO lambda selection
- Model selected by holdout test RMSE
- Train: 3,979 obs (2015–2022) | Test: 1,070 obs (2023–2024)

### Random Forest
- 100-combination programmatic grid search derived from p = 13 features and n = 3,979 training rows
- Selection criterion: OOB RMSE — no test data used during tuning
- Optimal: 1,200 trees, mtry = 4, min.node.size = 1
- Permutation-based variable importance
- Tuning gain: 6.6% lower RMSE and 18.0% lower MAPE vs default specification

---

## Dashboard

An 11-tab interactive Shiny dashboard is included:

| Tab | Content |
|---|---|
| 1 | Executive Summary |
| 2 | Revenue Dynamics |
| 3 | Competitive Landscape |
| 4 | Equity and Fairness |
| 5 | Conference Impact |
| 6 | Team Explorer |
| 7 | Methodology |
| 8 | Sensitivity Analysis |
| 9 | Linear Regression — full statistical report |
| 10 | Random Forest — grid search, importance, partial dependence |
| 11 | Model Comparison — all four projections |

Live dashboard: https://lynchconnor2006.shinyapps.io/collegerelegation

---

## Tier Structure

| Tier | Programs | Revenue Floor | Annual Movement |
|---|---|---|---|
| Tier 1 | Top 45 by revenue | 95% of prior year | Bottom 5 drop to Tier 2 |
| Tier 2 | Middle 45 by revenue | 88% of prior year | Top 5 promote, bottom 5 drop |
| Tier 3 | Bottom 44 by revenue | 65% of prior year | Top 5 promote to Tier 2 |

---

## Limitations

- Engagement premium measured from revenue rank improvement as a proxy, not direct fan demand or TV ratings data
- European tier multipliers dampened to 60% without a known true NCAA equivalent
- ML models capture the status quo only and cannot model behavioral responses to relegation incentives
- Static tier boundaries do not account for conference realignment or program reclassification

---

## References

- USA Today. (2024). *NCAA athletic department revenue database*. https://sports.usatoday.com/ncaa/finances
- Sports Reference LLC. (2024). *College football statistics and history*. https://www.sports-reference.com/cfb
- 247Sports. (2024). *College football recruiting rankings*. https://247sports.com
- Knight Newhouse Data. (2024). *College athletic financial information database*. https://knightnewhousedata.org
- UEFA. (2015–2023). *Individual club financial reports*. https://www.uefa.com
- Transfermarkt. (2024). *Club revenue and financial data*. https://www.transfermarkt.us

---

*Syracuse University, Falk College — Sports Analytics | April 2026*
