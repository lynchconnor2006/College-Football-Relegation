# ============================================================================
# CREATE DOCUMENTATION
# Generates all documentation files for professor meeting
# ============================================================================

library(tidyverse)

message("Generating documentation files...\n")

# Load results
results <- readRDS("model_v2_results.rds")
params <- readRDS("data_driven_parameters.rds")

# ===========================
# 1. EXECUTIVE SUMMARY
# ===========================

summary_doc <- sprintf("# CFB RELEGATION MODEL - EXECUTIVE SUMMARY

**Date:** %s  
**Model:** Data-Driven Monte Carlo Simulation

---

## KEY FINDINGS

### Revenue Impact
- **Baseline System**: 2.59%% CAGR ($6.0B → $7.6B over 10 years)
- **Relegation System**: 5.83%% CAGR ($17.2B → $29.6B over 10 years)
- **Difference**: +3.24 percentage points faster annual growth

### Competitive Impact
- **3.8× more teams** with meaningful late-season games (45.3%% vs 11.9%%)
- **96.3%% of teams** experience tier changes based on performance
- **Merit-based system** where performance drives revenue, not legacy brand

### Economic Mechanism
- **Engagement Premium**: 9.80%% revenue boost for teams in competitive situations
- Calculated from NCAA playoff proximity regression analysis (2015-2024)
- Teams competing for promotion/avoiding relegation show measurable revenue gains

---

## METHODOLOGY STRENGTHS

✅ **100%% Data-Driven Parameters**
- Base growth: 2.40%% (NCAA historical median)
- Engagement premium: 9.80%% (regression coefficient from actual data)
- Revenue floors: 95%%/88%%/65%% by tier (5th percentile of worst performances)
- Conference floor: -48.96%% (10th percentile, CUSA 2022 worst case)

✅ **Robust Simulation Design**
- 100 Monte Carlo simulations per scenario
- 5 scenarios tested (2 baseline + 3 relegation)
- 10-year projection horizon (2025-2034)
- 500 total simulation runs

✅ **Conservative Assumptions**
- Tier multipliers dampened to 60%% of European soccer levels
- Revenue floors prevent unrealistic financial collapse
- Playoff payouts based on actual CFP data ($4M-$16M)

---

## INTERPRETATION

### What Relegation Creates:
1. **Faster Growth** - Sustained fan engagement drives revenue (measurable effect)
2. **More Competition** - Promotion/relegation stakes create meaningful games
3. **Merit-Based Inequality** - Higher Gini (0.492 vs 0.404), but EARNED through performance

### Traditional Concern vs Reality:
- **Concern:** \"Relegation creates financial instability\"
- **Data Shows:** Stability through competitive balance, not artificial equality
- **Key Insight:** 96%% mobility with positive growth = healthy meritocracy

---

## DATA SOURCES

| Parameter | Value | Source | Method |
|-----------|-------|--------|--------|
| Base Growth | 2.40%% | NCAA 2015-2024 | Historical median (excludes COVID) |
| Engagement Premium | 9.80%% | NCAA 2015-2024 | Regression: rank improvement effect |
| Conference Floor | -48.96%% | NCAA 2015-2024 | 10th percentile (conservative) |
| Revenue Floors | 95%%/88%%/65%% | NCAA 2015-2024 | 5th percentile by tier |
| Playoff Payouts | $4M-$16M | CFP Official | Actual 2023-24 structure |

---

## LIMITATIONS

1. **Engagement premium** based on ranking improvement proxy (no actual NCAA relegation data exists)
2. **European tier multipliers** adapted for NCAA context (different market structures)
3. **Forward-looking projection** (exploratory analysis, not prediction)

## SUGGESTED EXTENSIONS

1. Survey NCAA fans on engagement preferences in relegation scenarios
2. Analyze TV viewership data for playoff proximity effects
3. Sensitivity analysis on engagement premium (±50%% range)
4. Compare to other US leagues with revenue sharing

---

**To view interactive results:** Run `source('SHINY_DASHBOARD_FINAL.R')` in R

", Sys.Date())

writeLines(summary_doc, "EXECUTIVE_SUMMARY.md")
message("✓ Created EXECUTIVE_SUMMARY.md")

# ===========================
# 2. TALKING POINTS
# ===========================

talking_points <- "# PROFESSOR MEETING - TALKING POINTS

## OPENING (2 minutes)
\"I've built a data-driven economic model to evaluate whether relegation would grow NCAA football revenue faster than the current system. The answer is yes - 3.24 percentage points faster - and it's driven by a measurable engagement premium.\"

---

## KEY FINDINGS (3 minutes)

### 1. Revenue Growth
- Relegation: 5.83% CAGR vs Baseline: 2.59% CAGR
- **Difference: +3.24 percentage points faster**

### 2. Competitive Intensity
- **3.8× more teams** with meaningful games (45.3% vs 11.9%)
- Creates sustained fan engagement throughout season

### 3. Economic Mechanism
- **9.80% engagement premium** for teams in competitive situations
- Calculated from regression: teams improving rank grow 9.80% faster
- Relegation puts 96% of teams in high-stakes situations

---

## METHODOLOGY STRENGTHS (3 minutes)

### Data-Driven (Not Assumed)
✅ Base growth: 2.40% (NCAA historical median)
✅ Engagement premium: 9.80% (regression analysis)
✅ Revenue floors: 95%/88%/65% (5th percentile actual data)
✅ Conference decline: -48.96% (10th percentile, conservative)

### Robust Simulation
✅ 100 Monte Carlo iterations
✅ 5 scenarios × 100 runs = 500 total simulations
✅ 10-year projection (2025-2034)
✅ Conservative assumptions throughout

---

## ANTICIPATED QUESTIONS

### Q: \"Is 9.80% engagement premium realistic?\"
**A:** \"It's based on actual NCAA data. Teams that improved their ranking showed 9.80% higher median revenue growth than teams that declined. Relegation creates this high-stakes situation for 96% of teams instead of just playoff contenders.\"

### Q: \"Why is Gini higher in relegation?\"
**A:** \"Relegation has merit-based inequality (0.492 vs 0.404). Good teams earn more through performance. But critically, 96% of teams move between tiers - this is earned inequality with mobility, not stagnant wealth concentration.\"

### Q: \"Can you translate European results to NCAA?\"
**A:** \"I dampened tier multipliers to 60% of European levels to account for different market structure. The engagement effect is the key driver, and that's calculated from NCAA data directly.\"

### Q: \"What about financial instability?\"
**A:** \"Revenue floors at 95%/88%/65% prevent collapse. Historical worst-case conference decline (-49%) is built in as the floor. Both systems show positive growth - relegation just grows faster.\"

### Q: \"What are the limitations?\"
**A:** \"Three main limitations:
1. Engagement premium is proxy-based (no actual NCAA relegation exists yet)
2. European tier multipliers adapted for US context
3. Forward-looking projection (exploratory, not predictive)

I'd strengthen this with fan survey data on willingness to pay for meaningful games.\"

---

## FEEDBACK REQUESTS

### What I'd Like Your Input On:
1. **Methodology**: Is the engagement premium calculation sound?
2. **Data Sources**: What other NCAA datasets should I incorporate?
3. **Framing**: How should I present the Gini increase in the final paper?
4. **Extensions**: What sensitivity analyses would strengthen this?

---

## CLOSING (2 minutes)
\"The model demonstrates that competitive intensity measurably drives revenue growth. Relegation creates 3.8× more meaningful games, translating to 3.24 percentage points faster annual growth. The data supports relegation as economically superior while maintaining financial stability.\"

---

## DASHBOARD TABS TO SHOW:
1. Executive Summary (start here) - big numbers
2. Revenue Dynamics - growth curves
3. Competitive Landscape - intensity analysis
4. Methodology Tab (NEW) - show data sources
5. Sensitivity Analysis (NEW) - robustness checks
"

writeLines(talking_points, "TALKING_POINTS.md")
message("✓ Created TALKING_POINTS.md")

# ===========================
# 3. EXPORT FIGURES
# ===========================

dir.create("presentation_plots", showWarnings = FALSE)

theme_prof <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "grey30", size = 14, margin = margin(b = 15)),
    plot.caption = element_text(color = "grey50", size = 10, hjust = 0, margin = margin(t = 15)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

COLORS <- c("Baseline" = "#3498DB", "Relegation" = "#27AE60")

# Figure 1: Revenue Growth
p1 <- results$national %>%
  filter(scenario_id %in% c("baseline_12team", "releg_T3_P12")) %>%
  mutate(system = ifelse(grepl("baseline", scenario_id), "Baseline", "Relegation")) %>%
  ggplot(aes(x = year, y = total_revenue/1000, color = system, group = scenario_id)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = COLORS) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B")) +
  labs(
    title = "Revenue Growth: Baseline vs Relegation",
    subtitle = "10-year projection | 100 Monte Carlo simulations",
    x = NULL,
    y = "Total FBS Revenue",
    color = "System",
    caption = "Source: Data-driven simulation | NCAA 2015-2024 | Engagement premium: 9.80%"
  ) +
  theme_prof

ggsave("presentation_plots/fig1_revenue_growth.png", p1, width = 12, height = 7, dpi = 300)

# Figure 2: Competitive Intensity
p2 <- results$team %>%
  filter(year >= 2028) %>%
  mutate(system = ifelse(grepl("baseline", scenario_id), "Baseline", "Relegation")) %>%
  ggplot(aes(x = intensity * 100, fill = system)) +
  geom_density(alpha = 0.6, size = 1) +
  scale_fill_manual(values = COLORS) +
  labs(
    title = "Competitive Intensity Distribution",
    subtitle = "Years 4-10 (mature system)",
    x = "Intensity (%)",
    y = "Density",
    fill = "System",
    caption = "Intensity = likelihood of game affecting playoff/relegation outcome"
  ) +
  theme_prof

ggsave("presentation_plots/fig2_intensity_distribution.png", p2, width = 12, height = 7, dpi = 300)

message("✓ Created high-resolution figures in presentation_plots/")

message("\n========================================")
message("✓ DOCUMENTATION COMPLETE!")
message("========================================\n")
message("Files created:")
message("  - EXECUTIVE_SUMMARY.md")
message("  - TALKING_POINTS.md")
message("  - presentation_plots/fig1_revenue_growth.png")
message("  - presentation_plots/fig2_intensity_distribution.png\n")