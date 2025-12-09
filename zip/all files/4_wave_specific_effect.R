# ==============================================================================
# EVENT-STUDY ANALYSIS: WAVE-SPECIFIC COVID EFFECTS
# Fixed Effects Models with Wave Dummies
# ==============================================================================

# ------------------------------------------------------------------------------
# LOAD LIBRARIES
# ------------------------------------------------------------------------------
library(tidyverse)
library(fixest)
library(ggplot2)
library(patchwork)

# ------------------------------------------------------------------------------
# SET WORKING DIRECTORY AND LOAD DATA
# ------------------------------------------------------------------------------
setwd("/Users/choochoojc/Desktop/STAT204/Homeworks/Final Project")

analysis_data_grok <- read.csv("analysis_data_grok.csv")

# ==============================================================================
# PART 1: PREPARE DATA FOR EVENT STUDY
# ==============================================================================

# ------------------------------------------------------------------------------
# Adjust Panel Variable (Wave Numbers)
# ------------------------------------------------------------------------------
# Convert panel from 2-6 to 1-5 for easier interpretation
analysis_data_grok <- analysis_data_grok %>%
  mutate(panel = panel - 1)

cat("Panel waves adjusted: now coded as 1-5\n")
cat("Wave 1 = baseline (pre-COVID)\n")
cat("Waves 4-5 = COVID period\n\n")

# ------------------------------------------------------------------------------
# Define Analysis Samples (Match Original Paper Specifications)
# ------------------------------------------------------------------------------
df_cons <- analysis_data_grok %>%
  filter(
    Outlier_Consumption_Panel == 0,
    Number_Waves == 5,
    Outlier_Missing_Panel == 0
  )

df_spend <- analysis_data_grok %>%
  filter(
    Number_Waves == 5,
    Outlier_Missing_Panel == 0
  )

# Ensure panel is integer for factor analysis
df_cons$panel  <- as.integer(df_cons$panel)
df_spend$panel <- as.integer(df_spend$panel)

cat("Consumption sample size:", nrow(df_cons), "\n")
cat("Spending sample size:", nrow(df_spend), "\n\n")

# ==============================================================================
# PART 2: DEFINE OUTCOMES AND HELPER FUNCTIONS
# ==============================================================================

# ------------------------------------------------------------------------------
# Define Outcome Variables and Labels
# ------------------------------------------------------------------------------
outcomes_cons <- c(
  ln_Consumption_Total = "Total Consumption",
  ln_Consumption_Rec   = "Recorded Consumption",
  ln_Consumption_Live  = "Live Consumption"
)

outcomes_spend <- c(
  ln_Spending_Total = "Total Spending",
  ln_Spending_Rec   = "Recorded Spending",
  ln_Spending_Live  = "Live Spending"
)

# ------------------------------------------------------------------------------
# Helper Function: Fit Fixed Effects Model with Wave Dummies
# ------------------------------------------------------------------------------
# Treats wave 1 as baseline reference category
# Model: log(Y+1) ~ i(panel, ref = 1) + controls | Pseudonym
# ------------------------------------------------------------------------------
fit_wave_model <- function(y, df) {
  feols(
    as.formula(
      paste0(
        y, " ~ i(panel, ref = 1) + ",
        "MusicEducation + MusicAppreciation + ActiveListening + MainstreamMusic + ",
        "Age + Children + i(gender, ref = 'weiblich') + ln_Income | Pseudonym"
      )
    ),
    data = df,
    vcov = ~ Pseudonym
  )
}

# ------------------------------------------------------------------------------
# Helper Function: Extract HP Percent Effects for Each Wave
# ------------------------------------------------------------------------------
# Computes Halvorsen-Palmquist percent effects relative to wave 1 baseline
# HP = exp(β_wave) - 1
# ------------------------------------------------------------------------------
get_hp_wave <- function(model, outcome_label, type_label) {
  cf  <- coef(model)
  ci  <- confint(model)
  
  # Extract coefficients for wave dummies (named "panel::2", "panel::3", etc.)
  idx <- grepl("^panel::", names(cf))
  cf_wave <- cf[idx]
  ci_wave <- ci[idx, , drop = FALSE]
  
  tibble(
    Type     = type_label,
    Outcome  = outcome_label,
    Wave     = as.integer(sub("panel::", "", names(cf_wave))),
    Effect   = exp(cf_wave) - 1,
    CI_lower = exp(ci_wave[, 1]) - 1,
    CI_upper = exp(ci_wave[, 2]) - 1
  )
}

# ==============================================================================
# PART 3: ESTIMATE WAVE-SPECIFIC MODELS
# ==============================================================================

cat("=== ESTIMATING WAVE-SPECIFIC MODELS ===\n\n")

# ------------------------------------------------------------------------------
# Fit Models for Consumption Outcomes
# ------------------------------------------------------------------------------
cat("Fitting consumption models...\n")
models_cons_wave <- lapply(names(outcomes_cons), fit_wave_model, df = df_cons)
names(models_cons_wave) <- outcomes_cons

# ------------------------------------------------------------------------------
# Fit Models for Spending Outcomes
# ------------------------------------------------------------------------------
cat("Fitting spending models...\n")
models_spend_wave <- lapply(names(outcomes_spend), fit_wave_model, df = df_spend)
names(models_spend_wave) <- outcomes_spend

cat("All models estimated successfully.\n\n")

# ==============================================================================
# PART 4: EXTRACT HP EFFECTS FOR ALL WAVES
# ==============================================================================

cat("=== EXTRACTING HP EFFECTS ===\n\n")

# ------------------------------------------------------------------------------
# Extract HP Effects for Consumption
# ------------------------------------------------------------------------------
hp_cons <- bind_rows(
  mapply(
    get_hp_wave,
    model         = models_cons_wave,
    outcome_label = names(models_cons_wave),
    type_label    = "Consumption",
    SIMPLIFY      = FALSE
  )
)

# ------------------------------------------------------------------------------
# Extract HP Effects for Spending
# ------------------------------------------------------------------------------
hp_spend <- bind_rows(
  mapply(
    get_hp_wave,
    model         = models_spend_wave,
    outcome_label = names(models_spend_wave),
    type_label    = "Spending",
    SIMPLIFY      = FALSE
  )
)

# ------------------------------------------------------------------------------
# Combine All HP Effects
# ------------------------------------------------------------------------------
hp_all <- bind_rows(hp_cons, hp_spend)

# Order outcomes for consistent plotting
hp_all$Outcome <- factor(
  hp_all$Outcome,
  levels = c(
    "Total Consumption", "Total Spending",
    "Recorded Consumption", "Recorded Spending",
    "Live Consumption", "Live Spending"
  )
)

# ---- NEW: create label for magnitude (percent) ----
hp_all <- hp_all %>%
  mutate(
    Effect_label = sprintf("%.2f", Effect)
  )

cat("HP effects extracted for", nrow(hp_all), "wave-outcome combinations.\n\n")

# ==============================================================================
# PART 5: VISUALIZE EVENT-STUDY EFFECTS
# ==============================================================================

cat("=== CREATING EVENT-STUDY PLOT ===\n\n")

# ------------------------------------------------------------------------------
# Plot: Wave-Specific Effects (Event Study Style)
# ------------------------------------------------------------------------------
wave <- ggplot(
  hp_all,
  aes(x = Wave, y = Effect, color = Type, group = Type)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.15,
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = Effect_label),
    vjust = -0.8,
    size = 2.8,
    show.legend = FALSE
  ) +
  facet_wrap(~ Outcome, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = 1:5) +
  labs(
    x = "Wave (1 = pre-COVID baseline; 4–5 = COVID period)",
    y = "HP percent effect vs. wave 1 (exp(β) − 1)",
    title = "Wave-specific effects on music spending and consumption",
    subtitle = "Fixed-effects models with wave dummies (wave 1 as reference)",
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "gray95"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(wave)

# ==============================================================================
# PART 6: SUMMARY STATISTICS AND DIAGNOSTICS
# ==============================================================================

# ------------------------------------------------------------------------------
# Display Summary of Wave Effects
# ------------------------------------------------------------------------------
cat("\n=== SUMMARY OF WAVE EFFECTS ===\n\n")

hp_summary <- hp_all %>%
  group_by(Type, Wave) %>%
  summarise(
    Mean_Effect = mean(Effect),
    Min_Effect  = min(Effect),
    Max_Effect  = max(Effect),
    .groups     = "drop"
  )

print(hp_summary)

# ==============================================================================
# PART 7: SAVE OUTPUTS
# ==============================================================================
ggsave(
  "5_wave_specific_effect.png",
  plot = wave,
  width = 12,
  height = 8,
  dpi = 320
)
