# ==============================================================================
# MUSIC CONSUMPTION AND COVID-19 ANALYSIS
# Fixed Effects Models, Fractional Logit, and Visualizations
# ==============================================================================

# ------------------------------------------------------------------------------
# LOAD LIBRARIES
# ------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(fixest)
library(fmlogit)
library(modelsummary)
library(ggplot2)
library(patchwork)
library(openxlsx)

# ------------------------------------------------------------------------------
# SET WORKING DIRECTORY AND LOAD DATA
# ------------------------------------------------------------------------------
setwd("/Users/choochoojc/Desktop/STAT204/Homeworks/Final Project")

analysis_data_grok <- read.csv("analysis_data_grok.csv")
summary_grok <- read.csv("summarystatistics_grok.csv")

# ==============================================================================
# PART 1: FIXED EFFECTS MODELS
# ==============================================================================

# ------------------------------------------------------------------------------
# Model (I)a: Total Consumption
# ------------------------------------------------------------------------------
m1a_cons <- feols(
  ln_Consumption_Total ~ 
    Corona + Summer + MusicEducation + MusicAppreciation +
    ActiveListening + MainstreamMusic + 
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) + 
    i(Occupation, ref = 3) + 
    Age + Children + ln_Income | 
    Pseudonym,
  data = analysis_data_grok %>% 
    filter(
      Outlier_Consumption_Panel == 0,
      Number_Waves == 5,
      Outlier_Missing_Panel == 0
    ),
  vcov = ~ Pseudonym
)

summary(m1a_cons)

# ------------------------------------------------------------------------------
# Model (I)b: Total Spending
# ------------------------------------------------------------------------------
m1b_spend <- feols(
  ln_Spending_Total ~ 
    Corona + Summer + MusicEducation + MusicAppreciation +
    ActiveListening + MainstreamMusic + 
    PurchaseReason_Atmosphere + PurchaseReason_Flexibility + 
    PurchaseReason_Habit + PurchaseReason_SoundQuality + 
    PurchaseReason_Mobility + PurchaseReason_Other + 
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) + 
    i(Occupation, ref = 3) + 
    Age + Children + ln_Income | 
    Pseudonym,
  data = analysis_data_grok %>% 
    filter(
      Number_Waves == 5,
      Outlier_Missing_Panel == 0
    ),
  vcov = ~ Pseudonym
)

summary(m1b_spend)

# ------------------------------------------------------------------------------
# Model (II)a: Recorded Market Consumption
# ------------------------------------------------------------------------------
m2a_rec_cons <- feols(
  ln_Consumption_Rec ~ 
    Corona + Summer + MusicEducation + MusicAppreciation +
    ActiveListening + MainstreamMusic +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = analysis_data_grok %>%
    filter(
      Outlier_Consumption_Panel == 0,
      Number_Waves == 5,
      Outlier_Missing_Panel == 0
    ),
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model (II)b: Recorded Market Spending
# ------------------------------------------------------------------------------
m2b_rec_spend <- feols(
  ln_Spending_Rec ~ 
    Corona + Summer + MusicEducation + MusicAppreciation +
    ActiveListening + MainstreamMusic +
    PurchaseReason_Atmosphere + PurchaseReason_Flexibility +
    PurchaseReason_Habit + PurchaseReason_SoundQuality +
    PurchaseReason_Mobility + PurchaseReason_Other +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = analysis_data_grok %>%
    filter(
      Number_Waves == 5,
      Outlier_Missing_Panel == 0
    ),
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model (III)a: Live Market Consumption
# ------------------------------------------------------------------------------
m3a_live_cons <- feols(
  ln_Consumption_Live ~ 
    Corona + Summer + MusicEducation + MusicAppreciation +
    ActiveListening + MainstreamMusic +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = analysis_data_grok %>%
    filter(
      Outlier_Consumption_Panel == 0,
      Number_Waves == 5,
      Outlier_Missing_Panel == 0
    ),
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model (III)b: Live Market Spending
# ------------------------------------------------------------------------------
m3b_live_spend <- feols(
  ln_Spending_Live ~ 
    Corona + Summer + MusicEducation + MusicAppreciation +
    ActiveListening + MainstreamMusic +
    PurchaseReason_Atmosphere + PurchaseReason_Flexibility +
    PurchaseReason_Habit + PurchaseReason_SoundQuality +
    PurchaseReason_Mobility + PurchaseReason_Other + PurchaseReason_None +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = analysis_data_grok %>%
    filter(
      Number_Waves == 5,
      Outlier_Missing_Panel == 0
    ),
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Quick Sanity Check: Display All Model Summaries
# ------------------------------------------------------------------------------
cat("\n=== Model Summaries ===\n")
summary(m1a_cons)
summary(m1b_spend)
summary(m2a_rec_cons)
summary(m2b_rec_spend)
summary(m3a_live_cons)
summary(m3b_live_spend)

# ==============================================================================
# PART 2: HALVORSEN-PALMQUIST PERCENT EFFECTS
# ==============================================================================

# ------------------------------------------------------------------------------
# Extract Corona Coefficients
# ------------------------------------------------------------------------------
coef_m1a_cons <- coef(m1a_cons)["Corona"]
coef_m1b_spend <- coef(m1b_spend)["Corona"]
coef_m2a_rec_cons <- coef(m2a_rec_cons)["Corona"]
coef_m2b_rec_spend <- coef(m2b_rec_spend)["Corona"]
coef_m3a_live_cons <- coef(m3a_live_cons)["Corona"]
coef_m3b_live_spend <- coef(m3b_live_spend)["Corona"]

# ------------------------------------------------------------------------------
# Calculate Halvorsen & Palmquist Percent Effects
# ------------------------------------------------------------------------------
HP_m1a_cons <- (exp(coef_m1a_cons) - 1)
HP_m1b_spend <- (exp(coef_m1b_spend) - 1)
HP_m2a_rec_cons <- (exp(coef_m2a_rec_cons) - 1)
HP_m2b_rec_spend <- (exp(coef_m2b_rec_spend) - 1)
HP_m3a_live_cons <- (exp(coef_m3a_live_cons) - 1)
HP_m3b_live_spend <- (exp(coef_m3b_live_spend) - 1)

# ------------------------------------------------------------------------------
# Function to Compute 95% CI for HP Effect
# ------------------------------------------------------------------------------
HP_CI <- function(model, var = "Corona") {
  ci_log <- confint(model, var)
  ci_hp <- exp(ci_log) - 1
  return(ci_hp)
}

# ------------------------------------------------------------------------------
# Calculate Confidence Intervals
# ------------------------------------------------------------------------------
CI_m1a_cons <- HP_CI(m1a_cons)
CI_m1b_spend <- HP_CI(m1b_spend)
CI_m2a_rec_cons <- HP_CI(m2a_rec_cons)
CI_m2b_rec_spend <- HP_CI(m2b_rec_spend)
CI_m3a_live_cons <- HP_CI(m3a_live_cons)
CI_m3b_live_spend <- HP_CI(m3b_live_spend)

# ------------------------------------------------------------------------------
# Create Results Table
# ------------------------------------------------------------------------------
HP_table <- data.frame(
  Type = c(
    "Consumption", "Spending", 
    "Consumption", "Spending", 
    "Consumption", "Spending"
  ), 
  Outcome = c(
    "Total Consumption",
    "Total Spending",
    "Recorded Consumption",
    "Recorded Spending",
    "Live Consumption",
    "Live Spending"
  ),
  Percent_Effect = c(
    HP_m1a_cons,
    HP_m1b_spend,
    HP_m2a_rec_cons,
    HP_m2b_rec_spend,
    HP_m3a_live_cons,
    HP_m3b_live_spend
  ),
  CI_Lower = c(
    CI_m1a_cons$`2.5 %`,
    CI_m1b_spend$`2.5 %`,
    CI_m2a_rec_cons$`2.5 %`,
    CI_m2b_rec_spend$`2.5 %`,
    CI_m3a_live_cons$`2.5 %`,
    CI_m3b_live_spend$`2.5 %`
  ),
  CI_Upper = c(
    CI_m1a_cons$`97.5 %`,
    CI_m1b_spend$`97.5 %`,
    CI_m2a_rec_cons$`97.5 %`,
    CI_m2b_rec_spend$`97.5 %`,
    CI_m3a_live_cons$`97.5 %`,
    CI_m3b_live_spend$`97.5 %`
  )
)

print(HP_table)

# ==============================================================================
# PART 3: EXPORT MODEL RESULTS TO EXCEL
# ==============================================================================

models_fe <- list(
  "Total Consumption" = m1a_cons,
  "Total Spending" = m1b_spend,
  "Recorded Consumption" = m2a_rec_cons,
  "Recorded Spending" = m2b_rec_spend,
  "Live Consumption" = m3a_live_cons,
  "Live Spending" = m3b_live_spend
)

modelsummary(
  models_fe,
  output = "corona_music_fe.xlsx",
  statistic = c("std.error", "statistic", "p.value"),
  stars = TRUE
)

# ==============================================================================
# PART 4: FRACTIONAL LOGIT MODEL - CONSUMPTION
# ==============================================================================

cat("\n=== FRACTIONAL LOGIT: CONSUMPTION SHARES ===\n")

# ------------------------------------------------------------------------------
# Prepare Data with Sample Selection
# ------------------------------------------------------------------------------
df_sub <- analysis_data_grok %>%
  filter(
    Outlier_Consumption_Panel == 0,
    Number_Waves == 5,
    Outlier_Missing_Panel == 0
  )

# ------------------------------------------------------------------------------
# Create Outcome Shares Matrix
# ------------------------------------------------------------------------------
y <- df_sub %>%
  dplyr::select(
    s_Consumption_PremiumStream_R,
    s_Consumption_FreeStream_R,
    s_Consumption_Digital_R,
    s_Consumption_Physical_R,
    s_Consumption_Radio_R,
    s_Consumption_OnlineRadio_R,
    s_No_Consumption_R
  ) %>%
  as.matrix()

# ------------------------------------------------------------------------------
# Relevel Categorical Variables
# ------------------------------------------------------------------------------
df_sub <- df_sub %>%
  mutate(
    MaritalStatus = relevel(factor(MaritalStatus), ref = "2"),
    Education = relevel(factor(Education), ref = "1"),
    Occupation = relevel(factor(Occupation), ref = "3")
  )

# ------------------------------------------------------------------------------
# Create Covariates Matrix
# ------------------------------------------------------------------------------
X <- df_sub %>%
  dplyr::select(
    Corona, Summer, MusicEducation, MusicAppreciation,
    ActiveListening, MainstreamMusic,
    MaritalStatus, Education, Occupation,
    GenderFemale, Age, Children, ln_Income
  )

# ------------------------------------------------------------------------------
# Estimate Fractional Logit Model
# ------------------------------------------------------------------------------
cat("=== Starting fmlogit estimation (Consumption) ===\n")
print(Sys.time())
start <- proc.time()

set.seed(204)

fm <- fmlogit(
  y = y,
  X = X,
  MLEmethod = "BHHH",
  cluster = df_sub$Pseudonym,
  reps = 1000
)

cat("=== Estimation finished ===\n")
print(Sys.time())
cat(sprintf(
  "Elapsed time: %.2f minutes\n\n", 
  (proc.time() - start)["elapsed"] / 60
))

# ------------------------------------------------------------------------------
# Calculate Average Partial Effects (APEs)
# ------------------------------------------------------------------------------
ape <- effects.fmlogit(
  object = fm,
  varlist = "Corona",
  effect = "marginal",
  marg.type = "aveacr",
  se = TRUE
)

# ==============================================================================
# PART 5: FRACTIONAL LOGIT MODEL - SPENDING
# ==============================================================================

cat("\n=== FRACTIONAL LOGIT: SPENDING SHARES ===\n")

# ------------------------------------------------------------------------------
# Prepare Data with Sample Selection
# ------------------------------------------------------------------------------
df_sub_2 <- analysis_data_grok %>%
  filter(
    Number_Waves == 5,
    Outlier_Missing_Panel == 0
  )

# ------------------------------------------------------------------------------
# Create Outcome Shares Matrix
# ------------------------------------------------------------------------------
y2 <- df_sub_2 %>%
  dplyr::select(
    s_Spending_Streaming_R,
    s_Spending_Physical_R,
    s_Spending_Digital_R,
    s_No_Spending_R
  ) %>%
  as.matrix()

# ------------------------------------------------------------------------------
# Relevel Categorical Variables
# ------------------------------------------------------------------------------
df_sub_2 <- df_sub_2 %>%
  mutate(
    MaritalStatus = relevel(factor(MaritalStatus), ref = "2"),
    Education = relevel(factor(Education), ref = "1"),
    Occupation = relevel(factor(Occupation), ref = "3")
  )

# ------------------------------------------------------------------------------
# Create Covariates Matrix
# ------------------------------------------------------------------------------
X2 <- df_sub_2 %>%
  dplyr::select(
    Corona, Summer, MusicEducation, MusicAppreciation,
    ActiveListening, MainstreamMusic, 
    PurchaseReason_Atmosphere, PurchaseReason_Flexibility, 
    PurchaseReason_Habit, PurchaseReason_SoundQuality, 
    PurchaseReason_Mobility, PurchaseReason_Other, 
    MaritalStatus, Education, Occupation,
    GenderFemale, Age, Children, ln_Income
  )

# ------------------------------------------------------------------------------
# Estimate Fractional Logit Model
# ------------------------------------------------------------------------------
cat("=== Starting fmlogit estimation (Spending) ===\n")
print(Sys.time())
start <- proc.time()

set.seed(204)

fm2 <- fmlogit(
  y = y2,
  X = X2,
  MLEmethod = "BHHH",
  cluster = df_sub_2$Pseudonym,
  reps = 1000
)

cat("=== Estimation finished ===\n")
print(Sys.time())
cat(sprintf(
  "Elapsed time: %.2f minutes\n\n", 
  (proc.time() - start)["elapsed"] / 60
))

# ------------------------------------------------------------------------------
# Calculate Average Partial Effects (APEs)
# ------------------------------------------------------------------------------
ape2 <- effects.fmlogit(
  object = fm2,
  varlist = "Corona",
  effect = "marginal",
  marg.type = "aveacr",
  se = TRUE
)

# ==============================================================================
# PART 6: PREPARE DATA FOR VISUALIZATION
# ==============================================================================

# ------------------------------------------------------------------------------
# Tidy APEs from Consumption Model
# ------------------------------------------------------------------------------
ape1_df <- data.frame(
  Outcome = rownames(ape$effects),
  Effect = ape$effects[, "Corona"],
  SE = ape$se[, "Corona"], 
  pval = ape$ztable$Corona[, "p-value"]
) %>%
  mutate(
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE,
    Group = "Consumption shares"
  )

# ------------------------------------------------------------------------------
# Tidy APEs from Spending Model
# ------------------------------------------------------------------------------
ape2_df <- data.frame(
  Outcome = rownames(ape2$effects),
  Effect = ape2$effects[, "Corona"],
  SE = ape2$se[, "Corona"], 
  pval = ape2$ztable$Corona[, "p-value"]
) %>%
  mutate(
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE,
    Group = "Spending shares"
  )

# ------------------------------------------------------------------------------
# Combine and Add Significance Stars
# ------------------------------------------------------------------------------
ape_all_df <- bind_rows(ape1_df, ape2_df) %>%
  mutate(
    stars = case_when(
      pval <= 0.001 ~ "***",
      pval <= 0.01 ~ "**",
      pval <= 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# ==============================================================================
# PART 7: VISUALIZATION - AVERAGE PARTIAL EFFECTS
# ==============================================================================

# ------------------------------------------------------------------------------
# Map Outcome Names to Pretty Labels
# ------------------------------------------------------------------------------
label_map <- tibble::tibble(
  Outcome = c(
    # Consumption shares
    "s_Consumption_PremiumStream_R",
    "s_Consumption_FreeStream_R",
    "s_Consumption_Digital_R",
    "s_Consumption_Physical_R",
    "s_Consumption_Radio_R",
    "s_Consumption_OnlineRadio_R",
    "s_No_Consumption_R",
    # Spending shares
    "s_Spending_Streaming_R",
    "s_Spending_Physical_R",
    "s_Spending_Digital_R",
    "s_No_Spending_R"
  ),
  Channel = c(
    # Consumption
    "Premium streaming",
    "Free streaming",
    "Digital downloads",
    "Physical formats",
    "Radio",
    "Online radio",
    "No consumption",
    # Spending
    "Streaming",
    "Physical formats",
    "Digital downloads",
    "No spending"
  ),
  Market = c(
    rep("Consumption shares", 7),
    rep("Spending shares", 4)
  )
)

# ------------------------------------------------------------------------------
# Join Labels and Add Direction
# ------------------------------------------------------------------------------
ape_pretty <- ape_all_df %>%
  left_join(label_map, by = "Outcome") %>%
  mutate(
    Sign = if_else(Effect >= 0, "Increase", "Decrease")
  )

# ------------------------------------------------------------------------------
# Prepare Consumption Panel
# ------------------------------------------------------------------------------
cons_levels <- c(
  "Premium streaming",
  "Free streaming",
  "Digital downloads",
  "Physical formats",
  "Radio",
  "Online radio",
  "No consumption"
)

ape_cons <- ape_pretty %>%
  filter(Market == "Consumption shares") %>%
  mutate(Channel = factor(Channel, levels = rev(cons_levels)))

# ------------------------------------------------------------------------------
# Prepare Spending Panel
# ------------------------------------------------------------------------------
spend_levels <- c(
  "Streaming",
  "Physical formats",
  "Digital downloads",
  "No spending"
)

ape_spend <- ape_pretty %>%
  filter(Market == "Spending shares") %>%
  mutate(Channel = factor(Channel, levels = rev(spend_levels)))

# ------------------------------------------------------------------------------
# Define Colorblind-Friendly Colors (Okabe–Ito Palette)
# ------------------------------------------------------------------------------
cb_colors <- c(
  "Increase" = "#0072B2",  # blue
  "Decrease" = "#D55E00"   # orange
)

# ------------------------------------------------------------------------------
# Plot: Consumption Shares
# ------------------------------------------------------------------------------
p_cons <- ggplot(
    ape_cons,
    aes(x = Channel, y = Effect, color = Sign)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15
  ) +
  geom_text(
    aes(label = stars, y = Effect),
    size = 4,
    vjust = 0,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = sprintf("%.3f", Effect)),
    size = 3,
    vjust = -1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cb_colors) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Average partial effect of COVID-19",
    title = "APEs of COVID-19 on music consumption",
    color = "Direction"
  ) +
  theme_bw(base_size = 11)

# ------------------------------------------------------------------------------
# Plot: Spending Shares
# ------------------------------------------------------------------------------
p_spend <- ggplot(
    ape_spend,
    aes(x = Channel, y = Effect, color = Sign)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15
  ) +
  geom_text(
    aes(label = stars, y = Effect),
    size = 4,
    vjust = 0,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = sprintf("%.3f", Effect)),
    size = 3,
    vjust = -1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cb_colors) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Average partial effect of COVID-19",
    title = "APEs of COVID-19 on music spending",
    color = "Direction"
  ) +
  theme_bw(base_size = 11)

# ------------------------------------------------------------------------------
# Combine APE Panels
# ------------------------------------------------------------------------------
p_apes_nice <- p_cons | p_spend +
  plot_annotation(
    title = "Effect of COVID-19 (Corona) on music consumption and spending shares",
    subtitle = "* p < 0.1   ** p < 0.05   *** p < 0.01"
  )

print(p_apes_nice)

# ==============================================================================
# PART 8: VISUALIZATION - HALVORSEN-PALMQUIST EFFECTS
# ==============================================================================

# ------------------------------------------------------------------------------
# Prepare HP Data for Plotting
# ------------------------------------------------------------------------------
HP_plot_df <- HP_table %>%
  mutate(
    Outcome = factor(
      Outcome,
      levels = c(
        "Total Consumption", "Total Spending",
        "Recorded Consumption", "Recorded Spending",
        "Live Consumption", "Live Spending"
      )
    ),
    Sign = if_else(Percent_Effect >= 0, "Increase", "Decrease")
  )

# ------------------------------------------------------------------------------
# Plot: HP Percent Effects - Spending
# ------------------------------------------------------------------------------
p_hp_spending <- ggplot(
  HP_plot_df %>% filter(Type == "Spending"),
  aes(x = Percent_Effect, y = Outcome, color = Sign)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(xmin = CI_Lower, xmax = CI_Upper),
    height = 0.2
  ) +
  geom_text(
    aes(label = sprintf("%.3f", Percent_Effect)),
    nudge_y = 0.25,
    size = 3.8,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cb_colors) +
  labs(
    x = "HP percent effect of COVID (exp(β) − 1)",
    y = NULL,
    title = "Spending outcomes",
    color = "Direction"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# ------------------------------------------------------------------------------
# Plot: HP Percent Effects - Consumption
# ------------------------------------------------------------------------------
p_hp_consumption <- ggplot(
  HP_plot_df %>% filter(Type == "Consumption"),
  aes(x = Percent_Effect, y = Outcome, color = Sign)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(xmin = CI_Lower, xmax = CI_Upper),
    height = 0.2
  ) +
  geom_text(
    aes(label = sprintf("%.3f", Percent_Effect)),
    nudge_y = 0.25,
    size = 3.8,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cb_colors) +
  labs(
    x = "HP percent effect of COVID (exp(β) − 1)",
    y = NULL,
    title = "Consumption outcomes",
    color = "Direction"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# ------------------------------------------------------------------------------
# Combined HP Plot
# ------------------------------------------------------------------------------
combined_plot <- (p_hp_spending | p_hp_consumption) +
  plot_annotation(
    title = "Effect of COVID-19 on music spending (left) and consumption (right)",
    subtitle = "Halvorsen–Palmquist percent effects (exp(β) − 1)",
    theme = theme(
      plot.title = element_text(hjust = 0, size = 13, face = "bold"),
      plot.subtitle = element_text(hjust = 0, size = 11)
    )
  )

print(combined_plot)

# ==============================================================================
# PART 9: SAVE ALL PLOTS
# ==============================================================================

# Save HP effects plot
ggsave(
  "2_FE_models.png", 
  plot = combined_plot,
  width = 12,
  height = 6.5,
  dpi = 320
)

# Save APE plot
ggsave(
  "3_APE.png", 
  plot = p_apes_nice,
  width = 12,
  height = 6.5,
  dpi = 320
)
