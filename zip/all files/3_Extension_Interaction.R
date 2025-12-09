# ==============================================================================
# MUSIC CONSUMPTION AND COVID-19 ANALYSIS - HETEROGENEOUS EFFECTS
# Moderators: Active Listening and Mainstream Music Taste
# ==============================================================================

# ------------------------------------------------------------------------------
# LOAD LIBRARIES
# ------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(fixest)
library(ggplot2)
library(patchwork)

# ------------------------------------------------------------------------------
# SET WORKING DIRECTORY AND LOAD DATA
# ------------------------------------------------------------------------------
setwd("/Users/choochoojc/Desktop/STAT204/Homeworks/Final Project")

analysis_data_grok <- read.csv("analysis_data_grok.csv")

# ==============================================================================
# PART 1: PREPARE BASE SAMPLES
# ==============================================================================

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

# ------------------------------------------------------------------------------
# Define Outcome Ordering for Plots
# ------------------------------------------------------------------------------
outcome_levels <- c(
  "Total Consumption", "Total Spending",
  "Recorded Consumption", "Recorded Spending",
  "Live Consumption", "Live Spending"
)

# ==============================================================================
# PART 2: HELPER FUNCTIONS
# ==============================================================================

# ------------------------------------------------------------------------------
# Function: Calculate HP Percent Effects for Interaction Models
# ------------------------------------------------------------------------------
# Computes Halvorsen-Palmquist (HP) percent effects for models with interactions:
#   log(Y+1) = ... + β1 * Corona + β3 * (Corona:Moderator)
#   Group 0 (moderator = 0): effect = β1
#   Group 1 (moderator = 1): effect = β1 + β3
#   HP = exp(effect) - 1
# ------------------------------------------------------------------------------
hp_from_model_generic <- function(model, outcome_name,
                                  mod_name,
                                  group_labels = c("Group 0", "Group 1")) {
  V  <- vcov(model)
  b1 <- coef(model)["Corona"]
  b3 <- coef(model)[paste0("Corona:", mod_name)]
  
  # Group 0 (moderator = 0): Corona main effect
  se1    <- sqrt(V["Corona", "Corona"])
  ci1log <- b1 + c(-1.96, 1.96) * se1
  
  # Group 1 (moderator = 1): Corona + interaction
  var12  <- V["Corona", "Corona"] +
    V[paste0("Corona:", mod_name), paste0("Corona:", mod_name)] +
    2 * V["Corona", paste0("Corona:", mod_name)]
  se12   <- sqrt(var12)
  ci2log <- (b1 + b3) + c(-1.96, 1.96) * se12
  
  tibble(
    Outcome  = outcome_name,
    Group    = group_labels,
    Effect   = c(exp(b1) - 1, exp(b1 + b3) - 1),
    CI_lower = c(exp(ci1log[1]) - 1, exp(ci2log[1]) - 1),
    CI_upper = c(exp(ci1log[2]) - 1, exp(ci2log[2]) - 1)
  )
}

# ------------------------------------------------------------------------------
# Function: Extract Significance Stars from Interaction Term
# ------------------------------------------------------------------------------
stars_from_models <- function(models_list, coef_name) {
  pvals <- sapply(models_list, function(m) {
    summary(m)$coeftable[coef_name, "Pr(>|t|)"]
  })
  dplyr::case_when(
    pvals <= 0.001 ~ "***",
    pvals <= 0.01  ~ "**",
    pvals <= 0.05  ~ "*",
    TRUE           ~ ""
  )
}

# ==============================================================================
# PART 3: ACTIVE LISTENING MODERATOR ANALYSIS
# ==============================================================================

cat("\n=== ACTIVE LISTENING MODERATOR ANALYSIS ===\n")

# ------------------------------------------------------------------------------
# Create Active Listening Groups (Median Split)
# ------------------------------------------------------------------------------
al_cut <- median(analysis_data_grok$ActiveListening, na.rm = TRUE)
cat("Active Listening median cutoff:", al_cut, "\n")

df_cons_AL <- df_cons %>%
  mutate(HighActive = if_else(ActiveListening >= al_cut, 1, 0))

df_spend_AL <- df_spend %>%
  mutate(HighActive = if_else(ActiveListening >= al_cut, 1, 0))

# ------------------------------------------------------------------------------
# Model 1: Total Consumption by Active Listening
# ------------------------------------------------------------------------------
fe_cons_total_AL <- feols(
  ln_Consumption_Total ~ Corona * HighActive + Summer +
    MusicEducation + MusicAppreciation + MainstreamMusic +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_cons_AL,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model 2: Total Spending by Active Listening
# ------------------------------------------------------------------------------
fe_spend_total_AL <- feols(
  ln_Spending_Total ~ Corona * HighActive + Summer +
    MusicEducation + MusicAppreciation + MainstreamMusic +
    PurchaseReason_Atmosphere + PurchaseReason_Flexibility +
    PurchaseReason_Habit + PurchaseReason_SoundQuality +
    PurchaseReason_Mobility + PurchaseReason_Other +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_spend_AL,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model 3: Recorded Consumption by Active Listening
# ------------------------------------------------------------------------------
fe_cons_rec_AL <- feols(
  ln_Consumption_Rec ~ Corona * HighActive + Summer +
    MusicEducation + MusicAppreciation + MainstreamMusic +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_cons_AL,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model 4: Recorded Spending by Active Listening
# ------------------------------------------------------------------------------
fe_spend_rec_AL <- feols(
  ln_Spending_Rec ~ Corona * HighActive + Summer +
    MusicEducation + MusicAppreciation + MainstreamMusic +
    PurchaseReason_Atmosphere + PurchaseReason_Flexibility +
    PurchaseReason_Habit + PurchaseReason_SoundQuality +
    PurchaseReason_Mobility + PurchaseReason_Other +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_spend_AL,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model 5: Live Consumption by Active Listening
# ------------------------------------------------------------------------------
fe_cons_live_AL <- feols(
  ln_Consumption_Live ~ Corona * HighActive + Summer +
    MusicEducation + MusicAppreciation + MainstreamMusic +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_cons_AL,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model 6: Live Spending by Active Listening
# ------------------------------------------------------------------------------
fe_spend_live_AL <- feols(
  ln_Spending_Live ~ Corona * HighActive + Summer +
    MusicEducation + MusicAppreciation + MainstreamMusic +
    PurchaseReason_Atmosphere + PurchaseReason_Flexibility +
    PurchaseReason_Habit + PurchaseReason_SoundQuality +
    PurchaseReason_Mobility + PurchaseReason_Other + PurchaseReason_None +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_spend_AL,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Collect Active Listening Models
# ------------------------------------------------------------------------------
models_AL <- list(
  "Total Consumption"    = fe_cons_total_AL,
  "Total Spending"       = fe_spend_total_AL,
  "Recorded Consumption" = fe_cons_rec_AL,
  "Recorded Spending"    = fe_spend_rec_AL,
  "Live Consumption"     = fe_cons_live_AL,
  "Live Spending"        = fe_spend_live_AL
)

# ------------------------------------------------------------------------------
# Calculate HP Effects for Active Listening
# ------------------------------------------------------------------------------
hp_AL <- bind_rows(
  hp_from_model_generic(
    fe_cons_total_AL, "Total Consumption",
    mod_name = "HighActive",
    group_labels = c("Lower active listening", "Higher active listening")
  ),
  hp_from_model_generic(
    fe_spend_total_AL, "Total Spending",
    mod_name = "HighActive",
    group_labels = c("Lower active listening", "Higher active listening")
  ),
  hp_from_model_generic(
    fe_cons_rec_AL, "Recorded Consumption",
    mod_name = "HighActive",
    group_labels = c("Lower active listening", "Higher active listening")
  ),
  hp_from_model_generic(
    fe_spend_rec_AL, "Recorded Spending",
    mod_name = "HighActive",
    group_labels = c("Lower active listening", "Higher active listening")
  ),
  hp_from_model_generic(
    fe_cons_live_AL, "Live Consumption",
    mod_name = "HighActive",
    group_labels = c("Lower active listening", "Higher active listening")
  ),
  hp_from_model_generic(
    fe_spend_live_AL, "Live Spending",
    mod_name = "HighActive",
    group_labels = c("Lower active listening", "Higher active listening")
  )
)

hp_AL$Outcome <- factor(hp_AL$Outcome, levels = outcome_levels)

# ------------------------------------------------------------------------------
# Add Significance Stars
# ------------------------------------------------------------------------------
stars_AL <- stars_from_models(models_AL, "Corona:HighActive")

hp_AL <- hp_AL %>%
  group_by(Outcome) %>%
  mutate(
    Stars = if_else(
      Group == "Higher active listening",
      stars_AL[unique(Outcome)],
      ""
    )
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Build right-side magnitude labels (Active Listening)
#   Text printed between 0.0 and 0.2 on x-axis
# ------------------------------------------------------------------------------
labels_AL <- hp_AL %>%
  group_by(Outcome) %>%
  summarise(
    label = paste0(
      "Lower = ", sprintf("%.2f", Effect[Group == "Lower active listening"]),
      ", Higher = ", sprintf("%.2f", Effect[Group == "Higher active listening"])
    ),
    .groups = "drop"
  )

text_x_AL <- 0.05   # fixed location inside [0, 0.2]

# ------------------------------------------------------------------------------
# Plot: Active Listening Moderator Effects
# ------------------------------------------------------------------------------
p_fe_AL <- ggplot(
  hp_AL,
  aes(x = Effect, y = Outcome, color = Group)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbarh(
    aes(xmin = CI_lower, xmax = CI_upper),
    height = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  geom_text(
    aes(label = Stars),
    nudge_x = 0.02,
    size = 5,
    show.legend = FALSE
  ) +
  geom_text(
    data = labels_AL,
    aes(x = text_x_AL, y = Outcome, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3
  ) +
  scale_x_continuous(limits = c(min(hp_AL$CI_lower), 0.22)) +
  labs(
    x = "HP percent effect of COVID (exp(β) − 1)",
    y = NULL,
    title = "COVID impact by listening engagement",
    subtitle = "Stars indicate significance of interaction Corona × HighActive",
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom",
    legend.margin   = margin(t = 3),
    plot.margin     = margin(5.5, 10, 5.5, 5.5)
  )

print(p_fe_AL)

# ==============================================================================
# PART 4: MAINSTREAM MUSIC TASTE MODERATOR ANALYSIS
# ==============================================================================

cat("\n=== MAINSTREAM MUSIC TASTE MODERATOR ANALYSIS ===\n")

# ------------------------------------------------------------------------------
# Create Mainstream Music Groups (Median Split)
# ------------------------------------------------------------------------------
mm_cut <- median(analysis_data_grok$MainstreamMusic, na.rm = TRUE)
cat("Mainstream Music median cutoff:", mm_cut, "\n")

df_cons_MM <- df_cons %>%
  mutate(HighMainstream = if_else(MainstreamMusic >= mm_cut, 1, 0))

df_spend_MM <- df_spend %>%
  mutate(HighMainstream = if_else(MainstreamMusic >= mm_cut, 1, 0))

# ------------------------------------------------------------------------------
# Model 1: Total Consumption by Mainstream Taste
# ------------------------------------------------------------------------------
fe_cons_total_MM <- feols(
  ln_Consumption_Total ~ Corona * HighMainstream + Summer +
    MusicEducation + MusicAppreciation + ActiveListening +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_cons_MM,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model 2: Total Spending by Mainstream Taste
# ------------------------------------------------------------------------------
fe_spend_total_MM <- feols(
  ln_Spending_Total ~ Corona * HighMainstream + Summer +
    MusicEducation + MusicAppreciation + ActiveListening +
    PurchaseReason_Atmosphere + PurchaseReason_Flexibility +
    PurchaseReason_Habit + PurchaseReason_SoundQuality +
    PurchaseReason_Mobility + PurchaseReason_Other +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_spend_MM,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model 3: Recorded Consumption by Mainstream Taste
# ------------------------------------------------------------------------------
fe_cons_rec_MM <- feols(
  ln_Consumption_Rec ~ Corona * HighMainstream + Summer +
    MusicEducation + MusicAppreciation + ActiveListening +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_cons_MM,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model 4: Recorded Spending by Mainstream Taste
# ------------------------------------------------------------------------------
fe_spend_rec_MM <- feols(
  ln_Spending_Rec ~ Corona * HighMainstream + Summer +
    MusicEducation + MusicAppreciation + ActiveListening +
    PurchaseReason_Atmosphere + PurchaseReason_Flexibility +
    PurchaseReason_Habit + PurchaseReason_SoundQuality +
    PurchaseReason_Mobility + PurchaseReason_Other +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_spend_MM,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model 5: Live Consumption by Mainstream Taste
# ------------------------------------------------------------------------------
fe_cons_live_MM <- feols(
  ln_Consumption_Live ~ Corona * HighMainstream + Summer +
    MusicEducation + MusicAppreciation + ActiveListening +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_cons_MM,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Model 6: Live Spending by Mainstream Taste
# ------------------------------------------------------------------------------
fe_spend_live_MM <- feols(
  ln_Spending_Live ~ Corona * HighMainstream + Summer +
    MusicEducation + MusicAppreciation + ActiveListening +
    PurchaseReason_Atmosphere + PurchaseReason_Flexibility +
    PurchaseReason_Habit + PurchaseReason_SoundQuality +
    PurchaseReason_Mobility + PurchaseReason_Other + PurchaseReason_None +
    i(MaritalStatus, ref = 2) +
    i(Education, ref = 1) +
    i(Occupation, ref = 3) +
    Age + Children + ln_Income | 
    Pseudonym,
  data = df_spend_MM,
  vcov = ~ Pseudonym
)

# ------------------------------------------------------------------------------
# Collect Mainstream Music Models
# ------------------------------------------------------------------------------
models_MM <- list(
  "Total Consumption"    = fe_cons_total_MM,
  "Total Spending"       = fe_spend_total_MM,
  "Recorded Consumption" = fe_cons_rec_MM,
  "Recorded Spending"    = fe_spend_rec_MM,
  "Live Consumption"     = fe_cons_live_MM,
  "Live Spending"        = fe_spend_live_MM
)

# ------------------------------------------------------------------------------
# Calculate HP Effects for Mainstream Music
# ------------------------------------------------------------------------------
hp_MM <- bind_rows(
  hp_from_model_generic(
    fe_cons_total_MM, "Total Consumption",
    mod_name = "HighMainstream",
    group_labels = c("Less mainstream taste", "More mainstream taste")
  ),
  hp_from_model_generic(
    fe_spend_total_MM, "Total Spending",
    mod_name = "HighMainstream",
    group_labels = c("Less mainstream taste", "More mainstream taste")
  ),
  hp_from_model_generic(
    fe_cons_rec_MM, "Recorded Consumption",
    mod_name = "HighMainstream",
    group_labels = c("Less mainstream taste", "More mainstream taste")
  ),
  hp_from_model_generic(
    fe_spend_rec_MM, "Recorded Spending",
    mod_name = "HighMainstream",
    group_labels = c("Less mainstream taste", "More mainstream taste")
  ),
  hp_from_model_generic(
    fe_cons_live_MM, "Live Consumption",
    mod_name = "HighMainstream",
    group_labels = c("Less mainstream taste", "More mainstream taste")
  ),
  hp_from_model_generic(
    fe_spend_live_MM, "Live Spending",
    mod_name = "HighMainstream",
    group_labels = c("Less mainstream taste", "More mainstream taste")
  )
)

hp_MM$Outcome <- factor(hp_MM$Outcome, levels = outcome_levels)

# ------------------------------------------------------------------------------
# Add Significance Stars
# ------------------------------------------------------------------------------
stars_MM <- stars_from_models(models_MM, "Corona:HighMainstream")

hp_MM <- hp_MM %>%
  group_by(Outcome) %>%
  mutate(
    Stars = if_else(
      Group == "More mainstream taste",
      stars_MM[unique(Outcome)],
      ""
    )
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Build right-side magnitude labels (Mainstream Taste)
# ------------------------------------------------------------------------------
labels_MM <- hp_MM %>%
  group_by(Outcome) %>%
  summarise(
    label = paste0(
      "Less = ", sprintf("%.2f", Effect[Group == "Less mainstream taste"]),
      ", More = ", sprintf("%.2f", Effect[Group == "More mainstream taste"])
    ),
    .groups = "drop"
  )

text_x_MM <- 0.05   # fixed location between 0 and 0.2

# ------------------------------------------------------------------------------
# Plot: Mainstream Music Moderator Effects
# ------------------------------------------------------------------------------
p_fe_MM <- ggplot(
  hp_MM,
  aes(x = Effect, y = Outcome, color = Group)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbarh(
    aes(xmin = CI_lower, xmax = CI_upper),
    height = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  geom_text(
    aes(label = Stars),
    nudge_x = 0.02,
    size = 5,
    show.legend = FALSE
  ) +
  geom_text(
    data = labels_MM,
    aes(x = text_x_MM, y = Outcome, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3
  ) +
  scale_x_continuous(limits = c(min(hp_MM$CI_lower), 0.22)) +
  labs(
    x = "HP percent effect of COVID (exp(β) − 1)",
    y = NULL,
    title = "COVID impact by music taste",
    subtitle = "Stars indicate significance of interaction Corona × HighMainstream",
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom",
    legend.margin   = margin(t = 3),
    plot.margin     = margin(5.5, 10, 5.5, 5.5)
  )

print(p_fe_MM)

# ==============================================================================
# PART 5: COMBINED VISUALIZATION
# ==============================================================================

combined_AL_MM <- p_fe_AL / p_fe_MM +
  plot_annotation(
    title = "Heterogeneous effects of COVID-19 on music consumption and spending",
    subtitle = "Top: Active listening moderator | Bottom: Mainstream taste moderator",
    theme = theme(
      plot.title    = element_text(hjust = 0, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0, size = 11)
    )
  )

print(combined_AL_MM)

# ==============================================================================
# PART 6: SAVE OUTPUTS
# ==============================================================================
ggsave(
  "4_interaction_combined.png",
  plot = combined_AL_MM,
  width = 14,
  height = 6.5,
  dpi = 320
)
