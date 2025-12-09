# ==============================================================================
# MUSIC CONSUMPTION AND COVID-19 ANALYSIS
# ==============================================================================

# Load required libraries
library(tidyverse)
library(dplyr)
library(fixest)
library(fmlogit)
library(ggplot2)
library(patchwork)
library(openxlsx)

# Set working directory
setwd("/Users/choochoojc/Desktop/STAT204/Homeworks/Final Project")

# Load data
data <- read.csv("analysis_data_grok.csv")

# Subset of data with important covariates (identified by the authors)
data <- data %>% 
  select(Corona, Summer, MusicEducation, MusicAppreciation, ActiveListening,
    MainstreamMusic, MaritalStatus, Education, Occupation, Age, Children, 
    ln_Income, Pseudonym, Consumption_Live, Consumption_Rec, Consumption_Total,
    Spending_Live, Spending_Rec, Spending_Total)

# Distribution of music spending
# Making "long data" for plotting
long_df_spending <- data %>%
  select(Spending_Live, Spending_Rec, Spending_Total) %>%
  pivot_longer(
    cols = starts_with("Spend"), 
    names_to = "Market",           
    values_to = "Spending"       
  ) %>%
  mutate(Market = recode(Market,
                         "Spending_Live"  = "Live Market",
                         "Spending_Rec"   = "Recorded Market",
                         "Spending_Total" = "Total Spending"))

palette_markets <- c(
  "Live Market"      = "#0072B2",  # blue
  "Recorded Market"  = "#009E73",  # green
  "Total Spending"   = "#E69F00"   # orange
)
spending <- ggplot(long_df_spending, aes(x = Spending, fill = Market, color = Market)) +
  facet_wrap(~ Market) + 
  geom_density(alpha = 0.45, linewidth = 1.1) +
  scale_fill_manual(values = palette_markets) +
  scale_color_manual(values = palette_markets) +
  labs(x = "Music Spending",
       y = "Density") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))
  
log_spending <- ggplot(long_df_spending, aes(x = log(Spending), fill = Market, color = Market)) +
  facet_wrap(~ Market) + 
  geom_density(alpha = 0.45, linewidth = 1.1) +
  scale_fill_manual(values = palette_markets) +
  scale_color_manual(values = palette_markets) +
  labs(x = "Log(Music Spending)",
       y = "Density") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))

spending_dist <- spending / log_spending + 
  plot_annotation(
    title = "Distribution of Music Spending (Raw vs Log Transformed)"
  ) &
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

ggsave("1_spending_distribution.png", spending_dist,
       width = 10, height = 5, dpi = 300)

# Distribution of music consumption
# Making "long data" for plotting
long_df_consumption <- data %>%
  select(Consumption_Live, Consumption_Rec, Consumption_Total) %>%
  pivot_longer(
    cols = starts_with("Consump"), 
    names_to = "Market",           
    values_to = "Consumption"       
  ) %>%
  mutate(Market = recode(Market,
                         "Consumption_Live"  = "Live Market",
                         "Consumption_Rec"   = "Recorded Market",
                         "Consumption_Total" = "Total Spending"))

palette_markets <- c(
  "Live Market"      = "#0072B2",  # blue
  "Recorded Market"  = "#009E73",  # green
  "Total Spending"   = "#E69F00"   # orange
)
consumption <- ggplot(long_df_consumption, aes(x = Consumption, fill = Market, color = Market)) +
  facet_wrap(~ Market) + 
  geom_density(alpha = 0.45, linewidth = 1.1) +
  scale_fill_manual(values = palette_markets) +
  scale_color_manual(values = palette_markets) +
  labs(x = "Music Consumption",
       y = "Density") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))
  
log_consumption <- ggplot(long_df_consumption, aes(x = log(Consumption), fill = Market, color = Market)) +
  facet_wrap(~ Market) + 
  geom_density(alpha = 0.45, linewidth = 1.1) +
  scale_fill_manual(values = palette_markets) +
  scale_color_manual(values = palette_markets) +
  labs(x = "Log(Music Consumption)",
       y = "Density") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))

consumption_dist <- consumption / log_consumption + 
  plot_annotation(
    title = "Distribution of Music Consumption (Raw vs Log Transformed)"
  ) &
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

ggsave("1_consumption_distribution.png", consumption_dist,
       width = 10, height = 5, dpi = 300)
