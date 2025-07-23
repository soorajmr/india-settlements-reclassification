# Copyright (c) 2025 Sooraj Raveendran
# Licensed under the MIT License. See LICENSE file in the project root for details.

# Data sources include NSS Employment and Unemployment Surveys
# and Periodic Labour Force Survey (PLFS) published by the Government of India.
# Original data is available from Government sources for academic and research
# purposes.

library(tidyverse)
library(broom)

data <- read_csv("data/males_agriculture_shares_2011_2023.csv")

data_long <- data %>%
  pivot_longer(cols = starts_with("X") | matches("^\\d{4}$"), 
               names_to = "year", 
               values_to = "agri_share") %>%
  mutate(
    year = as.numeric(gsub("X", "", year)),
    non_agri_share = 100 - agri_share
  ) %>%
  filter(!is.na(non_agri_share))

fit_and_predict <- function(state_data) {
  model <- lm(non_agri_share ~ year, data = state_data %>% filter(!(year %in% c(2019, 2020, 2022))))
  pred_years <- data.frame(year = c(2025, 2026))
  predictions <- predict(model, pred_years, interval = "prediction")
  result <- data.frame(
    year = pred_years$year,
    predicted_non_agri_share = predictions[,1],
    lower_bound = predictions[,2],
    upper_bound = predictions[,3]
  )
  
  model_stats <- glance(model)
  result$r_squared <- model_stats$r.squared
  result$p_value <- model_stats$p.value
  
  return(result)
}

projections <- data_long %>%
  group_by(State) %>%
  do(fit_and_predict(.)) %>%
  ungroup()

combined_data <- data_long %>% 
  select(State, year, non_agri_share) %>% 
  bind_rows(projections %>% select(State, year, non_agri_share = predicted_non_agri_share))
  
combined_data %>% 
  pivot_wider(names_from = year, values_from = non_agri_share) %>% 
  mutate(
    multiplier = 1 + (`2026` - `2011`) / `2011`
  ) %>% 
  write_csv('data/non_agri_wf_projections.csv')

