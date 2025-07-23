# Copyright (c) 2025 Sooraj Raveendran
# Licensed under the MIT License. See LICENSE file in the project root for details.

# Data sources include Census of India, NSS Employment and Unemployment Surveys
# and Periodic Labour Force Survey (PLFS) and Local Government Directory
# published by the Government of India. Original data is either in public domain
# or available from Government sources for academic and research purposes.

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(sampling)

state_names <- c(
  "Jammu and Kashmir",
  "Himachal Pradesh", 
  "Punjab",
  "Chandigarh",
  "Uttarakhand",
  "Haryana",
  "NCT of Delhi",
  "Rajasthan",
  "Uttar Pradesh",
  "Bihar",
  "Sikkim",
  "Arunachal Pradesh",
  "Nagaland",
  "Manipur",
  "Mizoram",
  "Tripura",
  "Meghalaya",
  "Assam",
  "West Bengal",
  "Jharkhand",
  "Odisha",
  "Chhattisgarh",
  "Madhya Pradesh",
  "Gujarat",
  "Daman & Diu",
  "Dadra & Nagar Haveli",
  "Maharashtra",
  "Andhra Pradesh",
  "Karnataka",
  "Goa",
  "Lakshadweep",
  "Kerala",
  "Tamil Nadu",
  "Puducherry",
  "Andaman & Nicobar Islands",
  "Telangana"
)

## Settlement-level Census 2011 data from Primary Census Abstract and 
## DCHB Town and Village directory datasets
settlements <- read_csv('data/settlements_2011.csv')

## State-wise share of male non-agricultural workers, estimated from 
## NSS EUS and PLFS rounds
wf_transition <- read_csv('data/non_agri_wf_projections.csv') %>% 
  select(State, wf_inflation = multiplier)

## List of 2011 Census towns and villages that went into Telangana
## after the bifurcation of Andhra Pradesh
telangana_settlements <- read_csv('data/telangana_settlements_2011.csv')

settlements <- settlements %>% 
  mutate(
    State = if_else(settlement_code %in% telangana_settlements$settlement_code,
                    "36", State)
  )

st_summary <- settlements %>% 
  mutate(State = as.numeric(State)) %>% 
  group_by(State) %>% 
  summarise(st_count = sum(if_else(status_2011 == "ST", 1, 0)),
            .groups = "drop")

## State-wise ULB count data collated from Local Government Directory (2025 June)
ulb_latest <- read_csv('data/lgd_ulb_data_summary.csv') %>% 
  mutate(
    State_Code = case_when(
      State_Code == 38 ~ 26,
      State_Code == 37 ~ 1,
      TRUE ~ State_Code
    )
  ) %>% 
  group_by(State_Code) %>% 
  summarise(ulb_count = sum(Count), .groups = "drop") %>% 
  rename(State = State_Code) 

ulb_latest <- st_summary %>% 
  left_join(ulb_latest, by = "State") %>% 
  mutate(
    additional_sts = if_else(ulb_count > st_count, ulb_count - st_count, 0),
    additional_sts = if_else(State == 26, 0, additional_sts)
  ) %>% 
  replace_na(list(additional_sts = 0))

## Based on the population projections for Indian states
## from the REPORT OF THE TECHNICAL GROUP ON POPULATION PROJECTIONS (2019)
state_pop_growth <- read_csv('data/state_population_growth_2011-26.csv') %>% 
  select(State, pop_growth)

settlements_proj <- settlements %>% 
  mutate(State = as.numeric(State)) %>% 
  inner_join(state_pop_growth, by = "State") %>% 
  inner_join(wf_transition, by = "State") %>% 
  mutate(
    pop_2026 = TOT_P * (1 + pop_growth / 100),
    non_agri_2026 = non_agri * wf_inflation,
    density = if_else(area > 0, TOT_P / area, 0),
    density_2026 = if_else(area > 0, pop_2026 / area, 0),
    status_2026 = case_when(
      status_2011 == "ST" ~ "ST_2011",
      status_2011 == "CT" ~ "CT_2011",
      pop_2026 >= 5000 & density_2026 >= 400 & 
        (non_agri >= 65 | non_agri_2026 >= 75) ~ "CT_2026",
      TRUE ~ "Village"
    )
  )

pps_sample_by_state <- function(data, state_name) {
  n_samples <- unique(data$additional_sts)
  
  if (nrow(data) < n_samples) {
    warning(paste("State", state_name, "has", nrow(data), "records but needs", n_samples, "samples"))
    return(data)
  }
  
  pik <- inclusionprobabilities(data$pop_2026, n_samples)
  sample_indices <- UPsystematic(pik)
  return(data[sample_indices == 1, ])
}

set.seed(100)
new_sts <- settlements_proj %>%
  inner_join(ulb_latest %>% select(State, additional_sts), by = "State") %>%
  filter(status_2026 %in% c("CT_2011", "CT_2026") &
           !(State %in% c(12, 13))) %>% 
  group_by(State) %>%
  group_modify(~ pps_sample_by_state(.x, .y$State)) %>%
  ungroup()

set.seed(100)
new_sts_additional <- settlements_proj %>%
  inner_join(ulb_latest %>% select(State, additional_sts), by = "State") %>%
  filter(State %in% c(12, 13) & pop_2026 > 2500) %>% 
  filter(status_2026 != "ST_2011") %>%  
  group_by(State) %>%
  group_modify(~ pps_sample_by_state(.x, .y$State)) %>%
  ungroup()

new_sts <- bind_rows(new_sts, new_sts_additional) %>% pull(settlement_code)

settlements_proj <- settlements_proj %>% 
  mutate(
    status_2026 = if_else(settlement_code %in% new_sts, "ST_2026", status_2026)
  )

state_names_df <- tibble(state_name = state_names) %>% 
  mutate(
    State = row_number(),
    state_name = case_when(
      State == 1 ~ "J&K and Ladakh",
      TRUE ~ state_name
    )
  )
  
state_summary_2011 <- settlements_proj %>% 
  inner_join(state_names_df, by = "State") %>% 
  group_by(State, state_name, status_2011) %>% 
  summarise(
    counts = n(),
    population_2011 = sum(TOT_P),
    .groups = "drop"
  ) %>% 
  write_csv('data/state_settlements_summary_2011.csv')

state_summary_2026 <- settlements_proj %>% 
  inner_join(state_names_df, by = "State") %>% 
  group_by(State, state_name, status_2026) %>% 
  summarise(
    counts = n(),
    population_2026 = round(sum(pop_2026), 0),
    .groups = "drop"
  ) %>% 
  write_csv('data/state_settlements_summary_2026.csv')

settlements_proj %>% 
  inner_join(state_names_df, by = "State") %>% 
  mutate(pop_2026 = round(pop_2026, 0)) %>% 
  select(
    state_code = State,
    state_name,
    settlement_code_2011 = settlement_code,
    settlement_name = Name,
    area_sqkm = area,
    population_2011 = TOT_P,
    male_non_agri_pct_2011 = non_agri,
    density_2011 = density,
    status_2011,
    pop_growth_pct = pop_growth,
    wf_inflation,
    population_2026 = pop_2026,
    male_non_agri_pct_2026 = non_agri_2026,
    density_2026,
    status_2026
  ) %>% 
  write_csv('data/settlements_reclassification_2011-26.csv')
