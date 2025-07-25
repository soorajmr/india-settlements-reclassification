# Copyright (c) 2025 Neha Maria Verghese
# Licensed under the MIT License. See LICENSE file in the project root for details.

# Data sources include NSS Employment and Unemployment Surveys
# and Periodic Labour Force Survey (PLFS) published by the Government of India.
# Original data is available from Government sources for academic and research
# purposes.

################################################################################
## The NSS EUS unit-level data was downloaded from 
## https://microdata.gov.in/NADA/index.php/catalog/127/study-description
## and PLFS unit-level data for the different rounds were 
## downloaded from https://microdata.gov.in/NADA/index.php/catalog/PLFS
################################################################################

library(dplyr)
library(haven)
library(tidyr)
library(sf)
library(readxl)
library(tidyverse)
library(Hmisc)
library(reshape2)
library(kableExtra)

# PLFS 2023-24
layout <- read_xlsx("data/PLFS_2023-24/Data_LayoutPLFS_2023-24.xlsx")  
layout <- layout[ 81:220,  ]
names(layout) <- layout[1,]
layout <- layout[-1,]

plfs_23 <- read_fwf("data/PLFS_2023-24/PERV1.TXT",
                    col_positions = fwf_widths(as.numeric(layout$`Field Length`)))

plfs_23 <- plfs_23 %>% 
  mutate(wt = ifelse(X137 == X136, (X138/100), X138/200)/4, 
         ps_source = ifelse( X32 < 81, "P", 
                             ifelse(X32 >= 81 & X35 == 1, 
                                    "S", "P")), 
         upss = ifelse(X32 < 81, X32, 
                       ifelse(X32 >= 81 & X35 == 1, X43, X32)       ), 
         new_nic = ifelse(X32 < 81, X33, 
                          ifelse(X32 >= 81 & 
                                   X35 == 1,X44, X33)), 
         new_nco = ifelse(X32 < 81, X34, 	
                          ifelse(X32 >= 81 & X35 ==1 , X45, X45       )),
         ss = ifelse(ps_source == "P", X41, 
                     X51),
         epl = ifelse(ps_source == "P", X40, 
                      X50), 
         jc = ifelse(ps_source == "P", X39, 
                     X49))

plfs_23 <- plfs_23 %>%
  mutate(
    NIC_num = as.numeric(new_nic),
    Industry_group = cut(NIC_num, breaks = breakpoints, labels = labels, include.lowest =TRUE))

plfs_23$new_nic2 <- substr(plfs_23$new_nic,1, 2)

# NSS Round 68
nss_b5_1 <- read_dta("data/Nss68_10/Block_5_1_Usual principal activity particulars of household members.dta")
nss_b5_2 <- read_dta("data/Nss68_10/Block_5_2_Usual subsidiary economic activity particulars of household members.dta")
nss_b5_4 <- read_dta("data/Nss68_10/Block_4_Demographic particulars of household members.dta")

nss_b5_2 <-  nss_b5_2 %>%
  rename(District_code = District_Code) %>%
  mutate(unique_id = paste0(HHID, Person_Serial_No)) %>%
  select(unique_id, Usual_SubsidiaryActivity_NIC2004, Usual_Subsidiary_Activity_Status )

nss_b5_1 <- nss_b5_1 %>%
  mutate(unique_id = paste0(HHID, Person_Serial_No))

nss_b5_4 <- nss_b5_4 %>%
  mutate(unique_id = paste0(HHID, Person_Serial_No)) %>%
  select(unique_id, Sex)

nss_b5 <- nss_b5_1 %>%
  left_join(nss_b5_2, by = "unique_id") %>%
  mutate(
    upss = 
      ifelse(Usual_Principal_Activity_Status < 81,
             Usual_Principal_Activity_Status, 
             ifelse(Usual_Principal_Activity_Status >= 81 & 
                      Whether_in_Subsidiary_Activity == 1, 
                    Usual_Subsidiary_Activity_Status,
                    Usual_Principal_Activity_Status )),
    NIC = 
      ifelse(Usual_Principal_Activity_Status < 81,
             Usual_Principal_Activity_NIC2008, 
             ifelse(Usual_Principal_Activity_Status >= 81 & 
                      Whether_in_Subsidiary_Activity == 1,
                    Usual_SubsidiaryActivity_NIC2004,
                    Usual_Principal_Activity_NIC2008 ))
  )

nss_b5 <- nss_b5 %>%
  left_join(nss_b5_4, by = "unique_id")

nss_b5$new_nic2 <- substr(nss_b5$NIC, 1, 2)

# share of male workers in agriculture 

eus_2011 <- nss_b5 %>% 
  filter(upss %in% c("11", "12", "21", "31", "41", "51") & Sex == 1) %>%
  group_by(State) %>%
  summarise(
    share = 
      100 * sum(ifelse(new_nic2 == "01", Multiplier_comb, 0), na.rm = TRUE)/
              sum(Multiplier_comb, na.rm = TRUE)
  )

eus_2011 <- eus_2011 %>%
  mutate(
    State =
      case_when(
        State == "01" ~ "Jammu & Kashmir", State == "02" ~ "Himachal Pradesh", 
        State == "03" ~ "Punjab", State == "04" ~ "Chandigarh", 
        State == "05" ~ "Uttarakhand", State == "06" ~ "Haryana", 
        State == "07" ~ "Delhi", State == "08" ~ "Rajasthan", 
        State == "09" ~ "Uttar Pradesh", State == "10" ~ "Bihar", 
        State == "11" ~ "Sikkim", State == "12" ~ "Arunachal Pradesh", 
        State == "13" ~ "Nagaland", State == "14" ~ "Manipur", 
        State == "15" ~ "Mizoram", State == "16" ~ "Tripura", State == "17" ~ "Meghalaya", 
        State == "18" ~ "Assam", State == "19" ~ "West Bengal", 
        State == "20" ~ "Jharkhand", State == "21" ~ "Odisha", 
        State == "22" ~ "Chhattisgarh", State == "23" ~ "Madhya Pradesh", 
        State == "24" ~ "Gujarat", State == "25" ~ "Daman & Diu", 
        State == "26" ~ "D & N. Haveli",
        State == "27" ~ "Maharashtra", State == "28" ~ "Andhra Pradesh", 
        State == "29" ~ "Karnataka", State == "30" ~ "Goa", 
        State == "31" ~ "Lakshadweep", State == "32" ~ "Kerala", 
        State == "33" ~ "Tamilnadu", State == "34" ~ "Puduchery", 
        State == "35" ~ "Andaman & N. Island", State == "36" ~ "Telangana", 
        State == "37" ~ "Ladakh", 
        TRUE ~ NA_character_
      )
  )

plfs_2023 <- plfs_23 %>%
  filter(upss %in% c(11:51) & X19 == 1) %>%
  group_by(X6) %>%
  summarise(
    share = 
      100 * sum(ifelse(new_nic2 == "01", wt, 0), na.rm = TRUE) / sum(wt, na.rm = TRUE)
  )

plfs_2023 <- plfs_2023 %>% 
  mutate(
    X6 = case_when(
      X6 == "01" ~ "Jammu & Kashmir", X6 == "02" ~ "Himachal Pradesh", X6 == "03" ~ "Punjab", 
      X6 == "04" ~ "Chandigarh", X6 == "05" ~ "Uttarakhand", X6 == "06" ~ "Haryana", 
      X6 == "07" ~ "Delhi", X6 == "08" ~ "Rajasthan", X6 == "09" ~ "Uttar Pradesh", 
      X6 == "10" ~ "Bihar", X6 == "11" ~ "Sikkim", X6 == "12" ~ "Arunachal Pradesh", 
      X6 == "13" ~ "Nagaland", X6 == "14" ~ "Manipur", X6 == "15" ~ "Mizoram", 
      X6 == "16" ~ "Tripura", X6 == "17" ~ "Meghalaya", X6 == "18" ~ "Assam", 
      X6 == "19" ~ "West Bengal", X6 == "20" ~ "Jharkhand", X6 == "21" ~ "Odisha", 
      X6 == "22" ~ "Chhattisgarh", X6 == "23" ~ "Madhya Pradesh", X6 == "24" ~ "Gujarat", 
      X6 == "25" ~ "Daman & Diu", 
      X6 == "26" ~ "D & N. Haveli", X6 == "27" ~ "Maharashtra", 
      X6 == "28" ~ "Andhra Pradesh", X6 == "29" ~ "Karnataka", X6 == "30" ~ "Goa", 
      X6 == "31" ~ "Lakshadweep", X6 == "32" ~ "Kerala", X6 == "33" ~ "Tamilnadu", 
      X6 == "34" ~ "Puduchery", X6 == "35" ~ "Andaman & N. Island", X6 == "36" ~ "Telangana", 
      X6 == "37" ~ "Ladakh", 
      TRUE ~ NA_character_
    )
  ) 

states_agri <- full_join(eus_2011, plfs_2023, by = c("State" = "X6"))  %>% 
               rename(`2011` = share.x, `2023` = share.y ) %>%
               melt(states_agri, id = "State") %>%
               mutate(variable = factor(variable, levels = c("2011", "2023"))


write.csv(states_agri, "data/males_agriculture_shares.csv")

# urbanisation by region
eus_2011 <- nss_b5 %>% 
  filter(upss %in% c("11", "12", "21", "31", "41", "51") & Sex == 1) %>%
  summarise(share = 
              100*sum(ifelse(new_nic2 == "01", Multiplier_comb, 0), 
                      na.rm = TRUE)/sum(Multiplier_comb, na.rm = TRUE))

nss_b5 <- nss_b5 %>%
  mutate(
    region = case_when(
      State %in% c("Punjab", "Haryana" ,"Rajasthan","Uttar Pradesh") ~ "North",
      State %in% c("Ladakh","Himachal Pradesh","Uttarakhand", 
                   "Jammu & Kashmir","Sikkim" ,"Arunachal Pradesh" ,  
                   "Nagaland", "Mizoram" ,"Tripura", "Meghalaya" ,
                   "Assam" , "Manipur") ~  "Special Category States",
      State %in% c( "Chhattisgarh" , "Madhya Pradesh", "Jharkhand" ) ~ "Central",
      State %in% c("Odisha","Bihar", "West Bengal" ) ~"East", 
      State %in% c("Goa" ,"Gujarat" ,  "Maharashtra") ~ "West", 
      State %in% c("Andhra Pradesh", "Karnataka","Kerala", "Tamilnadu",  "Telangana") ~
        "South",
      State %in% c("Puduchery","Andaman & N. Island", "Delhi", 
                   "Lakshadweep", "Chandigarh","Daman & Diu", "D & N. Haveli" ) ~
        "UTs"
    )
  ) 

eus_2011 <- nss_b5 %>% 
  filter(upss %in% c("11", "12", "21", "31", "41", "51") & Sex == 1
  ) %>%
  group_by(region) %>%
  summarise(
    share = 
      100 * sum(ifelse(new_nic2 == "01", Multiplier_comb, 0), 
                na.rm = TRUE)/sum(Multiplier_comb, na.rm = TRUE)
  )

plfs_23 <- plfs_23 %>% mutate(
  region = case_when(
    X6 %in% c("Punjab", "Haryana" ,"Uttar Pradesh","Rajasthan") ~ "North",
    X6 %in% c("Ladakh","Himachal Pradesh","Uttarakhand", 
              "Jammu & Kashmir","Sikkim" ,"Arunachal Pradesh" ,  
              "Nagaland", "Mizoram" ,"Tripura", "Meghalaya" ,
              "Assam" , "Manipur") ~  "Special Category States",
    X6 %in% c( "Chhattisgarh" , "Madhya Pradesh", "Jharkhand" ) ~ "Central",
    X6 %in% c("Odisha","Bihar", "West Bengal" ) ~"East", 
    X6 %in% c("Goa" ,"Gujarat" ,  "Maharashtra") ~ "West", 
    X6 %in% c("Andhra Pradesh", "Karnataka","Kerala", "Tamilnadu",  "Telangana") ~
      "South",
    X6 %in% c("Puduchery","Andaman & N. Island", "Delhi", 
              "Lakshadweep", "Chandigarh","Daman & Diu", "D & N. Haveli" ) ~
      "UTs"
  )
) 

plfs_2023 <- plfs_23 %>%
  filter(upss %in% c(11:51) & X19 == 1) %>%
  group_by(region) %>%
  summarise(
    share = 
      100 * sum(ifelse(new_nic2 == "01", wt, 0), 
                na.rm = TRUE)/sum(wt, na.rm = TRUE)
  )

states_agri <- full_join(eus_2011, plfs_2023, by = c("region"))  %>%
               rename(`2011` = share.x, `2023` = share.y )

write.csv(states_agri, "data/males_agriculture_shares_region_lvl.csv")
