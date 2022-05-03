################
#
# Data Cleaning and Preparation
#
################

library(tidyverse)
library(haven)
library(lavaan)
library(psych)
library(GPArotation)
library(naniar)
library(gtsummary)

dataset <- readxl::read_excel("data/Athlete_Non-Athlete MH Survey - ALL DATA.xlsx", 
                              sheet = "athlete_fin") %>% 
  slice(-1) %>%   
  replace_with_na_all(condition = ~.x == "999")%>% 
  janitor::clean_names() %>%   mutate(
    fruit_veg = case_when(
      fruit_veg == 1 ~ 1,
      fruit_veg == 2 ~ 0
    )
  )

dataset_no_na <- dataset %>% 
  select(-c("shield_dts", "date")) %>% 
  na.omit()

athletes <- dataset %>% filter(athlete_yn == "1") %>% select(-c("shield_dts", "date"))
athletes %>% colnames()

athletes_no_na <- athletes %>% na.omit()

non_athletes <- dataset %>% filter(athlete_yn == "2")

athletes <-
  athletes %>% 
  mutate(
    weeks_cat = weeks_sd %>% as.factor(),
    bubble_cat = bub_size %>% as.numeric(),
    play_hrs = play_hrs %>% as.numeric(),
    play_hrs_cat = case_when(
      play_hrs < 5 ~ "less than 5 hrs",
      5 <= play_hrs & play_hrs < 10 ~ "between 5 and 10 hrs",
      10 <= play_hrs & play_hrs < 15 ~ "between 10 and 15 hrs",
      15 <= play_hrs & play_hrs < 20 ~ "between 15 and 20 hrs",
      20 <= play_hrs & play_hrs < 25 ~ "between 20 and 25 hrs",
      25 <= play_hrs & play_hrs < 30 ~ "between 25 and 30 hrs",
      30 <= play_hrs ~ "30 or more hours"
    )
  )