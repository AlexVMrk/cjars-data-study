## Estimation Grouping Exploration Script
## 3/27/2025


# load libraries ----------------------------------------------------------

library(tidyverse)  #wrangling 
library(kableExtra) #For beutiful tables

# Questions: --------------------------------------------------------------


## For a given year / year window (2014-2015), if we look at the 
## employment rate for all 67 counties, broken down by race,
## What % of the statistics will be influenced by EG?
## (Do for each event type)


# load data ---------------------------------------------------------------

# Move working directory up one level.
setwd("../")

# Get master dataset with appropriate flags.
load('data/master.rda')


#Pick a criminal record type
event_type_fe  <- "felony"
event_type_inc <- "incarceration"
event_type_mi  <- "misdemeanor"

#for a single window of data.
year   <- '2014'
year_2 <- '2015'

df_fe <- master |> 
  filter(event_type == event_type_fe) |>
  #filter(fips %in% c(urban_county, rural_county)) |>
  filter(years_post %in% c(1, 3, 5)) |> 
  filter(variable_name %in% c("any_w2", "w2_wages", "above_poverty", "hud")) |>
  mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = '-')) |> 
  filter(cohort_year == year | (cohort_year == year_2 & demo_group == '0-0-0-0-0')) |>
  select(-c(4:8)) |> 
  select(1:4, demo_group, 6:13) 

df_inc <- master |> 
  filter(event_type == event_type_inc) |> 
  #filter(fips %in% c(urban_county, rural_county)) |>
  filter(years_post %in% c(1, 3, 5)) |>
  filter(variable_name %in% c("any_w2", "w2_wages", "above_poverty", "hud")) |>
  mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = '-')) |> 
  filter(cohort_year == year | (cohort_year == year_2 & demo_group == '0-0-0-0-0')) |>
  select(-c(4:8)) |> 
  select(1:4, demo_group, 6:13) 


df_mi <- master |> 
  filter(event_type == event_type_mi) |>
  #filter(fips %in% c(urban_county, rural_county)) |>
  filter(years_post %in% c(1, 3, 5)) |>
  filter(variable_name %in% c("any_w2", "w2_wages", "above_poverty", "hud")) |>
  mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = '-')) |> 
  filter(cohort_year == year | (cohort_year == year_2 & demo_group == '0-0-0-0-0')) |>select(-c(4:8)) |> 
   select(1:4, demo_group, 6:13) 





table_1 <- df_fe |> 
    filter(years_post == 1) |> 
    mutate(value_decomposed = ifelse(
      (!is_count),
      value * lead(value, 1),
      value
    ))  |>
    mutate(
      value_employed = ifelse(
        variable_name == 'w2_wages' & !is_count,
        value_decomposed / (lead(value_decomposed, 1) * lag(value, 4)),
        ifelse(variable_name == 'w2_wages' & is_count,
               value_decomposed * lag(value, 5),
               NA
        )
      )
    ) |>
    group_by(demo_group) |> 
    summarise(
      Rate = ifelse(first(variable_name == 'w2_wages'),
                    sum(value_decomposed[seq(1, n(), by = 2)], na.rm = T) / sum(value_employed[seq(0, n(), by = 2)], na.rm = T),
                    sum(value_decomposed[seq(1, n(), by = 2)], na.rm = T) / sum(value_decomposed[seq(2, n(), by = 2)], na.rm = T) 
      ),
      `Thousands of Individuals` = ifelse(first(variable_name == 'w2_wages'),
                                          sum(value_employed[seq(2, n(), by = 2)], na.rm = T) / 1000,
                                          sum(value_decomposed[seq(2, n(), by = 2)], na.rm = T) / 1000),
    	N_statistics = sum(!is.na(value)) / 2,
    	N_EG_stats = sum(flag_EG, na.rm = T) / 2,
    	percentage = paste(round((sum(flag_EG, na.rm = T) / 2) / ( sum(!is.na(value)) / 2), 2) * 100, "%")
    )  #|> 
    pivot_longer(
      cols = c(4:5),
      names_to = 'Statistic'
    ) |> 
    pivot_wider(
      names_from = variable_name
    ) |> 
    mutate(across(where(is.numeric), ~round(., 3)),
           County = ifelse(fips == '42029', 'Chester', 'Butler')) |> 
    ungroup() |> 
    select(County, 3:7)
  
  
    
    
    table_1$prop <-  table_1$EG_stats / table_1$n 
    
    table_1$n_zero <- str_count(table_1$demo_group, "0")
    
    
    plot <- table_1 |> 
    	group_by(n_zero) |> 
    	summarise(mean = mean(prop))

    ggplot(plot, aes(x=  n_zero, y = mean))   + 
    	geom_bar(stat = 'identity')
  