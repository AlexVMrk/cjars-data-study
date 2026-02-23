## Demographic Boundary Exploration Script
## Alexander Markel
## AM97354@wcupa.edu
## 3/27/2025
## GITHUB VERSION


# load data ---------------------------------------------------------------


# Move working directory up one level.
setwd("../")


#load long data
load('data/df_PA_long.rda')

#Unfiltered version, if exploring before 2009. 156 million rows
#load('data/df_PA_long_unfiltered.rda')



# Generate Subsets --------------------------------------------------------

# (save each, optinally)

males_W <- proc_df |> 
  filter((sex == 1 | sex == 0) & (race == 1 | race == 0)) |> 
  mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = "-")) |> 
  select(-c(sex, race, age_group, off_type, repeat_contact)) |> 
  select(fips, cohort_year, demo_group, everything())

#save(males_W, file = 'data/df_PA_long_MALE_W.rda')


males_B <- proc_df |> 
  filter((sex == 1 | sex == 0) & (race == 2 | race == 0)) |> 
  mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = "-")) |> 
  select(-c(sex, race, age_group, off_type, repeat_contact)) |> 
  select(fips, cohort_year, demo_group, everything())

#save(males_B, file = 'data/df_PA_long_MALE_B.rda')

females_W <- proc_df |> 
  filter((sex == 2 | sex == 0) & (race == 1 | race == 0) ) |> 
  mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = "-")) |> 
  select(-c(sex, race, age_group, off_type, repeat_contact)) |> 
  select(fips, cohort_year, demo_group, everything())

#save(females_W, file = 'data/df_PA_long_FEMALE_W.rda')

females_B <- proc_df |> 
  filter((sex == 2 | sex == 0) & (race == 2 | race == 0)) |> 
  mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = "-")) |> 
  select(-c(sex, race, age_group, off_type, repeat_contact)) |> 
  select(fips, cohort_year, demo_group, everything())
