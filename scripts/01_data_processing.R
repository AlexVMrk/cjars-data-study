## Data Processing Script
## Alexander Markel 
## AM97354@wcupa.edu
## 3/20/2025
## GITHUB VERSION

# load libraries ----------------------------------------------------------
library(tidyverse)  #wrangling 
library(tidycensus) #FIPS IDs
library(gt)         #table stuff

# load data ---------------------------------------------------------------

#DOWNLOAD CJARS "COUNTY" DATA FROM THIS LINK: https://joe.cjars.org/data
#PLACE "county_R" folder in the  "data" folder. Data folder should be one level below the working directory
# I.e /.root/data/county_R/...


#check working directory
getwd()

# Move working directory up one level. 
# it should be at the project root. 
setwd("../")

#full JOE county-wise data-set !!!WARNING: LARGE!!!
load("data/county_R/county_data.rda") 

# FIPS code reference table. 
# This reference table is needed because the JOE data operates in FIPS IDs
# while the rural.pa.gov reference table operates in string values. We need to
# convert the strings to FIPS IDs prior to joining dummy to JOE data.
data(fips_codes) 

#urban / rural definitons: https://www.rural.pa.gov/data/rural-urban-definitions
df_rural_urban <- readxl::read_xlsx('data/rural_urban.xlsx') 


# create PA subset and pivot to long --------------------------------------

# Filter for PA
df_PA <- data |>  
  filter(str_starts(fips, "42"))

# Check county count: confirm 67 per year 
gt(df_PA |> 
  group_by('year' = cohort_year) |> 
  summarise(
    counties = length(
      unique(fips)
      )
    )
  )

# Add rural/urban dummy
fips_codes <- fips_codes |> 
  #select PA
  filter(state == "PA") |> 
  #paste state and county codes together
  mutate(fips = paste0(state_code, county_code)) |> 
  #select the fips code and the county name (string)
  select(fips, county) |> 
  #remove "County" from county name (string)
  mutate(county = sub(" County", "", county)) |>  
  #join rural/urban column into the data
  left_join(df_rural_urban, by = join_by('county' == 'County Name'))  |> 
  #mutate rural from string to dummy
  mutate(rural = ifelse(`Rural/ Urban` == "Rural", 1, 0)) |> 
  #select fips code and rural dummy
  select(fips, rural) 


# Add rural dummy column to df_PA
df_PA_dummy <- left_join(
  #join onto what: df_PA
  df_PA, 
  #join what: the dummy column we just created called fips_codes
  fips_codes,
  #join by what: join by fips IDs
  by = join_by('fips' == 'fips'))|>  
  #reorder the columns a bit
  select(
    fips, cohort_year, rural, everything()
    )

# based on the previously-made decisions, we can filter our data to start at 2009. 
# we can also remove parole and probation columns before pivoting the data.

rm(data, df_rural_urban, fips_codes) #free up memory

gc() #free up memory

colnames(df_PA_dummy)

# Determine which is the last column before Parole and Probation begin
# I.e WHICH is the FIRST index in the colnames list that matches the string 'par'?
col_stop <- which(grepl("par", colnames(df_PA_dummy), ignore.case = TRUE))[1] - 1

# Filter appropriately
df_PA_dummy_filtered <- df_PA_dummy |> 
  filter(cohort_year > 2008) |> 
  select(1:col_stop)

## BEGIN PIVOT ##

# Define identifier columns
identifier_cols <- c('fips', 
                     'cohort_year', 
                     'rural', 
                     'sex', 
                     'race', 
                     'age_group', 
                     'off_type', 
                     'repeat_contact')


# Pivot on all non-grouping/non-identifier cols 
long_df <- pivot_longer(df_PA_dummy_filtered, 
                        -identifier_cols,
                        names_to = 'variable_name')


# Process long data by de-multiplexing name column
proc_df <- long_df |> 
  # EXTRACT follow-up year from variable_name column
  mutate(years_post = case_when(
    grepl("_y1$", variable_name) ~ 1, #ending in _y1
    grepl("_y2$", variable_name) ~ 2, #ending in _y2
    grepl("_y3$", variable_name) ~ 3, #ending in _y3
    grepl("_y4$", variable_name) ~ 4, #ending in _y4
    grepl("_y5$", variable_name) ~ 5, #ending in _y5
    .default = 0)) |> 
  #REMOVE year suffix from variable_name
  mutate(variable_name = sub("_y[12345]$", "", variable_name)) |> 
  #EXTRACT measurement-type (Count or Rate) from variable_name column
  mutate(is_count = case_when(
    grepl("N_", variable_name) ~ TRUE,
    .default = FALSE
  )) |>
  #REMOVE measurement-type prefix from variable_name
  mutate(variable_name = sub("N_", "", variable_name)) |> 
  #EXTRACT event-type (fe, mi, inc, pro, parole) from variable_name column
  mutate(event_type = case_when(
    grepl("^fe_", variable_name) ~ 'felony',
    grepl("^mi_", variable_name) ~ 'misdemeanor',
    grepl("^inc_", variable_name) ~ 'incarceration',
    grepl("^pro_", variable_name) ~ 'probation',
    grepl("^par_", variable_name) ~ 'parole',
    grepl("^fe_", variable_name) ~ 'felony'
  )) |> 
  #REMOVE event-type prefix from variable_name column
  mutate(variable_name = sub("^[^_]*_", "", variable_name)) |> 
  select(1:8, 
         is_count, 
         event_type,
         variable_name, 
         years_post, 
         value)
  

# SAVE FILTERED LONG DATA WITH DUMMY VARIABLE
save(proc_df, file = 'data/df_PA_long.rda')

# LIKELY-SKIP----------------------------------------------------------------


#### We can repeat the same process without filtering by years.
#### !!!YOU CAN LIKELY SKIP THIS SECTION!!!. I only used it to examine the 
#### date boundaries of the dataset (which is why I needed the whole date-range)



# Determine which is the last column before Parole and Probation begin
# I.e WHICH is the FIRST index in the colnames list that matches the string 'par'?
col_stop <- which(grepl("par", colnames(df_PA_dummy), ignore.case = TRUE))[1] - 1

# Pivot on all non-grouping/non-identifier cols 
long_df <- pivot_longer(df_PA_dummy |> 
		select(1:col_stop), 
                        -identifier_cols,
                        names_to = 'variable_name')


# Process long data by de-multiplexing name column
proc_df <- long_df |> 
  # EXTRACT follow-up year from variable_name column
  mutate(years_post = case_when(
    grepl("_y1$", variable_name) ~ 1, #ending in _y1
    grepl("_y2$", variable_name) ~ 2, #ending in _y2
    grepl("_y3$", variable_name) ~ 3, #ending in _y3
    grepl("_y4$", variable_name) ~ 4, #ending in _y4
    grepl("_y5$", variable_name) ~ 5, #ending in _y5
    .default = 0)) |> 
  #REMOVE year suffix from variable_name
  mutate(variable_name = sub("_y[12345]$", "", variable_name)) |> 
  #EXTRACT measurement-type (Count or Rate) from variable_name column
  mutate(is_count = case_when(
    grepl("N_", variable_name) ~ TRUE,
    .default = FALSE
  )) |>
  #REMOVE measurement-type prefix from variable_name
  mutate(variable_name = sub("N_", "", variable_name)) |> 
  #EXTRACT event-type (fe, mi, inc, pro, parole) from variable_name column
  mutate(event_type = case_when(
    grepl("^fe_", variable_name) ~ 'felony',
    grepl("^mi_", variable_name) ~ 'misdemeanor',
    grepl("^inc_", variable_name) ~ 'incarceration',
    grepl("^pro_", variable_name) ~ 'probation',
    grepl("^par_", variable_name) ~ 'parole',
    grepl("^fe_", variable_name) ~ 'felony'
  )) |> 
  #REMOVE event-type prefix from variable_name column
  mutate(variable_name = sub("^[^_]*_", "", variable_name)) |> 
  select(1:8, 
         is_count, 
         event_type,
         variable_name, 
         years_post, 
         value)

# SAVE UN-FILTERED (ALL YEARS) LONG DATA WITH DUMMY VARIABLE
save(proc_df, file = 'data/df_PA_long_unfiltered.rda')

# Here we work on appending flag columns to the long data -----------------

#### Some Statistics are parts of County Groups and Estimation Groups
#### (See technical documentation), we want to append these flags (dummies)
#### to our long data inn case we need to denote these flags in any figure
#### that we produce. 

### To achieve this, we need to:
### 1: Pivot the flag data into long form (so it matches our long data)
### 2: de-multiplex the variable names as we did above. (We should have columns: "flag", "value")
### 2: Pivot the "flag" column into wide form (because it's not an identifier)
### 3: join these flag columns into the long data to create a master dataset.


# LOAD DATA
# This contains flags for County Groups and Estimation Groups (f_CG, f_EG)
load("data/county_R/county_supplementary.rda")
data_sup <- data
rm(data)

# This contains the county group ID of each county (we don't do any pivoting with this)
load("data/county_R/county_to_CG_xwalk.rda")
data_xwalk <- data
names(data_xwalk) <- c("fips", "CG_IDENTIFIER")
rm(data)

## BEGIN PIVOT ##

# Determine which is the last column before Parole and Probation begin
# I.e WHICH is the FIRST index in the colnames list that matches the string 'par'?
col_stop <- which(grepl("par", colnames(data_sup), ignore.case = TRUE))[1] - 1


# Define identifier columns
identifier_cols <- c('fips', 
                     'cohort_year', 
                     'sex', 
                     'race', 
                     'age_group', 
                     'off_type', 
                     'repeat_contact',
                     'f_CG')


# Pivot data to long prior to de-multiplexing
# Note, this is for the filtered version (2008-xxxx, excluding parole & probation)
long_df <- data_sup |> 
  filter(cohort_year > 2008,
         str_starts(fips, '42')) |> 
  select(1:col_stop) |> 
  pivot_longer(cols = -identifier_cols,
               names_to = 'variable_name')


# Process long data by de-multiplexing name column
proc_df2 <- long_df |> 
  # EXTRACT follow-up year from variable_name column
  mutate(years_post = case_when(
    grepl("_y1$", variable_name) ~ 1, #ending in _y1
    grepl("_y2$", variable_name) ~ 2, #ending in _y2
    grepl("_y3$", variable_name) ~ 3, #ending in _y3
    grepl("_y4$", variable_name) ~ 4, #ending in _y4
    grepl("_y5$", variable_name) ~ 5, #ending in _y5
    .default = 0)) |> 
  #REMOVE year suffix from variable_name
  mutate(variable_name = sub("_y[12345]$", "", variable_name)) |> 
  #EXTRACT flag-type (F_Y2, F_EG or EG) from variable_name column
  mutate(flag = case_when(
    grepl("f_2Y", variable_name) ~ 'flag_2Y',
    grepl('f_EG', variable_name) ~ 'flag_EG',
    grepl('EG', variable_name) ~ 'EG_IDENTIFIER',
    .default = "THIS SHOULD NOT EXIST. CHECK FOR ERRORS."
  )) |>
  #REMOVE measurement-type prefix from variable_name
  mutate(variable_name = case_when(
    grepl("f_2Y_", variable_name) ~ sub("f_2Y_", "", variable_name),
    grepl('f_EG_', variable_name) ~ sub("f_EG_", "", variable_name),
    grepl('EG_', variable_name)   ~ sub("EG_", "", variable_name),
  )
) |> 
  #EXTRACT event-type (fe, mi, inc, pro, parole) from variable_name column
  mutate(event_type = case_when(
    grepl("^fe_", variable_name) ~ 'felony',
    grepl("^mi_", variable_name) ~ 'misdemeanor',
    grepl("^inc_", variable_name) ~ 'incarceration',
    grepl("^pro_", variable_name) ~ 'probation', #redundant
    grepl("^par_", variable_name) ~ 'parole', #redundant
  )) |> 
  #REMOVE event-type prefix from variable_name column
  mutate(variable_name = sub("^[^_]*_", "", variable_name)) |> 
  select(1:7, event_type, variable_name, years_post, f_CG, flag, value) |> 
  pivot_wider(names_from = flag)

rm(long_df) # clear up memory

load('data/df_PA_long.rda') # load long data if not already loaded


# Join flags into long data
master <- proc_df |> 
  left_join(
    proc_df2, # These are the flags
    by = join_by(
    fips == fips,                # ID COLUMN
    cohort_year == cohort_year,   # ID COLUMN
    sex == sex,                    # ID COLUMN
    race == race,                   # ID COLUMN
    age_group == age_group,          # ID COLUMN
    off_type == off_type,             # ID COLUMN
    repeat_contact == repeat_contact,# ID COLUMN
    event_type == event_type,       # ID COLUMN
    variable_name == variable_name,# ID COLUMN
    years_post == years_post,     # ID COLUMN
     
      )) |> 
  # Now simply join the county group ID column.
  left_join(data_xwalk, by = join_by('fips' == 'fips')) |> 
  # Reorder the columns a bit.
  relocate(18, .after = 14)





# SAVE REMAINING DATA ---------------------------------------------------------------



# SAVE WIDE DATA WITH DUMMY VARIABLE
save(df_PA_dummy, file = 'data/df_PA_dummy.rda')

# SAVE FILTERED WIDE DATA WITH DUMMY VARIABLE
save(df_PA_dummy_filtered, file = 'data/df_PA_dummy_filtered.rda')

# SAVE FILTERED LONG DATA WITH DUMMY VARIABLE WITH FLAG VARIABLES
save(master, file = 'data/master.rda')


rm(list = ls())

