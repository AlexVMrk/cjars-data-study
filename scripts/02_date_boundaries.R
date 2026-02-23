## Date Boundary Exploration Script
## Alexander Markel
## AM97354@wcupa.edu
## 3/20/2025
## GITHUB VERSION

# load libraries ----------------------------------------------------------

library(dplyr)      # wrangling
library(ggplot2)    # plotting
library(tidyr)      # for pivoting
library(patchwork)  # combining plots
library(stringr)    # working with strings
library(kableExtra) # for beautiful tables


# load data ---------------------------------------------------------------

load("data/df_PA_long.rda")




# Define Functions --------------------------------------------------------

# Plotting Function
GenerateTimelinePlot <- function(data, low, high) {
  
  plot <- ggplot(data,
                 aes(x = cohort_year,
                     y = event_type,
                     fill = count,
                     alpha = count)) +
    geom_tile(color = 'white') +
    scale_x_continuous(breaks = seq(2000, max(data$cohort_year), 1)) + 
    scale_y_discrete(labels = c("Felony",
                                "Incarceration",
                                "Misdemeanor")) +
    scale_fill_gradient(low = low,
                        high = high,
                        limits = c(0, 67),
                        breaks = seq(0, 67, by = 67)) + 
    scale_alpha(range = c(0, 1)) + 
    coord_fixed(0.9) + 
    theme_bw() + 
    guides(alpha = "none") + 
    facet_wrap(~plot_labs,
               nrow = length(unique(data$years_post))
    ) + 
    labs(
      x = 'Cohort Year',
      y = 'Event Type'
    )
  
  return(plot)
}


# Data Processing Function
GenerateTimeline <- function(statistic, 
                             subpopulation = '0-0-0-0-0',
                             col_high = 'blue', 
                             col_low = 'white') {
  
  
  #for EACH year(200-2021): 
    #for EACH event type(FE, INC, MI):
      #for EACH out-look period(1, 3, 5):
  #DO: count number of non-NA counties
  
  subpopulation_sex            = substr(subpopulation, 1, 1)
  subpopulation_race           = substr(subpopulation, 3, 3)
  subpopulation_age_group      = substr(subpopulation, 5, 5)
  subpopulation_off_type       = substr(subpopulation, 7, 7)
  subpopulation_repeat_contact = substr(subpopulation, 9, 9)
  
  
  
  df <- proc_df |>
    # Filter for the variable of interest
    filter(variable_name  == statistic) |> 
    # Filter for subpopulation of interest
    filter(sex            == subpopulation_sex,
           race           == subpopulation_race,
           age_group      == subpopulation_age_group,
           off_type       == subpopulation_off_type,
           repeat_contact == subpopulation_repeat_contact) |> 
    # For each year, each event type, each lookback period..
    group_by(cohort_year, event_type, years_post) |> 
    # Count number of non-NA values
    summarise(
      count = sum(!is.na(value)) / 2
    ) |> 
    mutate(plot_labs = paste('Outlook: ', years_post, "year(s)"))
  
  # Generate Plot
  plot <- GenerateTimelinePlot(df, low = col_low, high = col_high)
  print("Printing Plot")
  print(plot)
  print("Complete. Plot and DataFrame stored in list of two if you assigned a the function output to a variable.")
  return(list(plot = plot, data = df))

  
}

# Table Function
GenerateTimeLineTable <- function(
  subpopulation = '0-0-0-0-0',
  recidivism = F
  ) { 
  
  
  if (recidivism == F) { 
    
    variables <- c(
    'any_w2',
    'above_poverty',
    'w2_wages',
    'hud',
    'medicaid',
    'medicare',
    'mortality')
  } else if (recidivism == T) {
    
    variables <- c(
    'to_fe_recid',
    'to_inc_recid',
    'to_mi_recid')
  } else {
      print('Recidivism must be either T or F')
    }
  
  
  subpopulation_sex            = substr(subpopulation, 1, 1)
  subpopulation_race           = substr(subpopulation, 3, 3)
  subpopulation_age_group      = substr(subpopulation, 5, 5)
  subpopulation_off_type       = substr(subpopulation, 7, 7)
  subpopulation_repeat_contact = substr(subpopulation, 9, 9)
  
  df <-  proc_df |> 
    # First filter out NA values
    filter(!is.na(value)) |>
    # Filter for variables of interest; don't mix recid and nonrecid
    filter(variable_name %in% variables) |> 
  # Filter for subpopulation of interest
  filter(sex            == subpopulation_sex,
         race           == subpopulation_race,
         age_group      == subpopulation_age_group,
         off_type       == subpopulation_off_type,
         repeat_contact == subpopulation_repeat_contact) |> 
    # Group for each event type, statistic, and outlook year. 
    group_by(
      event_type,
      variable_name,
      years_post
    ) |> 
    # For each group, calculate date range (min to max cohort year)
    summarise(
      range = paste(min(cohort_year), "-", max(cohort_year))
    ) |> 
    # Pivot wide for table-friendly format
    pivot_wider(values_from = range,
                names_from  = years_post) |> 
    # Arrange Variable column alphabetically
    arrange(variable_name) |> 
    # Append prefix to variable name to indicate event type. 
    mutate(
      variable_name = case_when(
        event_type == 'felony'        ~ paste('FE_', variable_name),
        event_type == 'incarceration' ~ paste('INC_', variable_name),
        event_type == 'misdemeanor'   ~ paste('MI_', variable_name)
      )
    )
  
  # Generate tables. 
 print('test')
  if(!recidivism) { 
    
    df |> colnames() <- c("variable","variable name", "Y1", "Y3", "Y5") 
    table <-  df[, -c(1)] |> 
    kbl(caption = 'Data Boundaries') |> 
    kable_styling(bootstrap_options = c("striped")) |> 
  group_rows(group_label = "Poverty Line", start_row = 1, end_row = 3) |> 
  group_rows(group_label = 'W2 Filings', start_row = 4, end_row = 6) |> 
  group_rows(group_label = 'Housing Assistance', start_row = 7, end_row = 9) |> 
  group_rows(group_label = 'Medicaid', start_row = 10, end_row = 12) |> 
  group_rows(group_label = 'Medicare', start_row = 13, end_row = 15) |> 
  group_rows(group_label = 'Mortality', start_row = 16, end_row = 18) |> 
  group_rows(group_label = 'W2 Wages', start_row = 19, end_row = 21) 
    
  } else if (recidivism) {
  
    df |> colnames() <- c("variable", "variable name", "Y1", "Y2", "Y3","Y4", "Y5") 
    table <-  df[, -c(1)] |> 
    kbl(caption = 'Data Boundaries') |> 
    kable_styling(bootstrap_options = c("striped")) |> 
  group_rows(group_label = 'To Felony Recidivism', start_row = 1, end_row = 3) |> 
  group_rows(group_label = 'To Incarceration Recidivism', start_row = 4, end_row = 4) |> 
  group_rows(group_label = 'To Misdemeanor Recidivism', start_row = 5, end_row = 7)
    
  }
  
  return(list(table = table, data = df))
}


# Generate  ---------------------------------------------------------------


#Generate Timeline Plot (Warning, takes a minute+ for each to generate)
timeline <- GenerateTimeline(statistic = 'any_w2', 
                         subpopulation = '1-2-0-0-0',
                         col_low ='white', 
                         col_high = 'blue')

timeline[1] # This is the plot.
timeline[2] # This is the DataFrame.

#Generate TABLE view (Warning, takes ~30 sec for each to generate, 
#filtering from ~72,380,000 rows)
table <- GenerateTimeLineTable(recidivism = F)

table[1] # This is the table.
table[2] # this is the DataFrame. 



