## Two-Year window Exploration Script
## 3/31/2025


# load libraries ----------------------------------------------------------

library(dplyr)   # data wrangling
library(ggplot2) # plotting


# load data ---------------------------------------------------------------


# Move working directory up one level.
setwd("../")

#load unfiltered data
load('data/df_PA_long_unflitered.rda')


# define function ---------------------------------------------------------

PlotWindows <- function(
		data, 
	county_fips = '42029', 
	crime_event_type  = 'felony', 
	outcome_variable_name = 'above_poverty',
	outlook_years = c(1, 3),
	subpopulation = '0-1-0-0-0'
) {
	
	subpopulation_sex            = substr(subpopulation, 1, 1)
  subpopulation_race           = substr(subpopulation, 3, 3)
  subpopulation_age_group      = substr(subpopulation, 5, 5)
  subpopulation_off_type       = substr(subpopulation, 7, 7)
  subpopulation_repeat_contact = substr(subpopulation, 9, 9)
  
  
  plotting_data <- data |> 
  	  filter(!is_count) |> 
  	  filter(fips == county_fips) |> 
  filter(event_type == crime_event_type) |> 
  filter(variable_name %in% outcome_variable_name) |> 
  filter(years_post %in% outlook_years) |> 
  filter(
    sex == subpopulation_sex,
    race == subpopulation_race,
    age_group == subpopulation_age_group,
    off_type == subpopulation_off_type,
    repeat_contact == subpopulation_repeat_contact
 )
 
 plot <- ggplot(plotting_data, aes(x = cohort_year, y = value)) + 
  geom_line(aes(color = as.factor(years_post)), linewidth = 2) + 
  scale_x_continuous(limits = c(min(data$cohort_year), max(data$cohort_year)), breaks = seq(min(data$cohort_year), max(data$cohort_year), 1)) +
 	scale_color_manual(values = c("slategray3", "tomato2")) +
  theme_bw() +
  geom_point() + 
  theme(panel.grid.major.x = element_line(linewidth = 0.5, linetype = "solid", color = "lightgray"),
        panel.grid.minor.x = element_blank()) + 
  facet_wrap(~variable_name) + 
  labs(
    x = "Cohort Year",
    y = "Rate",
    color = 'Outlook Year')
 
 return(plot)
  	  
  	  
}



# Generate Plots ----------------------------------------------------------


PlotWindows(data = proc_df, 
	crime_event_type = 'felony',
	subpopulation = '0-1-0-0-0')



