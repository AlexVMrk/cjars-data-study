
# load libraries ----------------------------------------------------------

library(dplyr)      # data wrangling
library(kableExtra) # tables
library(stringr)    # working with strings
library(tidyr)      # pivot wider

library(tigris)     # geographies
library(sf)					# shapefiles
library(ggplot2)		# plotting
# load data ---------------------------------------------------------------

# Move working directory up one level.
setwd("../")
#load unfiltered data
load('data/master.rda')


# -------------------------------------------------------------------------

sample <- master |> 
												# Pick the outcome statistic
		filter(variable_name == 'above_poverty') |> 
					# pick a breakdown variable (age groups 1, 2 and 3 in this example)
	filter(age_group %in% c(1, 2, 3)) |> 
	# set all other demographic categories to zero
	filter(if_all(c(4,5,7,8), ~ . == 0)) |> 
	# pick an event type
	filter(event_type == 'felony') |> 
	# pick the outlook period
	filter(years_post == 1) |> 
	select(1:13) |> 
	filter(cohort_year %in% c('2010', '2012', '2014', '2016')) |> 
	filter(!is_count)



# -------------------------------------------------------------------------





# Get Pennsylvania counties shapefile
pa_counties <- counties(state = "PA", cb = TRUE, class = "sf")

# FIPS must match type (some are character)
pa_counties$fips <- pa_counties$GEOID


map_data <- pa_counties %>%
  left_join(sample, by = "fips")


# Recode the rural variable with meaningful labels
map_data$rural <- factor(map_data$rural, levels = c("0", "1"), labels = c("Urban", "Rural"))

# Define custom labels for your chosen breakdown variables
custom_labels <- labeller(
  age_group = c("1" = "15-24", "2" = "25-39", "3" = "40+")
)

# Plot with custom facet labels for the chosen breakdown variable
ggplot(map_data) +
  geom_sf(aes(fill = value), size = 0.5) +
	# Change this title if you change the outcome statistic
  scale_fill_viridis_c(option = "plasma", name = "Above-Poverty Rate") +
  #scale_color_manual(values = c("0" = "black", "1" = "red"), name = "Race") +
  theme_minimal() +
  labs(
  	# Change this caption if you change the outcome statistic
    caption = "Above-Poverty rate for different age groups. Felons only."
  ) +
  facet_grid(~ age_group ~ cohort_year, labeller = custom_labels) +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )




