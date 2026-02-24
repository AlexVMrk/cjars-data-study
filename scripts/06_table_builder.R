## Table Builder Script
## 3/31/2025


# TODO: Implement rural vs urban in both functions. COMPLETE
# TODO: Metaprogram the table generation

# load libraries ----------------------------------------------------------

library(dplyr)      # data wrangling
library(kableExtra) # tables
library(stringr)    # working with strings
library(tidyr)      # pivot wider

# load data ---------------------------------------------------------------

# Move working directory up one level.
setwd("../")
#load data
load('data/master.rda')



# Define Functions --------------------------------------------------------

`%ni%` <- Negate(`%in%`)

# Function to generate felony/misdemeanor/incarceration subsets of data
CreateSubsets <- function(
	data,
	year_start_INCLUSIVE = 2016,
	year_end_INCLUSIVE  = 2017,
	variable_group = 'socioeconomic',
	event_types = c('felony', 'incarceration', 'misdemeanor'),
	exclude_fips = c(),
	county_fips_A = '42029',
	county_fips_B = '42019',
	no_breakdowns = F
)  {
	
	rural_urban_split <- FALSE
	
	# Define variable filters (economic or health?)
	if(variable_group == 'socioeconomic'){
		variable_names_to_filter <- c("any_w2", "w2_wages", "above_poverty", "hud")
	} else if(variable_group == 'health') {
		variable_names_to_filter <- c("medicare", "medicaid", "mortality")
	} else{
		stop ("variable_group must be one of: 'socioeconomic', 'health' ")
	}
	
	if (county_fips_A %in% c('rural', 'urban') & county_fips_B %in% c('rural', 'urban') & county_fips_A != county_fips_B) {
		rural_urban_split <-  TRUE
	} else if (xor( 
		(county_fips_A %in% c('rural', 'urban') & county_fips_B %ni% c('rural', 'urban') ),
		(county_fips_B %in% c('rural', 'urban') & county_fips_A %ni% c('rural', 'urban') )
	)) {
		stop ("county_fips_A and county_fips_B must BOTH be fips IDs or the strings 'rural' and 'urban'")
	}
	
	# Convert years to numeric if not already numeric
  if(is.character(year_start_INCLUSIVE) | is.character(year_end_INCLUSIVE)) {
  	
  	year_start_INCLUSIVE <- as.numeric(year_start_INCLUSIVE)
  	year_end_INCLUSIVE   <- as.numeric(year_end_INCLUSIVE)
  }
	
	# We want to exclude some outlier counties when comparing rural vs urban, but not when comparing Chester VS butler, for instance. 
	if (length(exclude_fips > 0) &  !rural_urban_split) stop ('exluding counties (fips) only allowed when comparing all rural vs all urban. Set county_fips_A and county_fips_B to "rural" and "urban" if this is desired.')
	
	# Create a list of two-year window start years. 
	two_year_window_start <- seq(2000, 2022, by = 2)
	
	# Create a list of two-year window end years. 
	two_year_window_end <- two_year_window_start + 1
	
	
	# Multiple checks here. Function year inputs must match up with the two-year windows. (Unless you only want to look at all individuals, without any breakdowns)
	
	# If user selects a single year, then they must also select no_breakdowns = TRUE, because single-year outcomes only exist for the all demographic category.
	if (year_end_INCLUSIVE - year_start_INCLUSIVE == 0 & !no_breakdowns) {
		stop ("A single year of observation is not possible for any demographic breakdowns. Set no_breakdowns to TRUE if this is intended.")
	# If looking at any breakdowns, year_start_INCLUSIVE must be one from two_year_window_start list.
		} else if (year_start_INCLUSIVE %ni% two_year_window_start & !no_breakdowns) {
			stop ('Demographic breakdowns are measured in two-year windows (i.e 2014 and 2015 inclusively). year_start_INCLUSIVE must be one of: 2000, 2002, 2004, 2006, 2008, 1010, 2012, 2014, 2016, 2018, 2020, 2022')
			# If looking at any breakdowns, year_end_INCLUSIVE must be one from two_year_window_end list.
		} else if (year_end_INCLUSIVE %ni% two_year_window_end & !no_breakdowns) { 
			stop ('Demographic breakdowns are measured in two-year windows (i.e 2014 and 2015 inclusively). year_end_INCLUSIVE must be one of: 2001, 2003, 2005, 2007, 2009, 1011, 2013, 2015, 207, 2019, 2021, 2023')
		}
	
		# Calculate the number of windows the user is filtering for. 
		N_windows <- ((year_end_INCLUSIVE - year_start_INCLUSIVE) + 1) / 2
		
		# Extract the start years for each window. (2016 is the start year for the 2016-2017 window. 2018 is the start year for the 2018-2019 window. Etc.)
		window_starts <- seq(year_start_INCLUSIVE, year_end_INCLUSIVE, by = 2)
		
		# Check for errors
		if (N_windows != length(window_starts)) stop ('Number of windows does not match number of years. This should never happpen: is likely a code error.')
	
		if (county_fips_A == county_fips_B) stop ("FIPS A and B can't be identical.")
		
	# Check that event types are allowed. 
	allowed_event_types <- c('felony', 'incarceration', 'misdemeanor')
	selected_event_types <- event_types
	
	# Create an empty list for the event types.
	event_types <- list()
	
	for (event in selected_event_types) {
		
		# Stop the function if any event type is mispelled or is incorrect
		if(event %ni% allowed_event_types) stop ("Allowed event types: felony, misdemeanor, incarceration (case-sensitive)")
		
		# Fill the event_types list.
		if(event == 'felony')        event_types['event_type_fe']  <- 'felony'
		if(event == 'incarceration') event_types['event_type_inc'] <- 'incarceration'
		if(event == 'misdemeanor')   event_types['event_type_mi']  <- 'misdemeanor'
	}
	
	# Create an empty list for the generated subsets of data. 
	subsets <- list()
	
	
	if (!no_breakdowns) {
	  
	  if(!rural_urban_split) {
	    
	    # For each event type that the user provided:
	    for (event in event_types) {
	      
	      # Assign the corresponding DataFrame to a list element with the name of the event
	      subsets[[paste(event)]] <- master |> 
	        filter(event_type == event) |>
	        filter(fips %in% c(county_fips_A, county_fips_B)) |>
	        filter(years_post %in% c(1, 3, 5)) |> 
	        filter(variable_name %in% variable_names_to_filter) |>
	        mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = '-')) |> 
	        # Filter for only the window start year if the demographic has any breakdowns. Otherwise filter for between start and end years if the demographic group is '0-0-0-0-0' (all)
	        filter( ((cohort_year %in% window_starts & demo_group != '0-0-0-0-0') | ((cohort_year >= year_start_INCLUSIVE & cohort_year <= year_end_INCLUSIVE) & demo_group == '0-0-0-0-0'))) #|>
	      #select(-c(4:8)) |> 
	      #select(1:4, demo_group, 5:14) 
	      
	    }	
	  } else if(rural_urban_split) {
	    
	    for (event in event_types) {
	      
	      subsets[[paste(event)]] <- master |> 
	        filter(event_type == event) |> 
	        filter(fips %ni% c(exclude_fips)) |> 
	        filter(years_post %in% c(1, 3, 5)) |> 
	        filter(variable_name %in% variable_names_to_filter) |>
	        mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = '-')) |> 
	        # Filter for only the window start year if the demographic has any breakdowns. Otherwise filter for between start and end years if the demographic group is '0-0-0-0-0' (all)
	        filter( ((cohort_year %in% window_starts & demo_group != '0-0-0-0-0') | ((cohort_year >= year_start_INCLUSIVE & cohort_year <= year_end_INCLUSIVE) & demo_group == '0-0-0-0-0')))
	      
	    }
	  }

	} else if (no_breakdowns) {
	  
	  if (!rural_urban_split) {
	   
	    for (event in event_types) {
	      
	      subsets[[paste(event)]] <- master |> 
	        filter(event_type == event) |>
	        filter(fips %in% c(county_fips_A, county_fips_B)) |>
	        filter(years_post %in% c(1, 3, 5)) |> 
	        filter(variable_name %in% variable_names_to_filter) |>
	        mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = '-')) |> 
	        filter((cohort_year >= year_start_INCLUSIVE & cohort_year <= year_end_INCLUSIVE) & demo_group == '0-0-0-0-0') #|>
	      #select(-c(4:8)) |> 
	      #select(1:4, demo_group, 5:14) 
	      
	    } 
	  } else if(rural_urban_split){
	    
	    subsets[[paste(event)]] <- master |> 
	      filter(event_type == event) |>
	      filter(years_post %in% c(1, 3, 5)) |> 
	      filter(variable_name %in% variable_names_to_filter) |>
	      filter(fips %ni% c(exclude_fips)) |>
	      mutate(demo_group = paste(sex, race, age_group, off_type, repeat_contact, sep = '-')) |> 
	      filter((cohort_year >= year_start_INCLUSIVE & cohort_year <= year_end_INCLUSIVE) & demo_group == '0-0-0-0-0') #|>
	    #select(-c(4:8)) |> 
	    #select(1:4, demo_group, 5:14) 
	    
	  }
	} 
	
	return(subsets)
}




GenerateTableData <- function(
	subset = list(),
  breakdown_variables = c('race', 'sex'),
	breakdown_limits = c("012", '01'),
	outlook_years  = c(1, 3),
	first_row_ungroup = T) {
	
	# Determine number of statistics:
	n_stats <- length(unique(subset[[1]]$variable_name))
	end_index <- 3 + n_stats
	
	# Determine if its rural_vs_urban
	if (length(unique(subset[[1]]$fips)) > 2) {
		print ("Detected rural vs urban split.")
		grouping_var <- 'rural'
	} else if (length(unique(subset[[1]]$fips)) == 2) {
		grouping_var <- 'fips'
	} else {
		stop ("This error should never occur. Check code. Comment #3.")
	}
	
	# Check for spelling errors
	if(any(breakdown_variables %ni% c('sex', 'race', 'age_group', 'off_type', 'repeat_contact'))) {
		stop("breakdown_variables must be zero, one or two of: 'sex', 'race', 'age_group', 'off_type', 'repeat_contact' ")
	}
	
	# Check and stop if duplicate breakdown variables
	if(length(unique(breakdown_variables)) != length(breakdown_variables)) {
		stop("Duplicate breakdown_variable not allowed.")}
	
	
	# Check and stop if conflicting requests
	if(length(breakdown_variables) > 0  &  length(unique(subset[[1]]$demo_group)) == 1) {
		stop('You indicated at least one breakdown, but subset data only has "all" demographic data (0-0-0-0-0), so no breakdowns can be performed.')}
	
	# Disallow more than two groupings.
	if(length(breakdown_variables) > 2){
		stop ('More than two grouping variables creates cluttered and granular tables.')}
	
	# Check that breakdown limits start with zero
	if(any(substr(breakdown_limits, 1, 1) != '0')){
		indexes <- which(substr(breakdown_limits, 1, 1) != '0')
		
		for (index in indexes) {
			print(paste0("All breakdown limits must start with a zero. breakdown_limit ", breakdown_limits[index], " changed to: "))
			breakdown_limits[index] <- paste0("0", breakdown_limits[index])
			print(breakdown_limits[index])
		}
	}
	
	# Check that number of breakdown variables matches number of breakdown limits.
	if(length(breakdown_variables) != length(breakdown_limits)){
		stop('Number of breakdown limits and number of breakdown variables must be the same.')}
	
	
	
	# Create a dictionary to store the position of different demographic categories within the string 'x-x-x-x-x'
  dictionary <- data.frame(
    position = c(1, 2, 3, 4, 5),
    demographic_category = c('sex', 'race', 'age_group', 'off_type', 'repeat_contact'))
  
  unique_values <- list(list(0,1,2), list(0,1,2,3,4,5), list(0,1,2,3), list(0,1,2,3), list(0,1,2,3))
  dictionary$unique_values <- unique_values
  
  # Check that the breakdown limits are not out of bounds
  for (i in 1:length(breakdown_limits)) {
  	
  	length <- dictionary |> 
  		filter(demographic_category == breakdown_variables[i]) |> 
  		select(unique_values) |> 
  		pull() |> 
  		unlist() |> 
  		length() 
  	
  	if(nchar(breakdown_limits[i]) > length){
  		
  		print(paste("Breakdown limit for", breakdown_variables[i], 'is out of bounds.'))
  		stop('Breakdown limit is out of bounds.')}
  	
  }
  
  # Create result list
  results <- list("year_1" = list(),"year_3" = list(), "year_5" = list())
  

  # Define empty vector to store column index of breakdown variables
  columns_to_group <- vector()
  
  # Populate the above vector
  for (z in 1:length(breakdown_variables)) {
	  	columns_to_group[z] <-  which(names(subset[[1]][4:8]) %in% breakdown_variables[z]) + 3
	  	print(paste(breakdown_variables[z], columns_to_group[z]))
  }
 
  print('before:')
  print(columns_to_group)
  # Relocate columns to make things easier later.
  
  # For each dataset in the subset list:
  for (k in 1:length(subset)) {

  		# Relocate the breakdown variable to the first and second breakdown column.
  		subset[[k]] <- subset[[k]] |> 
	  	relocate(columns_to_group, .after = 3) |> 
  		unite(demo_group, 4:8, sep = '-', remove = FALSE) |> 
		  relocate(demo_group, .after = 19)
  }
  

 # Re-Define empty vector to store column index of breakdown variables (these should now be 1 and 2, or just 1.) 
 columns_to_group <- vector()
 
 print(subset[[1]])
 # Confirm that these are 1 and 2 or just one
 for (w in 1:length(breakdown_variables)) {
	  	columns_to_group[w] <-  which(names(subset[[1]][4:8]) %in% breakdown_variables[w]) + 3
	  	
 }
 
  print('after:')
  print(columns_to_group)
  
  # Define non-grouping variables These should NOT be the same as the redefined grouping indexes.
  columns_to_filter <- which(names(subset[[1]][4:8]) %ni% breakdown_variables) + 3
  
	# These are the column indexes of the demographic variables.
  demographics_indexes <- c(4, 5, 6, 7, 8)
	
  # all demographic indexes should be occupied, either by grouping or filtering columns
  # so 4+5+6+7+8 should be returned here. 
  if(sum(unlist(columns_to_group),  unlist(columns_to_filter)) != sum(demographics_indexes)) stop ("This error should never occur. Check code. Comment #1")
  
  
  # Redundant way of checking the above: filtering column indexes can not be the same as breakdown column indexes. 
  if(any(columns_to_filter %in% demographics_indexes[5 - length(columns_to_filter)])) stop ("This error should never occur. Check code. Comment #2")

  
	# For each subset (felony, incarceration, misdemeanor)
  for (data in subset) {
	  	
	  	# For each subset, for each outlook year
	  	for (i in 1:length(outlook_years)) {
	  		
	  		if(i == 1) outlook <- 1
	  		if(i == 2) outlook <- 3
	  		if(i == 3) outlook <- 5
	  		
	  	# Set all non-grouping variables equal to zero. 
  		 temp1 <- data |> 
  			filter(across(columns_to_filter, ~ . == '0')) 
  		 
  		 # For each subset, for each outlook year, for each column to group
  		 for (j in 1:length(columns_to_group)) {
  		 	
  		 	# Filter current column to group to allowed values as defined by breakdown_limits. 
  		 	temp1 <- temp1 |>
  		 		filter(across(columns_to_group[j], ~. %in% unlist(str_split(breakdown_limits[j], ""))))
  		 }
  		 
  		 
  		 temp2 <- temp1 |> 
  		 	# Filter for current outlook
  		 	filter(years_post == outlook) |> 
  		 	# Group by columns to group
  		 	group_by(across(columns_to_group)) |> 
  		 	# For each combination:
  		 	mutate(
  		 		# Decompose proportion (see notes)
  		 		value_decomposed = ifelse(
  		 		(!is_count),
  		 		value * lead(value, 1),
          value
  		 		)
  		 	 ) |>
  		 	mutate(
  		 		# Re-weight wages (see notes)
  		 		value_employed = ifelse(
  		 			variable_name == 'w2_wages' & !is_count, 
  		 			value_decomposed / (lead(value_decomposed, 1) * lag(value, 4)),
  		 			ifelse(variable_name == 'w2_wages' & is_count,
             value_decomposed * lag(value, 5),
             NA
      )
    )
  ) |> # For each geography, demographic group, and outcome statistic
  		 	group_by(!!sym(grouping_var), demo_group, variable_name) |> 
  		 	summarise(
  		 		Rate = ifelse(
  		 			first(variable_name == 'w2_wages'),
  		 			sum(value_decomposed[seq(1, n(), by = 2)], na.rm = T) / sum(value_employed[seq(0, n(), by = 2)], na.rm = T),
  		 			sum(value_decomposed[seq(1, n(), by = 2)], na.rm = T) / sum(value_decomposed[seq(2, n(), by = 2)], na.rm = T) 
                  ),
  		 		`Thousands of Individuals` = ifelse(
  		 			first(variable_name == 'w2_wages'),
            sum(value_employed[seq(2, n(), by = 2)], na.rm = T) / 1000,
            sum(value_decomposed[seq(2, n(), by = 2)], na.rm = T) / 1000)
  		 	) |> 
  		 	pivot_longer(
  		 		cols = c(4:5),
          names_to = 'Statistic'
  ) |> 
  		 	pivot_wider(
  		 		names_from = variable_name
  ) |> 
  		 	mutate(
  		 		across(where(is.numeric), ~round(., 3))
  		 		) |>
  		 	ungroup() |> 
  		 	select(!!grouping_var, demo_group, Statistic, 4:end_index) 
  		 
  		 if (first_row_ungroup & length(breakdown_limits) == 2) {
  		 	
  		 	n_rows_to_remove <- nchar(breakdown_limits[2]) - 1 # 1
  		  n_rows <- nrow(temp2)
  		  
  		  row_sequence <- seq(from = 1, to = n_rows, by = 2)
  		  
  		  row_sequence_A <- row_sequence[2: (n_rows / 4) ]
  		  row_sequence_B <- row_sequence[((n_rows / 4) + 2): (n_rows / 2)]
  		  
  		 rows_to_remove_A <- row_sequence_A[1:n_rows_to_remove]
	  	 rows_to_remove_B <- row_sequence_B[1:n_rows_to_remove]
	  	 
	  	 rows_to_remove <- c(rows_to_remove_A, rows_to_remove_A + 1,  rows_to_remove_B, rows_to_remove_B + 1)
	  	 print(n_rows_to_remove)
	  	 
	  	 temp2 <- temp2[-rows_to_remove, ]
  		 	
  		 }
  		 
  		 #if (ungroup_all_row) {
  		 #	temp <- temp[-c(3:4, 15:16), ]
  		 #}
  		 

  event_type <- unique(data$event_type)
  		 
  		 
	
  results[[i]][[paste(event_type)]] <- temp2
	  	}
  }
	return(results)
	}



#  test it out ------------------------------------------------------------

subsets <- CreateSubsets(data = master,
              year_start_INCLUSIVE = 2014,
              year_end_INCLUSIVE = 2017,
	             variable_group = 'socioeconomic',
              event_types = c('felony', 'misdemeanor'),
	county_fips_A = 'rural',
	county_fips_B = 'urban',
              exclude_fips = c(),
              no_breakdowns = F)

# verify 67 counites
length(unique(df_fe$fips))


df_fe <- subsets[['felony']]
df_mi <- subsets[['misdemeanor']]
df_inc <- subsets[['incarceration']]


table_data <- GenerateTableData(
	subset = subsets,
	breakdown_variables = c('race', 'sex'),
	breakdown_limits = c('012', '01'),
	outlook_years = c(1,3,5),
  first_row_ungroup = TRUE)


# Take a look at year 1 data.
test$year_1$felony



















	tables_list <- list(year_1 = list(), year_3 = list())
	
	index <- 0
	for (year in results) {
		
		index <- index + 1
		
		for (data in year) {
			
			event_type = unique(data$event_type)
			#tables_list[[index]][[paste(event_type)]]
year_1_fe[, -c(1, 2)] |> 
    kable(caption = 'Data for the years 2014-2015, only Incarcerations, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
      kable_minimal(full_width = F)  |> 
      group_rows('Butler County', 1, 10, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
      group_rows('Chester County', 11, 20, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|>
      group_rows('All', 1, 2)|> 
      group_rows('White', 3, 4)|> 
      group_rows('Young White', 5, 6)|> 
      group_rows('Black', 7, 8)|> 
      group_rows('Young Black', 9 ,10) |> 
      group_rows('All', 11, 12)|> 
      group_rows('White', 13, 14)|> 
      group_rows('Young White', 15, 16)|> 
      group_rows('Black', 17, 18)|> 
      group_rows('Young Black',19, 20) |> 
      add_header_above(c('Economic Outcomes' = 5))
			
			 return(test)
		}
	}