# load libraries ----------------------------------------------------------

prepare_packages <- function(required_packages = c("dplyr", "tidyr", "fastDummies", "ggnuplot", "stringr", "ProbMarg")) {
	
	installed_packages <-  installed.packages()[, 1]
	for(package in required_packages) {
	print(package)
	if (!(package %in% installed_packages)) {
		print(paste("Installing: ",package))
		install.packages(package)
		}
	library(package, character.only = TRUE)
		}
	}


prepare_packages()


# load data ---------------------------------------------------------------


setwd("..")
load("data/df_PA_long_unfiltered.rda")

df_wider <- proc_df |> 
  pivot_wider(
    names_from = is_count,
    values_from = value,
    names_prefix = "is_count_"
  )

# -------------------------------------------------------------------------
# free up memory
rm(proc_df)


GenerateRegressionData <- function(data, grouping_indexes = c(4, 5), outcome_variable = c("any_w2"), event_types = c('felony', 'incarceration', 'misdemeanor'), split_years = T, fips_as_dummies = c(42101, 42103), exclude_fips = c()) {
	

	
	`%ni%` <- Negate(`%in%`)
	possible_indexes <- c(4, 5, 6, 7, 8)
	two_year_window_start <- seq(2000, 2022, by = 2)
	not_grouping_indexes <- which(possible_indexes %ni% grouping_indexes) + 3	
	
	grouping_name_1 <- colnames(data[, grouping_indexes[1]])
  grouping_name_2  <- colnames(data[, grouping_indexes[2]])
  non_grouping_name_1 <- colnames(data[, not_grouping_indexes[1]])
  non_grouping_name_2 <- colnames(data[, not_grouping_indexes[2]])
  non_grouping_name_3 <- colnames(data[, not_grouping_indexes[3]])
	print(grouping_name_1)
	print(grouping_name_2)
		
	if (split_years) {
		
		names <- c('year_1', 'year_3', 'year_5')
		values <- c(1, 3, 5)
		data_list <- list()
		
	
			
			for (i in 1:3) {
				
			data_list[[paste(names[i])]] <-  data |> 
				
				# Filtering
				filter(years_post == values[i]) |> 
				filter(repeat_contact != 3) |> 
				filter(fips %ni% exclude_fips,
					     event_type %in% event_types) |> 
				filter(!!as.symbol(grouping_name_1) != 0, !!as.symbol(grouping_name_2) != 0,
		  	if_all(not_grouping_indexes, ~ . == 0),
		  	event_type %in% c('felony', 'incarceration', 'misdemeanor'),
		  	!if_any(c(is_count_TRUE, is_count_FALSE), is.na),
		  	variable_name %in% outcome_variable,
		  	cohort_year %in% two_year_window_start) |> 
				#Renaming
				rename(
				proportion = is_count_FALSE,
				denominator = is_count_TRUE
	) 
				
				
	if(length(fips_as_dummies) != 0) { 
		
		 data_list[[paste(names[i])]] <- data_list[[paste(names[i])]] |> 
		 	
		 	mutate(
		 		PHL_AL = ifelse(fips %in% fips_as_dummies, 1, 0)
		 		)
		 
	}
				
				
				data_list[[paste(names[i])]] <- data_list[[paste(names[i])]]	|> 
						#Mutating
					  mutate(
							PHL_AL = ifelse(fips %in% fips_as_dummies, 1, 0),
							numerator = round(proportion * denominator, 0)
	) |> 
						mutate(
							positive = numerator,
							negative = denominator - numerator
	) |> 
				#Grouping
						group_by(
							fips, PHL_AL, cohort_year, rural, .data[[grouping_name_1]], .data[[grouping_name_2]], event_type, variable_name, years_post
			) |> 
				#Summarize
						summarise(
							denominator = sum(denominator),
							positive = sum(positive),
							negative = sum(negative),
							proportion = ifelse(denominator == 0, proportion, sum(positive) /  sum(denominator))
	) |> 
							select(-c(fips, proportion, years_post)) |> 
							pivot_longer(cols = c('positive', 'negative'), names_to = 'name', values_to = 'weight') |> 
							mutate(outcome = ifelse(name == 'positive', 1, 0)) |> 
							# Dummies
							dummy_cols(select_columns = "cohort_year", remove_selected_columns = TRUE, remove_first_dummy = TRUE) 
				
					
				if(length(event_types) == 1) {
					
					data_list[[paste(names[i])]] <- data_list[[paste(names[i])]] |>
					#dummy_cols(select_columns = "event_type", remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE) |>  # CHANGE HERE
					dummy_cols(select_columns = c(grouping_name_1), remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE) |> 
					dummy_cols(select_columns = c(grouping_name_2), remove_selected_columns = TRUE, remove_most_frequent_dummy =  TRUE) |> 
					select(rural, contains("years_post"), contains(grouping_name_1), contains(grouping_name_2), contains('cohort'), PHL_AL, outcome, weight)#|> 
					#mutate(across(1:12, as.factor))
					
					
				#	return(data_list)
					
				} else if (length(event_types) > 1) {
					
					data_list[[paste(names[i])]] <- data_list[[paste(names[i])]] |>
					dummy_cols(select_columns = "event_type", remove_selected_columns = TRUE, remove_first_dummy = TRUE) |>  # CHANGE HERE
					dummy_cols(select_columns = c(grouping_name_1), remove_selected_columns = TRUE, remove_first_dummy = TRUE) |> 
			  	dummy_cols(select_columns = c(grouping_name_2), remove_selected_columns = TRUE, remove_first_dummy =   TRUE) |> 
					select(rural, contains("years_post"), contains("event_type"), contains(grouping_name_1), contains(grouping_name_2), contains('cohort'), PHL_AL, outcome, weight)#|> 
					#mutate(across(1:index, as.factor))
					
					#return(data_list)
				}
				 print(data_list[[i]])
				 
				index <- 4 + sum(str_detect(names(data_list[[paste(names[i])]]), grouping_name_1)) + 
								 sum(str_detect(names(data_list[[paste(names[i])]]), grouping_name_2)) + 
					       sum(str_detect(names(data_list[[paste(names[i])]]), "cohort")) + (length(event_types) - 1) - 1
				
			
				data_list[[paste(names[i])]] <- data_list[[paste(names[i])]] |>
        mutate(across(1:index, as.factor),
        	PHL_AL = as.factor(PHL_AL))
				print("DONE ONE")
		}
		
		return(data_list)
		
	} 
	
	
  
	regression_data <- data |> 
		# Filtering
		dplyr::filter(fips %ni% exclude_fips) |> 
		filter(!!as.symbol(grouping_name_1) != 0, !!as.symbol(grouping_name_2) != 0,
  	if_all(not_grouping_indexes, ~ . == 0),
  	event_type %in% event_types,
  	!if_any(c(is_count_TRUE, is_count_FALSE), is.na),
  	variable_name %in% outcome_variable,
  	cohort_year %in% two_year_window_start) |> 
		#Renaming
		rename(
		proportion = is_count_FALSE,
		denominator = is_count_TRUE
	) |> 
		#Mutating
	mutate(
		numerator = round(proportion * denominator, 0)
	) |> 
	mutate(
		positive = numerator,
		negative = denominator - numerator
	) |> 
		#Grouping
	group_by(
		fips, cohort_year, rural, sex, race, event_type, variable_name, years_post
	) |> 
		#Summarize
	summarise(
		denominator = sum(denominator),
		positive = sum(positive),
		negative = sum(negative),
		proportion = ifelse(denominator == 0, proportion, sum(positive) /  sum(denominator))
	) |> 
		select(-c(fips, proportion)) |> 
		pivot_longer(cols = c('positive', 'negative'), names_to = 'name', values_to = 'weight') |> 
		mutate(outcome = ifelse(name == 'positive', 1, 0)) |> 
		# Dummies
		dummy_cols(select_columns = "cohort_year", remove_selected_columns = TRUE, remove_first_dummy = TRUE) |> 
	  dummy_cols(select_columns = "event_type", remove_selected_columns = TRUE, remove_most_frequent_dummy =  TRUE) |> 
	  dummy_cols(select_columns = "years_post", remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE) |> 
  	dummy_cols(select_columns = c(grouping_name_1), remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE) |> 
	  dummy_cols(select_columns = c(grouping_name_2), remove_selected_columns = TRUE, remove_most_frequent_dummy =  TRUE) |> 
		select(contains('cohort'), rural, contains("years_post"), contains("event_type"), contains(grouping_name_1), contains(grouping_name_2), outcome, weight)|> 
		mutate(across(1:19, as.factor))
	
	return(regression_data)
}


# Sample usage:

reg_data <- GenerateRegressionData(
 	data = df_wider, 
	# Determine the indexes by running head(df_wider)
	# sex and race are columns 4 and 5, and so those
	# are the indexes in this example.
 	grouping_indexes = c(4, 5), 
	# Pick an outcome variable
 	outcome_variable = 'to_mi_recid', 
	# Probably keep this as TRUE always.
 	split_years = T, 
	# This argument allows for counties acting as dummy variables.
	# 42101 and 42103 are PHL and ALGH
 	fips_as_dummies = c(42101, 42103), 
	# Do you want to exclude any counties? 
 	exclude_fips = c(), 
	# Pick the event types to include.
 	event_types = c("felony", "misdemeanor"))
 
# check out the data
 year_1_data <- reg_data[[1]]
 


# generate regression data ------------------------------------------------


#Add Interactions
#Black * Rural
#Black * Felony
#Black * Misdemeanor
#Rerun with interactions with and without Philadelphia & Allegheny
#Rerun with interactions differing dependent variables: HUD, Poverty, Wages with and without Philadelphia & Allegheny


# No exclusion ------------------------------------------------------------

reg_data_RACExSEX_on_anyw2_no_exlusion <- GenerateRegressionData(data = df_wider, grouping_indexes = c(4, 5), outcome_variable = 'any_w2', exclude_fips = c())
reg_data_RACExSEX_on_HUD_no_exlusion <- GenerateRegressionData(data = df_wider, grouping_indexes = c(4, 5), outcome_variable = 'hud', exclude_fips = c())
reg_data_RACExSEX_on_poverty_no_exlusion <- GenerateRegressionData(data = df_wider, grouping_indexes = c(4, 5), outcome_variable = 'above_poverty', exclude_fips = c())


# with exclusion ----------------------------------------------------------

reg_data_RACExSEX_on_anyw2_with_exlusion <- GenerateRegressionData(data = df_wider, grouping_indexes = c(4, 5), outcome_variable = 'any_w2', exclude_fips = c('42101', '42103'))
reg_data_RACExSEX_on_HUD_with_exlusion <- GenerateRegressionData(data = df_wider, grouping_indexes = c(4, 5), outcome_variable = 'hud', exclude_fips = c('42101', '42103'))
reg_data_RACExSEX_on_poverty_with_exlusion <- GenerateRegressionData(data = df_wider, grouping_indexes = c(4, 5), outcome_variable = 'above_poverty', exclude_fips = c('42101', '42103'))




# run regressions ---------------------------------------------------------


# AnyW2 -------------------------------------------------------------------


# No exclusions (WE = WITH EXLUSION)

model_anyw2_NE <- glm(outcome ~ . - weight,
             data = reg_data_RACExSEX_on_anyw2_no_exlusion,
             weights = weight,
             family = "binomial")

model_anyw2_BLACKxFELON_NE <- glm(outcome ~ race_2 * event_type_felony + . - weight,
             data = reg_data_RACExSEX_on_anyw2_no_exlusion,
             weights = weight,
             family = "binomial")

model_anyw2_BLACKxMIS_NE <- glm(outcome ~ race_2 * event_type_misdemeanor + . - weight,
             data = reg_data_RACExSEX_on_anyw2_no_exlusion,
             weights = weight,
             family = "binomial")

model_anyw2_BLACKxRURAL_NE <- glm(outcome ~ race_2 * rural + . - weight,
             data = reg_data_RACExSEX_on_anyw2_no_exlusion,
             weights = weight,
             family = "binomial")


models <- list(model_anyw2_NE, model_anyw2_BLACKxFELON_NE, model_anyw2_BLACKxMIS_NE, model_anyw2_BLACKxRURAL_NE)

names <- c("2002", "2004", "2006", 
	"2008", "2010", "2012", "2014", 
	"2016", "Rural", "Three Years After", 
	"Five Years After", "Felony", "Misdemeanor", 
	"Female", "Black", "Asian or Pacific Islander",
	"Hispanic", "AIAN", "Black * Felony",
	"Black * Misdemeanor", "Black * Rural")


stargazer::stargazer(
... = models,
	covariate.labels = names,
	type = 'html',
	title = 'Regression on Employement, No exclusion',
	out = "assets/REG_ANYW2_NO_EXL.html")


# With exclusions (NE = NO EXLUSION)

model_anyw2_WE <- glm(outcome ~ . - weight,
             data = reg_data_RACExSEX_on_anyw2_with_exlusion,
             weights = weight,
             family = "binomial")

model_anyw2_BLACKxFELON_WE <- glm(outcome ~ race_2 * event_type_felony + . - weight,
             data = reg_data_RACExSEX_on_anyw2_with_exlusion,
             weights = weight,
             family = "binomial")

model_anyw2_BLACKxMIS_WE <- glm(outcome ~ race_2 * event_type_misdemeanor + . - weight,
             data = reg_data_RACExSEX_on_anyw2_with_exlusion,
             weights = weight,
             family = "binomial")

model_anyw2_BLACKxRURAL_WE <- glm(outcome ~ race_2 * rural + . - weight,
             data = reg_data_RACExSEX_on_anyw2_with_exlusion,
             weights = weight,
             family = "binomial")

models <- list(model_anyw2_WE, model_anyw2_BLACKxFELON_WE, model_anyw2_BLACKxMIS_WE, model_anyw2_BLACKxRURAL_WE)



stargazer::stargazer(
... = models,
	covariate.labels = names,
	type = 'html',
	title = 'Regression on Employement, With Exclusion (42101, 42103)',
	out = "assets/REG_ANYW2_WITH_EXL.html")



# HUD ---------------------------------------------------------------------

# No exclusions (WE = WITH EXLUSION)

model_HUD_NE <- glm(outcome ~ . - weight,
             data = reg_data_RACExSEX_on_HUD_no_exlusion,
             weights = weight,
             family = "binomial")

model_HUD_BLACKxFELON_NE <- glm(outcome ~ race_2 * event_type_felony + . - weight,
             data = reg_data_RACExSEX_on_HUD_no_exlusion,
             weights = weight,
             family = "binomial")

model_HUD_BLACKxMIS_NE <- glm(outcome ~ race_2 * event_type_misdemeanor + . - weight,
             data = reg_data_RACExSEX_on_HUD_no_exlusion,
             weights = weight,
             family = "binomial")

model_HUD_BLACKxRURAL_NE <- glm(outcome ~ race_2 * rural + . - weight,
             data = reg_data_RACExSEX_on_HUD_no_exlusion,
             weights = weight,
             family = "binomial")


models <- list(model_HUD_NE, model_HUD_BLACKxFELON_NE, model_HUD_BLACKxMIS_NE, model_HUD_BLACKxRURAL_NE)

stargazer::stargazer(
... = models,
	covariate.labels = names,
	type = 'html',
	title = 'Regression on HUD uptake, No Exclusion',
	out = "assets/REG_HUD_NO_EXL.html")


# With exclusions (NE = NO EXLUSION)

model_HUD_WE <- glm(outcome ~ . - weight,
             data = reg_data_RACExSEX_on_HUD_with_exlusion,
             weights = weight,
             family = "binomial")

model_HUD_BLACKxFELON_WE <- glm(outcome ~ race_2 * event_type_felony + . - weight,
             data = reg_data_RACExSEX_on_HUD_with_exlusion,
             weights = weight,
             family = "binomial")

model_HUD_BLACKxMIS_WE <- glm(outcome ~ race_2 * event_type_misdemeanor + . - weight,
             data = reg_data_RACExSEX_on_HUD_with_exlusion,
             weights = weight,
             family = "binomial")

model_HUD_BLACKxRURAL_WE <- glm(outcome ~ race_2 * rural + . - weight,
             data = reg_data_RACExSEX_on_HUD_with_exlusion,
             weights = weight,
             family = "binomial")

models <- list(model_HUD_WE, model_HUD_BLACKxFELON_WE, model_HUD_BLACKxMIS_WE, model_HUD_BLACKxRURAL_WE)


stargazer::stargazer(
... = models,
	covariate.labels = names,
	type = 'html',
	title = 'Regression on HUD uptake, With Exclusion (42101, 42103)',
	out = "assets/REG_HUD_WITH_EXL.html")




# ABOVE POVERTY ---------------------------------------------------------------------

# No exclusions (WE = WITH EXLUSION)

model_poverty_NE <- glm(outcome ~ . - weight,
             data = reg_data_RACExSEX_on_poverty_no_exlusion,
             weights = weight,
             family = "binomial")

model_poverty_BLACKxFELON_NE <- glm(outcome ~ race_2 * event_type_felony + . - weight,
             data = reg_data_RACExSEX_on_poverty_no_exlusion,
             weights = weight,
             family = "binomial")

model_poverty_BLACKxMIS_NE <- glm(outcome ~ race_2 * event_type_misdemeanor + . - weight,
             data = reg_data_RACExSEX_on_poverty_no_exlusion,
             weights = weight,
             family = "binomial")

model_poverty_BLACKxRURAL_NE <- glm(outcome ~ race_2 * rural + . - weight,
             data = reg_data_RACExSEX_on_poverty_no_exlusion,
             weights = weight,
             family = "binomial")


models <- list(model_poverty_NE, model_poverty_BLACKxFELON_NE, model_poverty_BLACKxMIS_NE, model_poverty_BLACKxRURAL_NE)

stargazer::stargazer(
... = models,
	covariate.labels = names,
	type = 'html',
	title = 'Regression on Above-Poverty Rate, No Exclusion',
	out = "assets/REG_poverty_NO_EXL.html")


# With exclusions (NE = NO EXLUSION)

model_poverty_WE <- glm(outcome ~ . - weight,
             data = reg_data_RACExSEX_on_poverty_with_exlusion,
             weights = weight,
             family = "binomial")

model_poverty_BLACKxFELON_WE <- glm(outcome ~ race_2 * event_type_felony + . - weight,
             data = reg_data_RACExSEX_on_poverty_with_exlusion,
             weights = weight,
             family = "binomial")

model_poverty_BLACKxMIS_WE <- glm(outcome ~ race_2 * event_type_misdemeanor + . - weight,
             data = reg_data_RACExSEX_on_poverty_with_exlusion,
             weights = weight,
             family = "binomial")

model_poverty_BLACKxRURAL_WE <- glm(outcome ~ race_2 * rural + . - weight,
             data = reg_data_RACExSEX_on_poverty_with_exlusion,
             weights = weight,
             family = "binomial")

models <- list(model_poverty_WE, model_poverty_BLACKxFELON_WE, model_poverty_BLACKxMIS_WE, model_poverty_BLACKxRURAL_WE)


stargazer::stargazer(
... = models,
	covariate.labels = names,
	type = 'html',
	title = 'Regression Above-Poverty Rate, With Exclusion (42101, 42103)',
	out = "assets/REG_poverty_WITH_EXL.html")


















# other -------------------------------------------------------------------


test <- GenerateRegressionData(df_wider, grouping_indexes = c(4, 5), outcome_variable = 'any_w2', exclude_fips = c())

disag_sex_race <- df_wider |> 
	filter(
		fips != '42101',
		fips != '42103') |> 
  #select(-c(6,7,8)) |> 
  filter(sex != 0 & race != 0,
  	if_all(6:8, ~ . == 0),
  	event_type %in% c('felony', 'incarceration', 'misdemeanor'),
  	!if_any(c(is_count_TRUE, is_count_FALSE), is.na),
  	variable_name == 'any_w2',
  	cohort_year %in% two_year_window_start) |> 
	rename(
		proportion = is_count_FALSE,
		denominator = is_count_TRUE
	) |> 
	mutate(
		numerator = round(proportion * denominator, 0)
	) |> 
	mutate(
		positive = numerator,
		negative = denominator - numerator
	) |> 
	group_by(
		fips, cohort_year, rural, sex, race, event_type, variable_name, years_post
	) |> 
	summarise(
		denominator = sum(denominator),
		positive = sum(positive),
		negative = sum(negative),
		proportion = ifelse(denominator == 0, proportion, sum(positive) /  sum(denominator))
	) |> 
	fastDummies::dummy_cols(select_columns = c("years_post"), remove_selected_columns = T, remove_most_frequent_dummy = T) 

unique(disag_sex_race$cohort_year)

regression_data <- disag_sex_race |> 
	select(2:6, 9, 10, 12:13) |> 
	pivot_longer(cols = c('positive', 'negative'), names_to = 'name', values_to = 'weight') |> 
	mutate(employed = ifelse(name == 'positive', 1, 0)) |> 
  dummy_cols(select_columns = c("cohort_year"), remove_selected_columns = T, remove_first_dummy =  TRUE) |> 
	dummy_cols(select_columns = c("event_type"), remove_selected_columns = T, remove_most_frequent_dummy = TRUE) |> 
  dummy_cols(select_columns = c("sex"), remove_selected_columns = T, remove_most_frequent_dummy = TRUE) |> 
  dummy_cols(select_columns = c("race"), remove_selected_columns = T, remove_first_dummy =  TRUE) |> 
	select(contains('cohort'), rural, years_post_3, years_post_5, event_type_felony, event_type_misdemeanor, sex_2, race_2, race_3, race_4, race_5, employed, weight) |> 
	mutate(across(1:19, as.factor))


test <- regression_data |> 
	group_by(employed, ) |> 
	summarise(sum = sum(weight)) |> 
	ggplot(aes(x = employed, y = sum)) +
	geom_bar(stat = 'identity')
	
# logit -------------------------------------------------------------------
options(scipen = 999)
library(caret)
df <- regression_data[, ]

model <- glm(employed ~ race_2 * rural + . - weight,
             data = df,
             weights = weight,
             family = "binomial")

model$coefficients
summary(model)
model_coeffs <-model$coefficients
model_coeffs_exp <- exp(model_coeffs)
model$coefficients <- model_coeffs_exp

labels <- c(
	"2002", "2004", "2006", 
	"2008", "2010", "2012", 
	"2014", "2016", "Rural", 
	"Three Years After", "Two Years After", 
	"Felony", "Misdemeanor", "Female",
	"Black", "Asian or Pacific Islander",
	"Hispanic",
	"AIAN"
	)

notes <- "Reference variables are: 2000, Urban, One Year After, Incarceration, Male."
stargazer::stargazer(model, type = 'text',
	covariate.labels = labels,
	notes = notes,
	align = T,
	title = "Logistic Regression on Employement"
	#out = "assets/LogitModelDefault.html"
	)

exp(model$coefficients)

confint(model)
df$predicted_prob <- predict(model, type = "response")
df$predicted_employed <- ifelse(df$predicted_prob >= 0.5, 1, 0)

# Convert employed to numeric for comparison
df$actual_employed <- as.numeric(as.character(df$employed))

# Calculate weighted accuracy
weighted_accuracy <- sum(df$weight * (df$actual_employed == df$predicted_employed)) / sum(df$weight)
weighted_accuracy

# -------------------------------------------------------------------------

zzzz <- proc_df |> 
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
  ) |> 
		filter(sex != 0, race != 0,
  		variable_name == "w2_wages")
zzzz <- zzzz |> 
			filter(sex != 0, race != 0,
  		variable_name == "w2_wages")
