urban_fips = df_wider |> 
	filter(rural == FALSE) |> 
	select(fips) |> 
	unique() |> 
	pull()

rural_fips = df_wider |> 
	filter(rural == TRUE) |> 
	select(fips) |> 
	unique() |> 
	pull()


reg_RACExSEX_incarc <- 	GenerateRegressionData(
		data = df_wider,
		grouping_indexes = c(4, 5),
		outcome_variable = "to_fe_recid",
		event_types = c("incarceration"),
		split_years = T,
		fips_as_dummies = c("42101", "42003"),
		exclude_fips = c())

sum(reg_RACExSEX_incarc$year_1$weight)
sum(reg_RACExSEX_incarc$year_3$weight)
sum(reg_RACExSEX_incarc$year_5$weight)

reg_RACExAGE_felon_mis <- 	GenerateRegressionData(
		data = df_wider,
		grouping_indexes = c(5, 4),
		outcome_variable = "any_w2",
		event_types = c("felony", "misdemeanor"),
		split_years = T,
		fips_as_dummies = c("42101", "42003"),
		exclude_fips = c())

sum(reg_RACExSEX_felon_mis$year_1$weight)
sum(reg_RACExSEX_felon_mis$year_3$weight)
sum(reg_RACExSEX_felon_mis$year_5$weight)


summary(model_Y1)
# -------------------------------------------------------------------------

# RACE+AGE
base_formula <- "outcome ~ rural + race_2 + race_3 + race_4 + race_5 + age_group_2 + age_group_3 + event_type_misdemeanor + cohort_year_2012 + cohort_year_2014 + cohort_year_2016 + PHL_AL + race_2:age_group_2 + race_4:age_group_2 + race_2:age_group_3 + race_4:age_group_3"

# RACE + OFF_TYPE
base_formula <- "outcome ~ rural + race_2 + race_3 + race_4 + race_5 + off_type_2 + off_type_3 + event_type_misdemeanor + cohort_year_2012 + cohort_year_2014 + cohort_year_2016 + PHL_AL + race_2:off_type_2 + race_2:off_type_3 + race_4:off_type_2 + race_4:off_type_3"

# RACE + REPEAT
base_formula <- "outcome ~ rural + race_2 + race_3 + race_4 + race_5 + repeat_contact_2  + event_type_misdemeanor + cohort_year_2016 + PHL_AL + race_2:repeat_contact_2 + race_4:repeat_contact_2"

# RACE + SEX
base_formula <- "outcome ~ rural + sex_2 + race_2 + race_3 + race_4 + race_5 +  cohort_year_2012 + cohort_year_2014 + cohort_year_2016 +  PHL_AL + race_2:sex_2 + race_4:sex_2"

get_stargazer_labels <- function(var_names) {
  replacements <- c(
    rural1 = "Rural",
    race_21 = "Black",
    race_31 = "Asian or Pacific Islander",
    race_41 = "Hispanic",
    race_51 = "AIAN",
    age_group_21 = "25–39",
    age_group_31 = "40+",
    event_type_felony1 = "Felony",
    cohort_year_20121 = "2012",
    cohort_year_20141 = "2014",
    cohort_year_20161 = "2016",
    PHL_AL1 = "Philly / Allegheny",
  	`race_21:age_group_31` = "Black*40+",
  	`race_41:age_group_31` = "Hispanic*40+",
  	`race_21:age_group_21` = "Black*25-39",
  	`race_41:age_group_21` = "Hispanic*25-39",
  	event_type_felony1  = "Felony",
  	event_type_misdemeanor1  = "Misdemeanor",
  	off_type_11 = "Violent Off.",
  	off_type_21 ="Property Off.",
  	off_type_31 ="Drug Off.",
  	 `race_21:off_type_21` = "Black*Property",
  	 `race_41:off_type_21` = "Hispanic*Property",
  	
  	 `race_21:off_type_31` = "Black*Drug",
  	 `race_41:off_type_31` = "Hispanic*Drug",
  	
  	repeat_contact_21 = "Repeat Contact",
  	`race_21:repeat_contact_21` = "Black*RepeatContact",
  	`race_41:repeat_contact_21` = "Hispanic*RepeatContact",
  	
  	sex_21 = "Female",
  	`sex_21:race_21` = "Black*Female",
  	`sex_21:race_41` = "Hispanic*Female"
  )
  
  out <- replacements[var_names]          # Try to map each var name
  out[is.na(out)] <- var_names[is.na(out)]  # Keep original name if not found
  out
}


ProduceRegressionOutputs <- function(type = "all") {

# Pick the base forumula above based on which two variables you are grouping by.
base_formula <- "outcome ~ rural + race_2 + race_3 + race_4 + race_5 + repeat_contact_2  + event_type_misdemeanor + cohort_year_2016 + PHL_AL + race_2:repeat_contact_2 + race_4:repeat_contact_2"


# Years you want to loop over
year_list <- c("year_1", "year_3", "year_5")

# To do all of them, un-comment the character vector below.
#outcome_variables <-  c("any_w2", "above_poverty", "mortality", "medicaid", "medicare", "to_fe_recid")

# To do one of them, use the below line and pick the outcome variable.
outcome_variables <-  c("to_mi_recid")

# Store models in a list
models <- list()
outputs <- list()
years_combined <- list()

for (var in outcome_variables) {
	title_variable = case_when(
		var == "any_w2" ~ "Employment Rate",
		var == "above_poverty" ~ "Above Poverty Rate",
		var == "mortality" ~ "Mortality Rate",
		var == "medicaid" ~ "Medcaid Uptake Rate",
		var == "medicare" ~ "Medicare Uptake Rate",
		var == "to_fe_recid" ~ "Felony Redicivism",
		var == "to_mi_recid" ~ "Misdemeanor Recidivism"
	)
	if(type=="all") {
		
base_formula <- "outcome ~ rural + race_2 + race_3 + race_4 + race_5 + off_type_2 + off_type_3 + event_type_misdemeanor + cohort_year_2012 + cohort_year_2014 + cohort_year_2016 + PHL_AL + race_2:off_type_2 + race_2:off_type_3 + race_4:off_type_2 + race_4:off_type_3"

		reg_RACExAGE_felon_mis <- GenerateRegressionData(
		data = df_wider,
		grouping_indexes = c(5, 8),
		outcome_variable = var,
		event_types = c("felony", "misdemeanor"),
		split_years = T,
		fips_as_dummies = c("42101", "42003"),
		exclude_fips = c())
		
		
	} else if(type=="rural") {
		
base_formula <- "outcome ~ race_2 + race_3 + race_4 + race_5 + age_group_2 + age_group_3 + event_type_misdemeanor + cohort_year_2012 + cohort_year_2014 + cohort_year_2016 + race_2:age_group_2 + race_4:age_group_2 + race_2:age_group_3 + race_4:age_group_3"

		reg_RACExAGE_felon_mis <- GenerateRegressionData(
		data = df_wider,
		grouping_indexes = c(5, 8),
		outcome_variable = var,
		event_types = c("felony", "misdemeanor"),
		split_years = T,
		fips_as_dummies = c(),
		exclude_fips = c(urban_fips))
		
	} else if(type=="urban"){
		
base_formula <- "outcome ~ race_2 + race_3 + race_4 + race_5 + age_group_2 + age_group_3 + event_type_misdemeanor + cohort_year_2012 + cohort_year_2014 + cohort_year_2016 + race_2:age_group_2 + race_4:age_group_2 + race_2:age_group_3 + race_4:age_group_3"
		
		reg_RACExAGE_felon_mis <- GenerateRegressionData(
		data = df_wider,
		grouping_indexes = c(5, 8),
		outcome_variable = var,
		event_types = c("felony", "misdemeanor"),
		split_years = T,
		fips_as_dummies = c(),
		exclude_fips = c(rural_fips))
		
	}
	
	
	Y1_weight <- sum(reg_RACExAGE_felon_mis$year_1$weight)
  Y3_weight <- sum(reg_RACExAGE_felon_mis$year_3$weight)
  Y5_weight <- sum(reg_RACExAGE_felon_mis$year_5$weight)
	weights_used = paste0("[", Y1_weight, ",", Y3_weight, ",",Y5_weight,"]")
	term_list <- attr(terms(as.formula(base_formula)), "term.labels")
	print(term_list)
	
	for (year in year_list) {
	  
  # Get the dataset for this year
  data_year <- reg_RACExAGE_felon_mis[[year]]
  
  # Get the variable names in the data
  data_vars <- colnames(data_year)
  
  # Keep only terms where all variables exist in the data
  terms_filtered <- term_list[sapply(term_list, function(term) {
    vars_in_term <- all.vars(as.formula(paste("~", term)))
    all(vars_in_term %in% data_vars)
  })]
  
  # Reconstruct the formula
  formula_str <- paste("outcome ~", paste(terms_filtered, collapse = " + "))
  print("HERE")
  print(formula_str)
  formula_final <- as.formula(formula_str)
  
	print(str(data_year))
  model <- glm(formula_final,
               data = data_year,
               weights = weight,
               family = "binomial")
  
  years_combined[[paste0(year)]] <- model
	}
	
	labels = get_stargazer_labels(names(coef(years_combined[["year_1"]]))[-1])
	print(labels)
	
  out_string = paste0("assets/regressions/exponentiated/", "race_repeat/","REG_", var,"_FELON_MISDEMEANOR_INTERACTIONS_", type, ".html")
  print(out_string)
  title_str = paste0("Regression on ", title_variable)
  
  html_output = stargazer::stargazer(
  	years_combined,
  	title = title_str,
  	type = "html",
  	out = out_string,
  	covariate.labels = labels, 
  	column.labels = c("Year 1", "Year 3", "Year 5"),
  	apply.coef = exp,
  	t.auto = FALSE,
  	p.auto = FALSE,
  	notes = paste("LOGIT coefficients are exponentiated. Weights:", weights_used,".","Counties: ",type))
  # Save the model
  models[[paste0(var)]] <- years_combined
  outputs[[paste0(var)]] <- html_output
	

}

	return(list(models = models, outputs = outputs))
}

models <- ProduceRegressionOutputs(type = "rural")



# -------------------------------------------------------------------------

 replacements <- c(
    rural1 = "Rural",
    race_21 = "Black",
    race_31 = "Asian or Pacific Islander",
    race_41 = "Hispanic",
    race_51 = "AIAN",
    age_group_21 = "25–39",
    age_group_31 = "40+",
    event_type_felony1 = "Felony",
    cohort_year_20121 = "2012",
    cohort_year_20141 = "2014",
    cohort_year_20161 = "2016",
    PHL_AL1 = "Philly / Allegheny",
  	`race_21:age_group_31` = "Black*40+",
  	`race_41:age_group_31` = "Hispanic*40+",
  	`race_21:age_group_21` = "Black*25-39",
  	`race_41:age_group_21` = "Hispanic*25-39",
  	event_type_felony1  = "Felony",
  	event_type_misdemeanor1  = "Misdemeanor",
  	off_type_11 = "Violent Off.",
  	off_type_21 ="Property Off.",
  	 `race_21:off_type_11` = "Black*Violent",
  	 `race_21:off_type_21` = "Black*Property",
  	 `race_41:off_type_11` = "Hispanic*Violent",
  	 `race_41:off_type_21` = "Hispanic*Property"
  )
  
  out <- replacements[var_names]          # Try to map each var name
  out[is.na(out)] <- var_names[is.na(out)]  # Keep original name if not found
  out



base_formula <- "outcome ~ rural + race_2 + race_3 + race_4 + race_5 + repeat_contact_2  + event_type_misdemeanor + cohort_year_2016 + PHL_AL + race_2:repeat_contact_2 + race_4:repeat_contact_2"

summary(model)
model <-  glm(outcome ~ rural + race_2 + race_3 + race_4 + race_5 + repeat_contact_2  + event_type_misdemeanor + cohort_year_2016 + PHL_AL + race_2:repeat_contact_2 + race_4:repeat_contact_2,
               data = reg_RACExAGE_felon_mis$year_1,
               weights = weight,
               family = "binomial")
summary(model)

var_names <- names(coef(model))[-1]


stargazer::stargazer(
  	model,
  	type = "text",
  	covariate.labels = out, 
  	column.labels = c("Year 1", "Year 3", "Year 5"),
  	apply.coef = exp,
  	t.auto = FALSE,
  	p.auto = FALSE,
  	notes = "LOGIT coefficients are exponentiated.")
