
## Alexander Markel
## AM97354@wcupa.edu
## 4/17/2025

## TO BE USED IN CONJUCTION WITH `Table Builder.R` script

# Generate Rural vs Urban Subsets -----------------------------------------


subsets_econ <- CreateSubsets(data = master,
              year_start_INCLUSIVE = 2010,
              year_end_INCLUSIVE = 2017,
	             variable_group = 'socioeconomic',
              event_types = c('felony', 'incarceration', 'misdemeanor'),
	county_fips_A = 'rural',
	county_fips_B = 'urban',
              exclude_fips = c('42101', '42003'),
              no_breakdowns = F)



subsets_health <- CreateSubsets(data = master,
              year_start_INCLUSIVE = 2010,
              year_end_INCLUSIVE = 2017,
	             variable_group = 'health',
              event_types = c('felony', 'incarceration', 'misdemeanor'),
	county_fips_A = 'rural',
	county_fips_B = 'urban',
              exclude_fips = c('42101', '42003'),
              no_breakdowns = F)



# ECONOMICS ---------------------------------------------------------------
test <- econ_race$year_1$felony

test2 <- test %>%
  pivot_wider(
    id_cols = c(demo_group, Statistic),
    names_from = rural,
    values_from = c(above_poverty, any_w2, hud, w2_wages),
    names_glue = "rural{rural}_{.value}"   # put rural first in name
  ) %>%
  relocate(starts_with("rural0"), .after = Statistic) %>%  # all rural first
  relocate(starts_with("rural1"), .after = last_col())     # then all urban
# BY RACE -----------------------------------------------------------------

econ_race <- GenerateTableData(
	subset = subsets_econ,
	breakdown_variables = c('race', "age_group"),
	breakdown_limits = c('012', "0123"), # white black hispanic
	outlook_years = c(1),
  first_row_ungroup = TRUE)



# FOR RACE
econ_race$year_1$felony[, -c(1, 2)] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 8, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 9, 16, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('White', 3, 4)|> 
    group_rows('Black', 5, 6)|>
		group_rows('Hispanic', 7, 8) |> 
    group_rows('All', 9, 10)|> 
    group_rows('White', 11, 12)|> 
    group_rows('Black', 13, 14)|> 
		group_rows("Hispanic", 15, 16) |> 
    add_header_above(c('Economic Outcomes' = 5)) |> 
  	save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_race_FELONS.html')

econ_race$year_1$incarceration[, -c(1, 2)] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 8, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 9, 16, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('White', 3, 4)|> 
    group_rows('Black', 5, 6)|>
		group_rows('Hispanic', 7, 8) |> 
    group_rows('All', 9, 10)|> 
    group_rows('White', 11, 12)|> 
    group_rows('Black', 13, 14)|> 
		group_rows("Hispanic", 15, 16) |> 
    add_header_above(c('Economic Outcomes' = 5))|> 
  	save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_race_INCARCERATIONS.html')
econ_race$year_1$misdemeanor[, -c(1, 2)] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 8, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 9, 16, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('White', 3, 4)|> 
    group_rows('Black', 5, 6)|>
		group_rows('Hispanic', 7, 8) |> 
    group_rows('All', 9, 10)|> 
    group_rows('White', 11, 12)|> 
    group_rows('Black', 13, 14)|> 
		group_rows("Hispanic", 15, 16) |> 
    add_header_above(c('Economic Outcomes' = 5))|> 
  	save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_race_MISDEMEANORS.html')



# BY AGE ------------------------------------------------------------------

econ_age <- GenerateTableData(
	subset = subsets_econ,
	breakdown_variables = c('age_group'),
	breakdown_limits = c('0123'), 
	outlook_years = c(1),
  first_row_ungroup = TRUE)

econ_age$year_1$felony[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 8, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 9, 16, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('15-24', 3, 4)|> 
    group_rows('25-39', 5, 6)|>
		group_rows('40+', 7, 8) |> 
    group_rows('All', 9, 10)|> 
    group_rows('15-24', 11, 12)|> 
    group_rows('25-39', 13, 14)|> 
		group_rows("40+", 15, 16) |> 
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_age_FELONY.html')

econ_age$year_1$incarceration[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 8, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 9, 16, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('15-24', 3, 4)|> 
    group_rows('25-39', 5, 6)|>
		group_rows('40+', 7, 8) |> 
    group_rows('All', 9, 10)|> 
    group_rows('15-24', 11, 12)|> 
    group_rows('25-39', 13, 14)|> 
		group_rows("40+", 15, 16) |> 
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_age_INCARCERATION.html')

econ_age$year_1$misdemeanor[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 8, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 9, 16, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('15-24', 3, 4)|> 
    group_rows('25-39', 5, 6)|>
		group_rows('40+', 7, 8) |> 
    group_rows('All', 9, 10)|> 
    group_rows('15-24', 11, 12)|> 
    group_rows('25-39', 13, 14)|> 
		group_rows("40+", 15, 16) |> 
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_age_MISDEMEANOR.html')



# BY SEX --------------------------------------------------------------

econ_gender <- GenerateTableData(
	subset = subsets_econ,
	breakdown_variables = c('sex'),
	breakdown_limits = c('012'), 
	outlook_years = c(1),
  first_row_ungroup = TRUE)

econ_gender$year_1$felony[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 6, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 7, 12, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('Male', 3, 4)|> 
    group_rows('Female', 5, 6)|>
	  group_rows('All', 7, 8)|>
 	  group_rows('Male', 9, 10)|>
 	  group_rows('Female', 11, 12)|>
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_sex_FELONY.html')

econ_gender$year_1$incarceration[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 6, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 7, 12, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('Male', 3, 4)|> 
    group_rows('Female', 5, 6)|>
	  group_rows('All', 7, 8)|>
	  group_rows('Male', 9, 10)|>
	  group_rows('Female', 11, 12)|>
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_sex_INCARCERATION.html')

econ_gender$year_1$misdemeanor[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 6, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 7, 12, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('Male', 3, 4)|> 
    group_rows('Female', 5, 6)|>
	  group_rows('All', 7, 8)|>
	  group_rows('Male', 9, 10)|>
	  group_rows('Female', 11, 12)|>
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_sex_MISDEMEANOR.html')



#  BY RACE + AGE ----------------------------------------------------------

econ_race_age <- GenerateTableData(
	subset = subsets_econ,
	breakdown_variables = c('race', 'age_group'),
	breakdown_limits = c('0124', '01'), 
	outlook_years = c(1),
  first_row_ungroup = TRUE)

econ_race_age$year_1$felony[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 14, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 15, 28, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('White', 3, 4)|> 
    group_rows('Young White', 5, 6)|>
	  group_rows('Black', 7, 8)|>
 	  group_rows('Young Black', 9, 10)|>
 	  group_rows('Hispanic', 11, 12)|>
    group_rows('Young Hispanic', 13, 14)|>
  	group_rows('All', 15, 16)|>
 	  group_rows('White', 17, 18)|>
  	group_rows('Young White', 19, 20)|>
  	group_rows('Black', 21, 22)|>
  	group_rows('Young Black', 23, 24)|>
  	group_rows('Hispanic', 25, 26)|>
  	group_rows('Young Hispanic', 27, 28)|>
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_race_age_FELONY.html')

econ_race_age$year_1$incarceration[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 14, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 15, 28, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('White', 3, 4)|> 
    group_rows('Young White', 5, 6)|>
	  group_rows('Black', 7, 8)|>
 	  group_rows('Young Black', 9, 10)|>
 	  group_rows('Hispanic', 11, 12)|>
    group_rows('Young Hispanic', 13, 14)|>
  	group_rows('All', 15, 16)|>
 	  group_rows('White', 17, 18)|>
  	group_rows('Young White', 19, 20)|>
  	group_rows('Black', 21, 22)|>
  	group_rows('Young Black', 23, 24)|>
  	group_rows('Hispanic', 25, 26)|>
  	group_rows('Young Hispanic', 27, 28)|>
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_race_age_INCARCERATION.html')

econ_race_age$year_1$misdemeanor[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 14, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 15, 28, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('White', 3, 4)|> 
    group_rows('Young White', 5, 6)|>
	  group_rows('Black', 7, 8)|>
 	  group_rows('Young Black', 9, 10)|>
 	  group_rows('Hispanic', 11, 12)|>
    group_rows('Young Hispanic', 13, 14)|>
  	group_rows('All', 15, 16)|>
 	  group_rows('White', 17, 18)|>
  	group_rows('Young White', 19, 20)|>
  	group_rows('Black', 21, 22)|>
  	group_rows('Young Black', 23, 24)|>
  	group_rows('Hispanic', 25, 26)|>
  	group_rows('Young Hispanic', 27, 28)|>
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_race_age_MISDEMEANOR.html')



# BY RACE + SEX -----------------------------------------------------------

econ_race_sex <- GenerateTableData(
	subset = subsets_econ,
	breakdown_variables = c('race', 'sex'),
	breakdown_limits = c('0124', '012'), 
	outlook_years = c(1),
  first_row_ungroup = TRUE)


econ_race_sex$year_1$felony[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 20, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 21, 40, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('White', 3, 4)|> 
    group_rows('Male White', 5, 6)|>
	  group_rows('Female White', 7, 8)|>
 	  group_rows('Black', 9, 10)|>
 	  group_rows('Male Black', 11, 12)|>
    group_rows('Female Black', 13, 14)|>
  	group_rows('Hispanic', 15, 16)|>
 	  group_rows('Male Hispanic', 17, 18)|>
  	group_rows('Female Hispanic', 19, 20)|>
  	group_rows('All', 21, 22)|>
  	group_rows('White', 23, 24)|>
  	group_rows('Male White', 25, 26)|>
  	group_rows('Female White', 27, 28)|>
	  group_rows('Black', 29, 30)|>
  	group_rows('Male Black', 31, 32)|>
	  group_rows('Female Black', 33, 34)|>
	  group_rows('Hispanic', 35, 36)|>
	  group_rows('Male Hispanic', 37, 38)|>
	  group_rows('Female Hispanic', 39, 40)|>
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_race_sex_FELONY.html')

econ_race_sex$year_1$incarceration[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 20, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 21, 40, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('White', 3, 4)|> 
    group_rows('Male White', 5, 6)|>
	  group_rows('Female White', 7, 8)|>
 	  group_rows('Black', 9, 10)|>
 	  group_rows('Male Black', 11, 12)|>
    group_rows('Female Black', 13, 14)|>
  	group_rows('Hispanic', 15, 16)|>
 	  group_rows('Male Hispanic', 17, 18)|>
  	group_rows('Female Hispanic', 19, 20)|>
  	group_rows('All', 21, 22)|>
  	group_rows('White', 23, 24)|>
  	group_rows('Male White', 25, 26)|>
  	group_rows('Female White', 27, 28)|>
	  group_rows('Black', 29, 30)|>
  	group_rows('Male Black', 31, 32)|>
	  group_rows('Female Black', 33, 34)|>
	  group_rows('Hispanic', 35, 36)|>
	  group_rows('Male Hispanic', 37, 38)|>
	  group_rows('Female Hispanic', 39, 40)|>
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_race_sex_INCARCERATION.html')

econ_race_sex$year_1$misdemeanor[, -c(1,2 )] |> 
	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |> 
    group_rows('Urban', 1, 20, label_row_css = "border-bottom: 2px solid; font-size: 1.2em;") |> 
    group_rows('Rural', 21, 40, label_row_css = "border-bottom: 2px solid; padding-top: 0.5em; font-size: 1.2em;")|> 
    group_rows('All', 1, 2)|> 
    group_rows('White', 3, 4)|> 
    group_rows('Male White', 5, 6)|>
	  group_rows('Female White', 7, 8)|>
 	  group_rows('Black', 9, 10)|>
 	  group_rows('Male Black', 11, 12)|>
    group_rows('Female Black', 13, 14)|>
  	group_rows('Hispanic', 15, 16)|>
 	  group_rows('Male Hispanic', 17, 18)|>
  	group_rows('Female Hispanic', 19, 20)|>
  	group_rows('All', 21, 22)|>
  	group_rows('White', 23, 24)|>
  	group_rows('Male White', 25, 26)|>
  	group_rows('Female White', 27, 28)|>
	  group_rows('Black', 29, 30)|>
  	group_rows('Male Black', 31, 32)|>
	  group_rows('Female Black', 33, 34)|>
	  group_rows('Hispanic', 35, 36)|>
	  group_rows('Male Hispanic', 37, 38)|>
	  group_rows('Female Hispanic', 39, 40)|>
    add_header_above(c('Economic Outcomes' = 5)) |>
    save_kable(file = 'G:/JOE/JOE_R/figures/Week 11/Summary Statistics/rural_vs_urban_ECON_race_sex_MISDEMEANOR.html')


# TIMESERIES --------------------------------------------------------------

fe <- subsets_econ$felony |> 
	filter(demo_group != '0-0-0-0-0')
inc <- subsets_econ$incarceration |> 
	filter(demo_group != '0-0-0-0-0')
mi <- subsets_econ$misdemeanor |> 
	filter(demo_group != '0-0-0-0-0')


plotting <- function(
		data,
	  subpopulation = '0-1-0-0-0',
		crime_event_type  = 'felony', 
  	outcome_variable_name = 'above_poverty',
	  outlook_years = c(1, 3),
	  exclude_fips = c('42101', '42003')
) {
	
	subpopulation_sex            = substr(subpopulation, 1, 1)
  subpopulation_race           = substr(subpopulation, 3, 3)
  subpopulation_age_group      = substr(subpopulation, 5, 5)
  subpopulation_off_type       = substr(subpopulation, 7, 7)
  subpopulation_repeat_contact = substr(subpopulation, 9, 9)
  
  print(length(unique(data$fips)))
  
  plotting_data <- data |> 
  	#filter(!is_count) |> 
  	filter(fips %ni% exclude_fips) |> 
  	filter(event_type == crime_event_type) |> 
    filter(variable_name %in% outcome_variable_name) |> 
    filter(years_post %in% outlook_years) |> 
    filter(
    sex == subpopulation_sex,
    race == subpopulation_race,
    age_group == subpopulation_age_group,
    off_type == subpopulation_off_type,
    repeat_contact == subpopulation_repeat_contact
    	)  |> 	mutate(
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
  group_by(cohort_year, rural, years_post) |> 
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
  		 	) 
  
  print(head(plotting_data))
  	
  
ggplot(plotting_data, aes(x = cohort_year, y = Rate)) + 
  geom_line(aes(color = as.factor(rural)), linewidth = 1) + 
  facet_grid(~years_post, labeller = labeller(years_post = c("1" = "1 Year Post", "3" = "3 Years Post"))) + 
  geom_point() +
  scale_y_continuous(breaks = seq(0, 1, 0.01)) + 
  scale_color_manual(values = c('red', 'blue'), 
                     labels = c("Urban", "Rural"),
                     name = "Geography") +
  theme_bw() +
  labs(x = "Cohort Year", y = "Rate",
  	title = {{ outcome_variable_name }})
	
	
}


plotting(data = mi, '0-1-0-0-0',
	outcome_variable_name = 'hud',
	crime_event_type = 'misdemeanor')	
	
	
	
	
	
	
	
	
	
	