test3 <- test %>%
  pivot_wider(
    id_cols = c(demo_group, Statistic),
    names_from = rural,
    values_from = c(above_poverty, any_w2, hud, w2_wages),
    names_glue = "rural{rural}_{.value}"
  ) %>%
  relocate(starts_with("rural0"), .after = Statistic) %>%
  relocate(starts_with("rural1"), .after = last_col()) %>%
  group_by(demo_group) %>%
  mutate(
    # Total population (in thousands of individuals)
    total_pop = sum(if_else(Statistic == "Thousands of Individuals",
                            rural0_above_poverty + rural1_above_poverty, 0)) * 1000,

    # Weighted rate only when row is "Rate"
    all_rate = if_else(
      Statistic == "Rate",
      (
        rural0_above_poverty * (first(rural0_above_poverty[Statistic == "Thousands of Individuals"]) * 1000) +
        rural1_above_poverty * (first(rural1_above_poverty[Statistic == "Thousands of Individuals"]) * 1000)
      ) / total_pop,
      NA_real_
    ),

    # Add "all" counts (only makes sense for Thousands of Individuals)
    all_thousands = if_else(
      Statistic == "Thousands of Individuals",
      rural0_above_poverty + rural1_above_poverty,
      NA_real_
    )
  ) %>%
  ungroup()



names(test3) <- c(
  "demo_group", "Statistic",
  "Above Poverty", "Employment", "HUD", "Wages",
  "Above Poverty", "Employment", "HUD", "Wages"
)

# RACE ONLY
test3[,-c(1)] |> 
	
  	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |>  
  
  # Top-level grouping (based on demo_group blocks)
  group_rows('All', 1, 2, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
  group_rows('White', 3, 4, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
  group_rows('Black', 5, 6, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
  group_rows('Hispanic', 7, 8, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
  
  # Add header groups
  add_header_above(c(
    " " = 1,                       # demo_group + Statistic
    "Rural Outcomes" = 4, 
    "Urban Outcomes" = 4
  ))


# AGE RACE
test3[,-c(1)] |> 
	
  	kable(caption = 'Data for the years 2010-2017, for values measured one years after initial contact.',
          align = 'lrrrrrr', format.args = list(big.mark = ",")) |> 
    kable_minimal(full_width = F) |>  
  
  # Top-level grouping (based on demo_group blocks)
  group_rows('All', 1, 2, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
	group_rows('All White', 3, 4, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
  group_rows('15-24 White', 5, 6, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
  group_rows('25-39 White', 7, 8, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
  group_rows('40+ White', 9, 10, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
 	group_rows('All Black', 11,12,
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
  group_rows('15-24 Black', 13,14,
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
  group_rows('25-39 Black', 15,16, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |> 
  group_rows('40+ Black', 17,18, 
             label_row_css = "border-bottom: 2px solid; font-size: 1.1em;") |>  
  # Add header groups
  add_header_above(c(
    " " = 1,                       # demo_group + Statistic
    "Rural Outcomes" = 4, 
    "Urban Outcomes" = 4
  ))
