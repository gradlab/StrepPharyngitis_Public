#Functions for calculating average monthly visits by region or subregions

calculate_averages <- function(states_to_leave_out, regions){
  coh_dat_pop <- left_join(dat, coh) |> left_join(pop_sizes)
  region_totals <- pop_sizes |> 
    filter(!STATE %in% states_to_leave_out) |>
    mutate(region = tolower(STATE)) |>
    left_join(regions) |> 
    select(-region) |>
    group_by(part) |>
    summarize(region_total = sum(POPSIZE))
  
  regions_weighted_allyears <- coh_dat_pop |> 
    filter(!STATE %in% states_to_leave_out) |>
    mutate(region = tolower(STATE)) |>
    left_join(regions) |>
    left_join(region_totals) |> 
    mutate(pop_frac = POPSIZE/region_total) |>
    mutate(group_weight = pop_frac/NMEMB) |>
    mutate(weighted_cases = NVISITS*group_weight) |>
    group_by(part, YEAR, MONTH) |>
    summarize(sum_region_cases = sum(weighted_cases)) |>
    mutate(sum_region_cases_per_thousand = sum_region_cases*1000) 
  
  regions_weighted <- coh_dat_pop |> 
    filter(!STATE %in% states_to_leave_out) |>
    mutate(region = tolower(STATE)) |>
    left_join(regions) |>
    left_join(region_totals) |> 
    mutate(pop_frac = POPSIZE/region_total) |>
    mutate(group_weight = pop_frac/NMEMB) |>
    mutate(weighted_cases = NVISITS*group_weight) |>
    group_by(part, YEAR, MONTH) |>
    summarize(sum_region_cases = sum(weighted_cases)) |>
    mutate(sum_region_cases_per_thousand = sum_region_cases*1000) |>
    group_by(part, MONTH) |>
    summarize(yearly_average_sum_region_cases_per_thousand = mean(sum_region_cases_per_thousand),
              sd = sd(sum_region_cases_per_thousand))
  
  return(list(region_totals,regions_weighted_allyears,regions_weighted))
  
}




#function to plot the region difference averages
plot_averages_CIs_points <- function(regions_weighted, regions_weighted_allyears, colors_df){
  ggplot() + 
    geom_line(aes(x = MONTH, y = yearly_average_sum_region_cases_per_thousand, color = Region), data = regions_weighted) +
    geom_ribbon(aes(x  = MONTH, ymin = yearly_average_sum_region_cases_per_thousand - (1.96*sd)/sqrt(9), ymax = yearly_average_sum_region_cases_per_thousand + (1.96*sd)/sqrt(9), fill = Region), alpha = 0.2, data = regions_weighted) +
    geom_point(aes(x = MONTH, y = sum_region_cases_per_thousand, group = Region, color = Region), data = regions_weighted_allyears, size = 0.1) +
    labs(color = "Region", fill = "Region", title = "Streptococcal Pharyngitis Average Visits per Month") + 
    scale_color_manual(values= colors_df$region_color) +
    scale_fill_manual(values = colors_df$region_color) +
    ylab("Visits per 1,000 People") +
    scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title = element_text(hjust = 0.5, size = 20)) 
  
}

#function that calculates the differences between 2 specified regions given a dataframe
get_region_diff <- function(region1, region2, regions_weighted, month){
  df_r1 <- regions_weighted |>
    filter(part == region1, MONTH == month)
  df_r2 <- regions_weighted |>
    filter(part == region2, MONTH == month)
  return(df_r1$yearly_average_sum_region_cases_per_thousand - df_r2$yearly_average_sum_region_cases_per_thousand)
}

#function that tests differences between regions and months 
region_ttest <- function(region1, region2, regions_weighted_allyears, month){
  df_r1 <- regions_weighted_allyears |>
    filter(MONTH == month) |>
    filter(part == region1) 
  
  df_r2 <- regions_weighted_allyears |>
    filter(MONTH == month) |>
    filter(part == region2)
  
  t.test(df_r1$sum_region_cases_per_thousand, 
         df_r2$sum_region_cases_per_thousand)
}

get_diff_df <- function(states_to_remove, regions){
  regions_weighted <- calculate_averages(states_to_remove,regions)[[3]]
  regions_weighted_allyears <- calculate_averages(states_to_remove,regions)[[2]]
  tmp <- expand.grid(region1 = unique(regions_weighted$part), region2 = unique(regions_weighted$part))
  df2 <- data.frame()
  for(i in 1:12){
    tmp2 <- tmp |> mutate(MONTH = i)
    df2 <- rbind(df2, tmp2)
  }
  diff <- c()
  pval <- c()
  for(z in 1:nrow(df2)){
    tmp3 <- get_region_diff(df2$region1[z], df2$region2[z],regions_weighted, df2$MONTH[z])
    diff <- append(diff, tmp3)
    tmp4 <- region_ttest(df2$region1[z], df2$region2[z], regions_weighted_allyears, df2$MONTH[z])$p.value
    pval <- append(pval, tmp4)
  }
  df2 <- df2 |> 
    mutate("diff" = diff, "pval" = pval) |>
    mutate("Comparison" = paste0(str_to_title(region1), "-", str_to_title(region2)))
  return(df2)
}


















