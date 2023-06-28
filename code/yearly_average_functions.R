get_yearly_differences <- function(regions){
  alldat_region <- left_join(vis_year, coh) |> left_join(regions) |> left_join(pop_sizes)
  region_totals_cont <- pop_sizes |> 
    left_join(regions) |> 
    filter(!STATE %in% c("Hawaii", "Alaska")) |>
    group_by(part) |>
    summarize(region_total = sum(POPSIZE))
  
  regions_weighted_allyears <- alldat_region |> 
    filter(!STATE %in% c("Hawaii", "Alaska")) |>
    left_join(region_totals_cont) |> 
    mutate(pop_frac = POPSIZE/region_total) |>
    mutate(group_weight = pop_frac/NMEMB) |>
    mutate(weighted_cases = yearly_visits*group_weight) |>
    group_by(part, YEAR) |>
    summarize(sum_region_cases = sum(weighted_cases)) |>
    mutate(sum_region_cases_per_thousand = sum_region_cases*1000)
  
  return(regions_weighted_allyears)
}

region_ttest_yearly <- function(region1, region2, regions_weighted_allyears){
  df_r1 <- regions_weighted_allyears |>
    filter(Region == region1) 
  
  df_r2 <- regions_weighted_allyears |>
    filter(Region == region2)
  
  t.test(df_r1$sum_region_cases_per_thousand, 
         df_r2$sum_region_cases_per_thousand)
}


#make dataframe and perform each t-test
# ttest_table <- expand_grid(region1 = c("South", "Midwest", "Northeast", "West"),
#                            region2 = c("South", "Midwest", "Northeast", "West")) |>
#   filter(region1 != region2) |>
#   mutate(Comparison = paste0(region1, "-", region2)) |>
#   filter(!Comparison %in% c("Midwest-South", "Northeast-South", "Northeast-Midwest",
#                             "West-South", "West-Midwest", "West-Northeast"))



fill_table <- function(ttest_table, regions_weighted_allyears, avg_yearly_regions){
  pvals = c()
  for(i in 1:nrow(ttest_table)){
    pval <- region_ttest_yearly(ttest_table$region1[i], ttest_table$region2[i],
                                regions_weighted_allyears)$p.value
    pvals <- append(pvals, pval)
    
  }
  ttest_table["pvals"] <- pvals
  diff <- c()
  for (i in 1:nrow(ttest_table)){
    d <- as.numeric(avg_yearly_regions[which(avg_yearly_regions$Region == ttest_table$region1[i]),2] -
                      avg_yearly_regions[which(avg_yearly_regions$Region == ttest_table$region2[i]),2])
    diff <- append(diff, d)
  }
  
  ttest_table["diff"] <- diff
  return(ttest_table)
}


#add differences to table


# region_comparisons <- ttest_table |>
#   ggplot(aes(x = -log(pvals), y = diff, shape = Comparison)) +
#   geom_point() +
#   geom_vline(xintercept = -log(0.05/6), linetype = 2, color = "red") +
#   xlab("Negative Log P-value") +
#   ylab("Difference in Visits per 1000") +
#   ggtitle("Streptococcal Pharyngitis Average Yearly Region Comparisons") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5, size = 18))

