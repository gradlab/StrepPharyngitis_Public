coh_dat_pop <- left_join(dat, coh) |> left_join(pop_sizes)

#basic age group analytics

#need to also look at the age distribution by region in the membership population by region
total_memb_byregion_age <- coh |> left_join(regions) |> filter(!STATE %in% c("Hawaii", "Alaska")) |> 
  group_by(AGEGRP, part) |>
  summarize(avg_memb_peryear = mean(NMEMB))

#calculate overall membership by region
total_memb_byregion <-total_memb_byregion_age |> group_by(part) |> summarize(total_memb = sum(avg_memb_peryear))

#weighted visits by age group
#calculate the total population in each age stratum based on the census
total_age_pop <- pop_sizes |> filter(!STATE %in% c("Hawaii", "Alaska")) |> group_by(AGEGRP) |> summarize(total_pop = sum(POPSIZE))
weighted_age_groups_permonth <- coh_dat_pop |> left_join(total_age_pop) |>
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  mutate(pop_frac = POPSIZE / total_pop) |>
  mutate(group_weight = pop_frac / NMEMB) |>
  mutate(weighted_vis = NVISITS * group_weight) |>
  group_by(MONTH, AGEGRP, YEAR) |>
  summarize(sum_weighted_vis = sum(weighted_vis)) |>
  mutate(sum_weighted_vis_per_thous = sum_weighted_vis*1000) |>
  group_by(MONTH, AGEGRP) |>
  summarize(avg_vis_per_thous = mean(sum_weighted_vis_per_thous), sd = sd(sum_weighted_vis_per_thous))

weighted_age_groups_yearly <- coh_dat_pop |> left_join(total_age_pop) |>
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  mutate(pop_frac = POPSIZE / total_pop) |>
  mutate(group_weight = pop_frac / NMEMB) |>
  mutate(weighted_vis = NVISITS * group_weight) |>
  group_by(MONTH, AGEGRP, YEAR) |>
  summarize(sum_weighted_vis = sum(weighted_vis)) |>
  mutate(sum_weighted_vis_per_thous = sum_weighted_vis*1000)


#calculate uptick dates by subregion and age group

under_19_pop <- coh_dat_pop |> filter(AGEGRP %in% c("00_04", "05_09", "10_19")) |> left_join(subregions)
under4_pop <- coh_dat_pop |> filter(AGEGRP  == "00_04") |> left_join(subregions)
fivetonine_pop <- coh_dat_pop |> filter(AGEGRP  == "05_09") |> left_join(subregions)
tentonineteen_pop <- coh_dat_pop |> filter(AGEGRP  == "10_19") |> left_join(subregions)
fivetonineteen_pop <- coh_dat_pop |> filter(AGEGRP %in% c("05_09", "10_19")) |> left_join(subregions)
over_19_pop <- coh_dat_pop |> filter(!AGEGRP %in% c("00_04", "05_09", "10_19")) |> left_join(subregions)

#need to make these by subregion instead
total_subregions_pops_under19 <- pop_sizes |> filter(AGEGRP %in% c("00_04", "05_09", "10_19")) |>
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  left_join(subregions) |> group_by(part) |>
  summarize(total_subregion_pop_under19 = sum(POPSIZE)) 

total_subregions_pops_under4 <- pop_sizes |> filter(AGEGRP  == "00_04") |>
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  left_join(subregions) |> group_by(part) |>
  summarize(total_subregion_pop_under4 = sum(POPSIZE)) 

total_subregions_pops_fivetonine <- pop_sizes |> filter(AGEGRP  == "05_09") |>
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  left_join(subregions) |> group_by(part) |>
  summarize(total_subregion_pop_fivetonine = sum(POPSIZE)) 

total_subregions_pops_tentonineteen <- pop_sizes |> filter(AGEGRP  == "10_19") |>
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  left_join(subregions) |> group_by(part) |>
  summarize(total_subregion_pop_tentonineteen = sum(POPSIZE)) 
  
total_subregions_pops_fivetonineteen <- pop_sizes |> filter(AGEGRP %in%  c("05_09", "10_19")) |>
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  left_join(subregions) |> group_by(part) |>
  summarize(total_subregion_pop_fivetonineteen = sum(POPSIZE)) 

total_subregions_pops_over19 <- pop_sizes |> filter(!AGEGRP %in% c("00_04", "05_09", "10_19")) |>
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  left_join(subregions) |> group_by(part) |>
  summarize(total_subregion_pop_over19 = sum(POPSIZE))
#get average visits in each subregion in each month over all years in those under 19 and over 19


#unweighted
under_19_pop |> group_by(MONTH, YEAR, part) |>
  summarize(nvis = sum(NVISITS), nmemb = sum(NMEMB)) |>
  mutate(vis_per_thous = nvis / nmemb * 1000)
  
#with weighting
under19_subregion_monthly_vis_allyears <- under_19_pop |> left_join(total_subregions_pops_under19) |>
  mutate(pop_frac = POPSIZE / total_subregion_pop_under19) |>
  mutate(group_weight = pop_frac / NMEMB) |>
  mutate(weighted_vis_per_thous = NVISITS * group_weight*1000) |>
  group_by(MONTH, YEAR, part) |>
  summarize(vis_per_thous = sum(weighted_vis_per_thous))

under19_subregion_monthly_vis_avg <- under19_subregion_monthly_vis_allyears |>
  group_by(MONTH, part) |>
  summarize(avg_vis_per_thous = mean(vis_per_thous), sd = sd(vis_per_thous))

#make plot of the visits 
under_19_monthly_vis_plot <- under19_subregion_monthly_vis_avg |>
  mutate(part = factor(part, subcolors_df$Region)) |>
  ggplot(aes(MONTH, avg_vis_per_thous, group = part)) + 
  geom_line(aes(color = part)) + geom_ribbon(aes(MONTH, ymin = avg_vis_per_thous - 1.96*sd/3, 
                                ymax = avg_vis_per_thous + 1.96*sd/3, fill = part), alpha = 0.3) +
  geom_point(aes(MONTH, vis_per_thous, group= part, color = part), 
             data = under19_subregion_monthly_vis_allyears, size =0.2) +
  theme_minimal() + scale_color_manual(name = "Subregion", values = subcolors_df$region_color)  +
  scale_fill_manual(name = "Subregion", values = subcolors_df$region_color) + 
  ylab("Under 19 Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


#under 4
under4_subregion_monthly_vis_allyears <- under4_pop |> left_join(total_subregions_pops_under4) |>
  mutate(pop_frac = POPSIZE / total_subregion_pop_under4) |>
  mutate(group_weight = pop_frac / NMEMB) |>
  mutate(weighted_vis_per_thous = NVISITS * group_weight*1000) |>
  group_by(MONTH, YEAR, part) |>
  summarize(vis_per_thous = sum(weighted_vis_per_thous))

under4_subregion_monthly_vis_avg <- under4_subregion_monthly_vis_allyears |>
  group_by(MONTH, part) |>
  summarize(avg_vis_per_thous = mean(vis_per_thous), sd = sd(vis_per_thous))

#make plot of the visits 
under_4_monthly_vis_plot <- under4_subregion_monthly_vis_avg |>
  mutate(part = factor(part, subcolors_df$Region)) |>
  ggplot(aes(MONTH, avg_vis_per_thous, group = part)) + 
  geom_line(aes(color = part)) + geom_ribbon(aes(MONTH, ymin = avg_vis_per_thous - 1.96*sd/3, 
                                                 ymax = avg_vis_per_thous + 1.96*sd/3, fill = part), alpha = 0.3) +
  geom_point(aes(MONTH, vis_per_thous, group= part, color = part), 
             data = under4_subregion_monthly_vis_allyears, size =0.2) +
  theme_minimal() + scale_color_manual(name = "Subregion", values = subcolors_df$region_color)  +
  scale_fill_manual(name = "Subregion", values = subcolors_df$region_color) + 
  ylab("Under 4 Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#5-9
fivetonine_subregion_monthly_vis_allyears <- fivetonine_pop |> left_join(total_subregions_pops_fivetonine) |>
  mutate(pop_frac = POPSIZE / total_subregion_pop_fivetonine) |>
  mutate(group_weight = pop_frac / NMEMB) |>
  mutate(weighted_vis_per_thous = NVISITS * group_weight*1000) |>
  group_by(MONTH, YEAR, part) |>
  summarize(vis_per_thous = sum(weighted_vis_per_thous))

fivetonine_subregion_monthly_vis_avg <- fivetonine_subregion_monthly_vis_allyears |>
  group_by(MONTH, part) |>
  summarize(avg_vis_per_thous = mean(vis_per_thous), sd = sd(vis_per_thous))

#make plot of the visits 
fivetonine_monthly_vis_plot <- fivetonine_subregion_monthly_vis_avg |>
  mutate(part = factor(part, subcolors_df$Region)) |>
  ggplot(aes(MONTH, avg_vis_per_thous, group = part)) + 
  geom_line(aes(color = part)) + geom_ribbon(aes(MONTH, ymin = avg_vis_per_thous - 1.96*sd/3, 
                                                 ymax = avg_vis_per_thous + 1.96*sd/3, fill = part), alpha = 0.3) +
  geom_point(aes(MONTH, vis_per_thous, group= part, color = part), 
             data = fivetonine_subregion_monthly_vis_allyears, size =0.2) +
  theme_minimal() + scale_color_manual(name = "Subregion", values = subcolors_df$region_color)  +
  scale_fill_manual(name = "Subregion", values = subcolors_df$region_color) + 
  ylab("5-9 Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#10 to 19
tentonineteen_subregion_monthly_vis_allyears <- tentonineteen_pop |> left_join(total_subregions_pops_tentonineteen) |>
  mutate(pop_frac = POPSIZE / total_subregion_pop_tentonineteen) |>
  mutate(group_weight = pop_frac / NMEMB) |>
  mutate(weighted_vis_per_thous = NVISITS * group_weight*1000) |>
  group_by(MONTH, YEAR, part) |>
  summarize(vis_per_thous = sum(weighted_vis_per_thous))

tentonineteen_subregion_monthly_vis_avg <- tentonineteen_subregion_monthly_vis_allyears |>
  group_by(MONTH, part) |>
  summarize(avg_vis_per_thous = mean(vis_per_thous), sd = sd(vis_per_thous))

#make plot of the visits 
tentonineteen_monthly_vis_plot <- tentonineteen_subregion_monthly_vis_avg |>
  mutate(part = factor(part, subcolors_df$Region)) |>
  ggplot(aes(MONTH, avg_vis_per_thous, group = part)) + 
  geom_line(aes(color = part)) + geom_ribbon(aes(MONTH, ymin = avg_vis_per_thous - 1.96*sd/3, 
                                                 ymax = avg_vis_per_thous + 1.96*sd/3, fill = part), alpha = 0.3) +
  geom_point(aes(MONTH, vis_per_thous, group= part, color = part), 
             data = tentonineteen_subregion_monthly_vis_allyears, size =0.2) +
  theme_minimal() + scale_color_manual(name = "Subregion", values = subcolors_df$region_color)  +
  scale_fill_manual(name = "Subregion", values = subcolors_df$region_color) + 
  ylab("10-19 Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


#5-19, grouping 5-9 and 10-19 together
fivetonineteen_subregion_monthly_vis_allyears <- fivetonineteen_pop |> left_join(total_subregions_pops_fivetonineteen) |>
  mutate(pop_frac = POPSIZE / total_subregion_pop_fivetonineteen) |>
  mutate(group_weight = pop_frac / NMEMB) |>
  mutate(weighted_vis_per_thous = NVISITS * group_weight*1000) |>
  group_by(MONTH, YEAR, part) |>
  summarize(vis_per_thous = sum(weighted_vis_per_thous))

fivetonineteen_subregion_monthly_vis_avg <- fivetonineteen_subregion_monthly_vis_allyears |>
  group_by(MONTH, part) |>
  summarize(avg_vis_per_thous = mean(vis_per_thous), sd = sd(vis_per_thous))

#make plot of the visits 
fivetonineteen_monthly_vis_plot <- fivetonineteen_subregion_monthly_vis_avg |>
  mutate(part = factor(part, subcolors_df$Region)) |>
  ggplot(aes(MONTH, avg_vis_per_thous, group = part)) + 
  geom_line(aes(color = part)) + geom_ribbon(aes(MONTH, ymin = avg_vis_per_thous - 1.96*sd/3, 
                                                 ymax = avg_vis_per_thous + 1.96*sd/3, fill = part), alpha = 0.3) +
  geom_point(aes(MONTH, vis_per_thous, group= part, color = part), 
             data = fivetonineteen_subregion_monthly_vis_allyears, size =0.2) +
  theme_minimal() + scale_color_manual(name = "Subregion", values = subcolors_df$region_color)  +
  scale_fill_manual(name = "Subregion", values = subcolors_df$region_color) + 
  ylab("5-19 Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


#over19
over19_subregion_monthly_vis_allyears <- over_19_pop |> left_join(total_subregions_pops_over19) |>
  mutate(pop_frac = POPSIZE / total_subregion_pop_over19) |>
  mutate(group_weight = pop_frac / NMEMB) |>
  mutate(weighted_vis_per_thous = NVISITS * group_weight*1000) |>
  group_by(MONTH, YEAR, part) |>
  summarize(vis_per_thous = sum(weighted_vis_per_thous))

over19_subregion_monthly_vis_avg <- over19_subregion_monthly_vis_allyears |>
  group_by(MONTH, part) |>
  summarize(avg_vis_per_thous = mean(vis_per_thous), sd = sd(vis_per_thous))

#make plot of the visits 
over_19_monthly_vis_plot <- over19_subregion_monthly_vis_avg |>
  mutate(part = factor(part, subcolors_df$Region)) |>
  ggplot(aes(MONTH, avg_vis_per_thous, group = part)) + 
  geom_line(aes(color = part)) + geom_ribbon(aes(MONTH, ymin = avg_vis_per_thous - 1.96*sd/3, 
                                                 ymax = avg_vis_per_thous + 1.96*sd/3, fill = part), alpha = 0.3) +
  geom_point(aes(MONTH, vis_per_thous, group= part, color = part), 
             data = over19_subregion_monthly_vis_allyears, size =0.2) +
  theme_minimal() + scale_color_manual(name = "Subregion", values = subcolors_df$region_color)  +
  scale_fill_manual(name = "Subregion", values = subcolors_df$region_color) + 
  ylab("Over 19 Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  

#really is in chidlren that this differences play out

#calculate uptick dates in each region in each agegroup over 19
month_mins_under19 <- expand_grid(YEAR = c(2010:2018), part = c("East South Central", "West South Central",
                                                        "West North Central", "South Atlantic", "Middle Atlantic",
                                                        "Mountain West", "East North Central", "New England", "Pacific West"), min = NA)

#fill dataframe
for(i in 1:nrow(month_mins_under19)){
  df <- under19_subregion_monthly_vis_allyears |>
    filter(YEAR == month_mins_under19[i,1], part == as.character(month_mins_under19[i,2]))
  month_mins_under19[i,3] <- df[which(df$vis_per_thous == min(df$vis_per_thous)), 1]
}

avg_month_mins_under19 <- month_mins_under19 |> group_by(part) |> summarize(mean_min = mean(min), sd = sd(min)) |> mutate(lower = mean_min - 1.96*sd/3, upper = mean_min + 1.96*sd/3) |>
  mutate(Subregion = factor(part, levels = c("West South Central", "East South Central", "Mountain West",
                                             "West North Central", "South Atlantic", "East North Central", 
                                             "Pacific West", "Middle Atlantic", "New England"))) |>
  arrange(Subregion)

avg_subregionvis_under19 <- under19_subregion_monthly_vis_allyears |>
  group_by(part, MONTH) |> 
  summarize(avg_vis_perthous = mean(vis_per_thous), sd_vis = sd(vis_per_thous)) |>
  left_join(avg_month_mins_under19) |> 
  mutate(lower = avg_vis_perthous - 1.96*sd_vis / 3, upper = avg_vis_perthous + 1.96*sd_vis / 3) |> 
  mutate(Region = Subregion) |>
  left_join(subcolors_df) |> arrange(Subregion)

subregion_uptick_plot_under19 <- avg_subregionvis_under19 |> 
  mutate(Subregion = factor(Subregion, subcolors_df$Region)) |>
  ggplot(aes(x = MONTH, y = avg_vis_perthous)) + geom_line(aes(color = Subregion)) +
  geom_ribbon(aes(x = MONTH, ymin = lower, ymax = upper, fill = Subregion), alpha = 0.2) + 
  theme_minimal() + scale_color_manual(values = subcolors_df$region_color) + geom_vline(aes(xintercept = mean_min), color = 'black', linetype = 2, alpha = 0.5) +
  scale_fill_manual(values = subcolors_df$region_color) + 
  facet_wrap(~Subregion) +
  ylab("Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



month_mins_date_under19 <- avg_month_mins_under19 |> mutate(Date = date(duration(month = mean_min-1))-1, lower_date = date(duration(month = lower-1))-1,
                                            upper_date =date(duration(month = upper-1))-1, cat = "Min Visit Date") |> 
  select(Subregion, Date, lower_date, upper_date, cat)

year(month_mins_date_under19$Date) <- 2019
year(month_mins_date_under19$lower_date) <- 2019
year(month_mins_date_under19$upper_date) <- 2019

min_uptick_order_under19 <- month_mins_date_under19 |> arrange(Date) |> pull(Subregion)

#do this with under 4
month_mins_under4 <- expand_grid(YEAR = c(2010:2018), part = c("East South Central", "West South Central",
                                                                "West North Central", "South Atlantic", "Middle Atlantic",
                                                                "Mountain West", "East North Central", "New England", "Pacific West"), min = NA)

#fill dataframe
for(i in 1:nrow(month_mins_under4)){
  df <- under4_subregion_monthly_vis_allyears |>
    filter(YEAR == month_mins_under4[i,1], part == as.character(month_mins_under4[i,2]))
  month_mins_under4[i,3] <- df[which(df$vis_per_thous == min(df$vis_per_thous)), 1]
}

avg_month_mins_under4 <- month_mins_under4 |> group_by(part) |> summarize(mean_min = mean(min), sd = sd(min)) |> mutate(lower = mean_min - 1.96*sd/3, upper = mean_min + 1.96*sd/3) |>
  mutate(Subregion = factor(part, levels = c("West South Central", "East South Central", "Mountain West",
                                             "West North Central", "South Atlantic", "East North Central", 
                                             "Pacific West", "Middle Atlantic", "New England"))) |>
  arrange(Subregion)

avg_subregionvis_under4 <- under4_subregion_monthly_vis_allyears |>
  group_by(part, MONTH) |> 
  summarize(avg_vis_perthous = mean(vis_per_thous), sd_vis = sd(vis_per_thous)) |>
  left_join(avg_month_mins_under4) |> 
  mutate(lower = avg_vis_perthous - 1.96*sd_vis / 3, upper = avg_vis_perthous + 1.96*sd_vis / 3) |> 
  mutate(Region = Subregion) |>
  left_join(subcolors_df) |> arrange(Subregion)

subregion_uptick_plot_under4 <- avg_subregionvis_under4 |> 
  mutate(Subregion = factor(Subregion, subcolors_df$Region)) |>
  ggplot(aes(x = MONTH, y = avg_vis_perthous)) + geom_line(aes(color = Subregion)) +
  geom_ribbon(aes(x = MONTH, ymin = lower, ymax = upper, fill = Subregion), alpha = 0.2) + 
  theme_minimal() + scale_color_manual(values = subcolors_df$region_color) + geom_vline(aes(xintercept = mean_min), color = 'black', linetype = 2, alpha = 0.5) +
  scale_fill_manual(values = subcolors_df$region_color) + 
  facet_wrap(~Subregion) +
  ylab("Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


month_mins_date_under4 <- avg_month_mins_under4 |> mutate(Date = date(duration(month = mean_min-1))-1, lower_date = date(duration(month = lower-1))-1,
                                                            upper_date =date(duration(month = upper-1))-1, cat = "Min Visit Date") |> 
  select(Subregion, Date, lower_date, upper_date, cat) |> mutate(age = "under4")

year(month_mins_date_under4$Date) <- 2019
year(month_mins_date_under4$lower_date) <- 2019
year(month_mins_date_under4$upper_date) <- 2019

min_uptick_order_under4 <- month_mins_date_under4 |> arrange(Date) |> pull(Subregion)

#five to nine
month_mins_fivetonine<- expand_grid(YEAR = c(2010:2018), part = c("East South Central", "West South Central",
                                                               "West North Central", "South Atlantic", "Middle Atlantic",
                                                               "Mountain West", "East North Central", "New England", "Pacific West"), min = NA)

#fill dataframe
for(i in 1:nrow(month_mins_fivetonine)){
  df <- fivetonine_subregion_monthly_vis_allyears |>
    filter(YEAR == month_mins_fivetonine[i,1], part == as.character(month_mins_fivetonine[i,2]))
  month_mins_fivetonine[i,3] <- df[which(df$vis_per_thous == min(df$vis_per_thous)), 1]
}

avg_month_mins_fivetonine <- month_mins_fivetonine |> group_by(part) |> summarize(mean_min = mean(min), sd = sd(min)) |> mutate(lower = mean_min - 1.96*sd/3, upper = mean_min + 1.96*sd/3) |>
  mutate(Subregion = factor(part, levels = c("West South Central", "East South Central", "Mountain West",
                                             "West North Central", "South Atlantic", "East North Central", 
                                             "Pacific West", "Middle Atlantic", "New England"))) |>
  arrange(Subregion)

avg_subregionvis_fivetonine <- fivetonine_subregion_monthly_vis_allyears |>
  group_by(part, MONTH) |> 
  summarize(avg_vis_perthous = mean(vis_per_thous), sd_vis = sd(vis_per_thous)) |>
  left_join(avg_month_mins_fivetonine) |> 
  mutate(lower = avg_vis_perthous - 1.96*sd_vis / 3, upper = avg_vis_perthous + 1.96*sd_vis / 3) |> 
  mutate(Region = Subregion) |>
  left_join(subcolors_df) |> arrange(Subregion)

subregion_uptick_plot_fivetonine <- avg_subregionvis_fivetonine |> 
  mutate(Subregion = factor(Subregion, subcolors_df$Region)) |>
  ggplot(aes(x = MONTH, y = avg_vis_perthous)) + geom_line(aes(color = Subregion)) +
  geom_ribbon(aes(x = MONTH, ymin = lower, ymax = upper, fill = Subregion), alpha = 0.2) + 
  theme_minimal() + scale_color_manual(values = subcolors_df$region_color) + geom_vline(aes(xintercept = mean_min), color = 'black', linetype = 2, alpha = 0.5) +
  scale_fill_manual(values = subcolors_df$region_color) + 
  facet_wrap(~Subregion) +
  ylab("Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


month_mins_date_fivetonine <- avg_month_mins_fivetonine |> mutate(Date = date(duration(month = mean_min-1))-1, lower_date = date(duration(month = lower-1))-1,
                                                          upper_date =date(duration(month = upper-1))-1, cat = "Min Visit Date") |> 
  select(Subregion, Date, lower_date, upper_date, cat) |> mutate(age = "5-9")

  year(month_mins_date_fivetonine$Date) <- 2019
year(month_mins_date_fivetonine$lower_date) <- 2019
year(month_mins_date_fivetonine$upper_date) <- 2019

min_uptick_order_fivetonine <- month_mins_date_fivetonine |> arrange(Date) |> pull(Subregion)


#10-19
month_mins_tentonineteen<- expand_grid(YEAR = c(2010:2018), part = c("East South Central", "West South Central",
                                                                  "West North Central", "South Atlantic", "Middle Atlantic",
                                                                  "Mountain West", "East North Central", "New England", "Pacific West"), min = NA)

#fill dataframe
for(i in 1:nrow(month_mins_tentonineteen)){
  df <- tentonineteen_subregion_monthly_vis_allyears |>
    filter(YEAR == month_mins_tentonineteen[i,1], part == as.character(month_mins_tentonineteen[i,2]))
  month_mins_tentonineteen[i,3] <- df[which(df$vis_per_thous == min(df$vis_per_thous)), 1]
}

avg_month_mins_tentonineteen <- month_mins_tentonineteen |> group_by(part) |> summarize(mean_min = mean(min), sd = sd(min)) |> mutate(lower = mean_min - 1.96*sd/3, upper = mean_min + 1.96*sd/3) |>
  mutate(Subregion = factor(part, levels = c("West South Central", "East South Central", "Mountain West",
                                             "West North Central", "South Atlantic", "East North Central", 
                                             "Pacific West", "Middle Atlantic", "New England"))) |>
  arrange(Subregion)

avg_subregionvis_tentonineteen <- tentonineteen_subregion_monthly_vis_allyears |>
  group_by(part, MONTH) |> 
  summarize(avg_vis_perthous = mean(vis_per_thous), sd_vis = sd(vis_per_thous)) |>
  left_join(avg_month_mins_tentonineteen) |> 
  mutate(lower = avg_vis_perthous - 1.96*sd_vis / 3, upper = avg_vis_perthous + 1.96*sd_vis / 3) |> 
  mutate(Region = Subregion) |>
  left_join(subcolors_df) |> arrange(Subregion)

subregion_uptick_plot_tentonineteen <- avg_subregionvis_tentonineteen |> 
  mutate(Subregion = factor(Subregion, subcolors_df$Region)) |>
  ggplot(aes(x = MONTH, y = avg_vis_perthous)) + geom_line(aes(color = Subregion)) +
  geom_ribbon(aes(x = MONTH, ymin = lower, ymax = upper, fill = Subregion), alpha = 0.2) + 
  theme_minimal() + scale_color_manual(values = subcolors_df$region_color) + geom_vline(aes(xintercept = mean_min), color = 'black', linetype = 2, alpha = 0.5) +
  scale_fill_manual(values = subcolors_df$region_color) + 
  facet_wrap(~Subregion) +
  ylab("Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


month_mins_date_tentonineteen <- avg_month_mins_tentonineteen |> mutate(Date = date(duration(month = mean_min-1))-1, lower_date = date(duration(month = lower-1))-1,
                                                                  upper_date =date(duration(month = upper-1))-1, cat = "Min Visit Date") |> 
  select(Subregion, Date, lower_date, upper_date, cat) |> mutate(age = "10-19")

year(month_mins_date_tentonineteen$Date) <- 2019
year(month_mins_date_tentonineteen$lower_date) <- 2019
year(month_mins_date_tentonineteen$upper_date) <- 2019

min_uptick_order_tentonineteen <- month_mins_date_tentonineteen |> arrange(Date) |> pull(Subregion)

#5-19
month_mins_fivetonineteen<- expand_grid(YEAR = c(2010:2018), part = c("East South Central", "West South Central",
                                                                     "West North Central", "South Atlantic", "Middle Atlantic",
                                                                     "Mountain West", "East North Central", "New England", "Pacific West"), min = NA)

#fill dataframe
for(i in 1:nrow(month_mins_fivetonineteen)){
  df <- fivetonineteen_subregion_monthly_vis_allyears |>
    filter(YEAR == month_mins_fivetonineteen[i,1], part == as.character(month_mins_fivetonineteen[i,2]))
  month_mins_fivetonineteen[i,3] <- df[which(df$vis_per_thous == min(df$vis_per_thous)), 1]
}

avg_month_mins_fivetonineteen <- month_mins_fivetonineteen |> group_by(part) |> summarize(mean_min = mean(min), sd = sd(min)) |> mutate(lower = mean_min - 1.96*sd/3, upper = mean_min + 1.96*sd/3) |>
  mutate(Subregion = factor(part, levels = c("West South Central", "East South Central", "Mountain West",
                                             "West North Central", "South Atlantic", "East North Central", 
                                             "Pacific West", "Middle Atlantic", "New England"))) |>
  arrange(Subregion)

avg_subregionvis_fivetonineteen <- fivetonineteen_subregion_monthly_vis_allyears |>
  group_by(part, MONTH) |> 
  summarize(avg_vis_perthous = mean(vis_per_thous), sd_vis = sd(vis_per_thous)) |>
  left_join(avg_month_mins_fivetonineteen) |> 
  mutate(lower = avg_vis_perthous - 1.96*sd_vis / 3, upper = avg_vis_perthous + 1.96*sd_vis / 3) |> 
  mutate(Region = Subregion) |>
  left_join(subcolors_df) |> arrange(Subregion)

subregion_uptick_plot_fivetonineteen <- avg_subregionvis_fivetonineteen |> 
  mutate(Subregion = factor(Subregion, subcolors_df$Region)) |>
  ggplot(aes(x = MONTH, y = avg_vis_perthous)) + geom_line(aes(color = Subregion)) +
  geom_ribbon(aes(x = MONTH, ymin = lower, ymax = upper, fill = Subregion), alpha = 0.2) + 
  theme_minimal() + scale_color_manual(values = subcolors_df$region_color) + geom_vline(aes(xintercept = mean_min), color = 'black', linetype = 2, alpha = 0.5) +
  scale_fill_manual(values = subcolors_df$region_color) + 
  facet_wrap(~Subregion) +
  ylab("Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


month_mins_date_fivetonineteen <- avg_month_mins_fivetonineteen |> mutate(Date = date(duration(month = mean_min-1))-1, lower_date = date(duration(month = lower-1))-1,
                                                                        upper_date =date(duration(month = upper-1))-1, cat = "Min Visit Date") |> 
  select(Subregion, Date, lower_date, upper_date, cat) |> mutate(age = "5-19")

year(month_mins_date_fivetonineteen$Date) <- 2019
year(month_mins_date_fivetonineteen$lower_date) <- 2019
year(month_mins_date_fivetonineteen$upper_date) <- 2019

min_uptick_order_fivetonineteen <- month_mins_date_fivetonineteen |> arrange(Date) |> pull(Subregion)


#do the same steps with over 19
month_mins_over19 <- expand_grid(YEAR = c(2010:2018), part = c("East South Central", "West South Central",
                                                                "West North Central", "South Atlantic", "Middle Atlantic",
                                                                "Mountain West", "East North Central", "New England", "Pacific West"), min = NA)

#fill dataframe
for(i in 1:nrow(month_mins_over19)){
  df <- over19_subregion_monthly_vis_allyears |>
    filter(YEAR == month_mins_over19[i,1], part == as.character(month_mins_over19[i,2]))
  month_mins_over19[i,3] <- df[which(df$vis_per_thous == min(df$vis_per_thous)), 1]
}

avg_month_mins_over19 <- month_mins_over19 |> group_by(part) |> summarize(mean_min = mean(min), sd = sd(min)) |> mutate(lower = mean_min - 1.96*sd/3, upper = mean_min + 1.96*sd/3) |>
  mutate(Subregion = factor(part, levels = c("West South Central", "East South Central", "Mountain West",
                                             "West North Central", "South Atlantic", "East North Central", 
                                             "Pacific West", "Middle Atlantic", "New England"))) |>
  arrange(Subregion)

avg_subregionvis_over19 <- over19_subregion_monthly_vis_allyears |>
  group_by(part, MONTH) |> 
  summarize(avg_vis_perthous = mean(vis_per_thous), sd_vis = sd(vis_per_thous)) |>
  left_join(avg_month_mins_over19) |> 
  mutate(lower = avg_vis_perthous - 1.96*sd_vis / 3, upper = avg_vis_perthous + 1.96*sd_vis / 3) |> 
  mutate(Region = Subregion) |>
  left_join(subcolors_df) |> arrange(Subregion)

subregion_uptick_plot_over19 <- avg_subregionvis_over19 |> 
  mutate(Subregion = factor(Subregion, subcolors_df$Region)) |>
  ggplot(aes(x = MONTH, y = avg_vis_perthous)) + geom_line(aes(color = Subregion)) +
  geom_ribbon(aes(x = MONTH, ymin = lower, ymax = upper, fill = Subregion), alpha = 0.2) + 
  theme_minimal() + scale_color_manual(values = subcolors_df$region_color) + geom_vline(aes(xintercept = mean_min), color = 'black', linetype = 2, alpha = 0.5) +
  scale_fill_manual(values = subcolors_df$region_color) + 
  facet_wrap(~Subregion) +
  ylab("Average Visits per 1000") +
  xlab("Month") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



month_mins_date_over19 <- avg_month_mins_over19 |> mutate(Date = date(duration(month = mean_min-1))-1, lower_date = date(duration(month = lower-1))-1,
                                                            upper_date =date(duration(month = upper-1))-1, cat = "Min Visit Date") |> 
  select(Subregion, Date, lower_date, upper_date, cat) |> mutate(age = "over19")

year(month_mins_date_over19$Date) <- 2019
year(month_mins_date_over19$lower_date) <- 2019
year(month_mins_date_over19$upper_date) <- 2019

min_uptick_order_over19 <- month_mins_date_over19 |> arrange(Date) |> pull(Subregion)









