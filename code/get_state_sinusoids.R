#fitting state data to sinusoids

#define a function for sinusoid fitting
use_nls_func_params = function(df) {
  nls(weighted_cases_per_thousand ~ A * cos(omega * (MONTH - phase)) + offset,
      start = list(A = amp_estimate, phase = phase_estimate, offset = offset_estimate),
      data = df)
}

#join visit, membership, and population data
coh_dat_pop <- left_join(dat,coh) |> left_join(pop_sizes)

#calculate population sizes of each state
total_state_pops <- pop_sizes |>
  group_by(STATE) |>
  summarize(total_state_pop = sum(POPSIZE))

state_weighted_vis_allyears <- coh_dat_pop |>
  select(MONTH, STATE, SEX, AGEGRP, NVISITS, YEAR, NMEMB, POPSIZE) |>
  left_join(total_state_pops) |>
  mutate(pop_frac = POPSIZE / total_state_pop) |>
  mutate(group_weight = pop_frac/NMEMB) |>
  group_by(STATE, MONTH, YEAR) |>
  summarize(weighted_cases = sum(NVISITS*group_weight)) |>
  mutate(weighted_cases_per_thousand = weighted_cases*1000) |>
  mutate(region = tolower(STATE))|>
  left_join(regions) |>
  select(STATE, MONTH, YEAR, weighted_cases_per_thousand, part) 

state_weighted_vis_avg <- state_weighted_vis_allyears |> 
  group_by(STATE, MONTH, part) |>
  summarize(mean_weighted_cases_per_thousand = mean(weighted_cases_per_thousand), sd = sd(weighted_cases_per_thousand))

#now fit sinusoids to the data from all years
month_maxes <- state_weighted_vis_allyears |> 
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  group_by(STATE, YEAR) |>
  summarize(max = max(weighted_cases_per_thousand), max_month = MONTH[which(weighted_cases_per_thousand== max)], 
            min = min(weighted_cases_per_thousand), diff = max-min, mean = mean(weighted_cases_per_thousand))
phase_estimate <- mean(month_maxes$max_month)
amp_estimate <- mean(month_maxes$diff)/2
offset_estimate <- mean(month_maxes$mean)
omega = 2 * pi / 12

#initialize a dataframe to store state sinusoid predictions and parameters
state_sinusoids <- data.frame()
state_sinusoids_parameters <- data.frame("State" = unique(state_weighted_vis_allyears$STATE),
                                         "Amplitude" = NA,
                                         "Phase" = NA,
                                         "Offset" = NA,
                                         "RSS" = NA,
                                         "Amplitude_SE" = NA,
                                         "Phase_SE" = NA,
                                         "Offset_SE" = NA)
mods <- list()

#fit sinusoids for each state, store parameters and predictions
for(i in 1:length(state_sinusoids_parameters$State)){
  state <- state_sinusoids_parameters$State[i] #assign state
  tmp <- state_weighted_vis_allyears |> 
    filter(STATE == state) #this is a dataframe with just the state
  mod <- tmp |> 
    use_nls_func_params() #this creates the sinusoidal model
  mods <- append(mods, mod)
  state_sinusoids_parameters[i,2] <- coef(mod)[[1]]
  state_sinusoids_parameters[i,3] <- coef(mod)[[2]]
  state_sinusoids_parameters[i,4] <- coef(mod)[[3]]
  state_sinusoids_parameters[i,5] <- sum(residuals(mod)^2) #this probably needs to be fixed now that there are multiple years
  state_sinusoids_parameters[i,6] <- summary(mod)$coefficients[1,2]
  state_sinusoids_parameters[i,7] <- summary(mod)$coefficients[2,2]
  state_sinusoids_parameters[i,8] <- summary(mod)$coefficients[3,2]
  tmp2 <- state_weighted_vis_avg |>
    filter(STATE == state) |>
    mutate(prediction = coef(mod)[[1]]* cos(omega*(MONTH - coef(mod)[[2]])) + coef(mod)[[3]] ) 
  state_sinusoids <- rbind(state_sinusoids, tmp2)
}

# write_csv(state_sinusoids, "state_sinusoids.csv")
# write_csv(state_sinusoids_parameters, "state_sinusoids_parameters.csv")

#add confidence intervals to state sinusoids
range_col <- function(vec){
  quantile(vec, c(0.025, 0.975))
}

state_ci_df <- data.frame()

for(i in 1:length(state_sinusoids_parameters$State)){
  state_amps <- rnorm(10000, mean = state_sinusoids_parameters$Amplitude[i], sd = state_sinusoids_parameters$Amplitude_SE[i]) #SE vs SD?
  state_phases <- rnorm(10000, mean = state_sinusoids_parameters$Phase[i], sd = state_sinusoids_parameters$Phase_SE[i])
  state_offsets <- rnorm(10000, mean = state_sinusoids_parameters$Offset[i], sd = state_sinusoids_parameters$Offset_SE[i])
  state_boot <- data.frame("Amp" = state_amps, "Phase" = state_phases, "Offset" = state_offsets)
  state_boot <- state_boot |>
    mutate("Jan" = Amp* cos(omega * (1-Phase))+ Offset,
           "Feb" = Amp* cos(omega * (2-Phase))+ Offset,
           "Mar" = Amp* cos(omega * (3-Phase))+ Offset,
           "Apr" = Amp* cos(omega * (4-Phase))+ Offset,
           "May" = Amp* cos(omega * (5-Phase))+ Offset,
           "Jun" = Amp* cos(omega * (6-Phase))+ Offset,
           "Jul" = Amp* cos(omega * (7-Phase))+ Offset,
           "Aug" = Amp* cos(omega * (8-Phase))+ Offset,
           "Sep" = Amp* cos(omega * (9-Phase))+ Offset,
           "Oct" = Amp* cos(omega * (10-Phase))+ Offset,
           "Nov" = Amp* cos(omega * (11-Phase))+ Offset,
           "Dec" = Amp* cos(omega * (12-Phase))+ Offset)
  lower_upper <- t(apply(state_boot[4:15], 2, range_col))
  names <- rownames(lower_upper)
  lower_upper <- as.tibble(lower_upper)
  rownames(lower_upper) = names
  colnames(lower_upper) = c("lower", "upper")
  lower_upper_state <- lower_upper |>
    mutate(MONTH = c(1:12),
           STATE = state_sinusoids_parameters$State[i])
  state_ci_df <- rbind(state_ci_df, lower_upper_state)
  
}
state_sinusoids_withCIs <- left_join(state_sinusoids, state_ci_df)
state_sinusoids_withCIs_cont <- state_sinusoids_withCIs |> filter(!STATE %in% c("Hawaii", "Alaska", "South Carolina"))
state_sinusoids_withCIs_cont_sub <- subregions |> 
  mutate(Region = part) |> 
  select(-part) |> 
  left_join(subcolors_df) |> 
  right_join(state_sinusoids_withCIs_cont) |>
  mutate(Region = factor(Region, levels = c("East South Central", "West South Central",
                                            "West North Central", "South Atlantic", "Middle Atlantic",
                                            "Mountain West", "East North Central", "New England", "Pacific West")))
#--------------------------------------------------
#repeat process but on log(y) for state weighted visits
state_weighted_vis_avg_log <- state_weighted_vis_avg |> mutate(log_mean_weighted_cases_per_thousand = log(mean_weighted_cases_per_thousand)) |>
  select(-mean_weighted_cases_per_thousand)
#other initial parameter estimates should be fine

state_weighted_vis_allyears_log <- state_weighted_vis_allyears |> mutate(log_weighted_cases_per_thousand = log(weighted_cases_per_thousand)) |>
  select(-weighted_cases_per_thousand)

#make a new version 
use_nls_func_params_log = function(df) {
  nls(log(weighted_cases_per_thousand + 0.00001) ~ A * cos(omega * (MONTH - phase)) + offset, #add small constant to deal with 0s
      start = list(A = amp_estimate, phase = phase_estimate, offset = offset_estimate),
      data = df)
}
state_sinusoids_log <- data.frame()
state_sinusoids_parameters_log <- data.frame("State" = unique(state_weighted_vis_allyears$STATE),
                                         "Amplitude" = NA,
                                         "Phase" = NA,
                                         "Offset" = NA,
                                         "RSS" = NA,
                                         "Amplitude_SE" = NA,
                                         "Phase_SE" = NA,
                                         "Offset_SE" = NA)
#fit sinusoids for each state, store parameters and predictions
for(i in 1:length(state_sinusoids_parameters_log$State)){
  state <- state_sinusoids_parameters_log$State[i] #assign state
  tmp <- state_weighted_vis_allyears |> 
    filter(STATE == state) #this is a dataframe with just the state
  mod <- tmp |> 
    use_nls_func_params_log() #this creates the sinusoidal model
  state_sinusoids_parameters_log[i,2] <- coef(mod)[[1]]
  state_sinusoids_parameters_log[i,3] <- coef(mod)[[2]]
  state_sinusoids_parameters_log[i,4] <- coef(mod)[[3]]
  state_sinusoids_parameters_log[i,5] <- sum(residuals(mod)^2) 
  state_sinusoids_parameters_log[i,6] <- summary(mod)$coefficients[1,2]
  state_sinusoids_parameters_log[i,7] <- summary(mod)$coefficients[2,2]
  state_sinusoids_parameters_log[i,8] <- summary(mod)$coefficients[3,2]
  tmp2 <- state_weighted_vis_avg |>
    filter(STATE == state) |>
    mutate(prediction = coef(mod)[[1]]* cos(omega*(MONTH - coef(mod)[[2]])) + coef(mod)[[3]] ) 
  state_sinusoids_log <- rbind(state_sinusoids_log, tmp2)
}

state_sinusoids_log <- state_sinusoids_log |> mutate(log_mean_weighted_cases_per_thousand = log(mean_weighted_cases_per_thousand)) |> 
  select(STATE, MONTH, part, log_mean_weighted_cases_per_thousand, sd, prediction) #not sure sd is still needed

#add confidence intervals
state_ci_df_log <- data.frame()
for(i in 1:length(state_sinusoids_parameters_log$State)){
  state_amps <- rnorm(10000, mean = state_sinusoids_parameters_log$Amplitude[i], sd = state_sinusoids_parameters_log$Amplitude_SE[i]) #SE vs SD?
  state_phases <- rnorm(10000, mean = state_sinusoids_parameters_log$Phase[i], sd = state_sinusoids_parameters_log$Phase_SE[i])
  state_offsets <- rnorm(10000, mean = state_sinusoids_parameters_log$Offset[i], sd = state_sinusoids_parameters_log$Offset_SE[i])
  state_boot <- data.frame("Amp" = state_amps, "Phase" = state_phases, "Offset" = state_offsets)
  state_boot <- state_boot |>
    mutate("Jan" = Amp* cos(omega * (1-Phase))+ Offset,
           "Feb" = Amp* cos(omega * (2-Phase))+ Offset,
           "Mar" = Amp* cos(omega * (3-Phase))+ Offset,
           "Apr" = Amp* cos(omega * (4-Phase))+ Offset,
           "May" = Amp* cos(omega * (5-Phase))+ Offset,
           "Jun" = Amp* cos(omega * (6-Phase))+ Offset,
           "Jul" = Amp* cos(omega * (7-Phase))+ Offset,
           "Aug" = Amp* cos(omega * (8-Phase))+ Offset,
           "Sep" = Amp* cos(omega * (9-Phase))+ Offset,
           "Oct" = Amp* cos(omega * (10-Phase))+ Offset,
           "Nov" = Amp* cos(omega * (11-Phase))+ Offset,
           "Dec" = Amp* cos(omega * (12-Phase))+ Offset)
  lower_upper <- t(apply(state_boot[4:15], 2, range_col))
  names <- rownames(lower_upper)
  lower_upper <- as.tibble(lower_upper)
  rownames(lower_upper) = names
  colnames(lower_upper) = c("lower", "upper")
  lower_upper_state <- lower_upper |>
    mutate(MONTH = c(1:12),
           STATE = state_sinusoids_parameters_log$State[i])
  state_ci_df_log <- rbind(state_ci_df_log, lower_upper_state)
  
}
state_sinusoids_withCIs_log <- left_join(state_sinusoids_log, state_ci_df_log)
state_sinusoids_withCIs_cont_log <- state_sinusoids_withCIs_log |> filter(!STATE %in% c("Hawaii", "Alaska", "South Carolina"))
state_sinusoids_withCIs_cont_sub_log <- subregions |> 
  mutate(Region = part) |> 
  select(-part) |> 
  left_join(subcolors_df) |> 
  right_join(state_sinusoids_withCIs_cont_log) |>
  mutate(Region = factor(Region, levels = c("East South Central", "West South Central",
                                            "West North Central", "South Atlantic", "Middle Atlantic",
                                            "Mountain West", "East North Central", "New England", "Pacific West")))

#save table of state phases from log model:
phase_table_log <- state_sinusoids_parameters_log |> mutate(Phase_corrected = Phase %% 12) |>
  mutate(Phase_lower = Phase_corrected - 1.96*Phase_SE,
         Phase_upper = Phase_corrected + 1.96*Phase_SE) |>
  mutate(Final_phase = paste0(round(Phase_corrected,2), " ", "(", round(Phase_lower,2), "-", round(Phase_upper,2), ")")) |>
  filter(!State %in% c("South Carolina", "Hawaii", "Alaska")) |>
  select(State, Final_phase) |>
  mutate("Phase(95% CI)" = Final_phase) |>
  select(-Final_phase)


#make qq plots for the log and non-logged versions to compare
qqplot_sinusoids <- left_join(state_weighted_vis_allyears, state_sinusoids) |> filter(!STATE %in% c("Alaska", "Hawaii", "South Carolina")) |>
  mutate(resid = weighted_cases_per_thousand - prediction) |>
  ggplot(aes(sample = resid)) + geom_qq(size = 0.3) + geom_qq_line(color = "red", linewidth = 0.3) + facet_wrap(~STATE) + theme_bw()
qqplot_sinusoids_log <- left_join(state_weighted_vis_allyears_log, state_sinusoids_log) |> filter(!STATE %in% c("Alaska", "Hawaii", "South Carolina")) |>
  mutate(resid = log_weighted_cases_per_thousand - prediction) |>
  ggplot(aes(sample = resid)) + geom_qq(size = 0.3) + geom_qq_line(color = "red", linewidth = 0.3) + facet_wrap(~STATE) + theme_bw()

#--------------------------------------------------
#repeat process but restricting to only those ages <19 and younger (school age) and those >19 
under_19_pop <- coh_dat_pop |> filter(AGEGRP %in% c("00_04", "05_09", "10_19"))
over_19_pop <- coh_dat_pop |> filter(!AGEGRP %in% c("00_04", "05_09", "10_19"))

#unweighted visits just for checking purposes
state_unweighted_vis_allyears_under19 <- under_19_pop |> group_by(STATE, MONTH, YEAR) |> summarize(nvis = sum(NVISITS), nmemb = sum(NMEMB)) |>
  mutate(vis_per_thous = nvis/nmemb*1000) 

state_unweighted_vis_avg_under19 <- state_unweighted_vis_allyears_under19 |> 
  group_by(STATE, MONTH) |>
  summarize(mean_vis_per_thous = mean(vis_per_thous), sd = sd(vis_per_thous))

#now do the normalization to total state populations in that age group
total_state_pops_under19 <- pop_sizes |> filter(AGEGRP %in% c("00_04", "05_09", "10_19")) |>
  group_by(STATE) |> summarize(total_state_pop_under19 = sum(POPSIZE))

total_state_pops_over19 <- pop_sizes |> filter(!AGEGRP %in% c("00_04", "05_09", "10_19")) |>
  group_by(STATE) |> summarize(total_state_pop_over19 = sum(POPSIZE))

state_weighted_vis_allyears_under19 <- under_19_pop |>
  select(MONTH, STATE, SEX, AGEGRP, NVISITS, YEAR, NMEMB, POPSIZE) |>
  left_join(total_state_pops_under19) |>
  mutate(pop_frac = POPSIZE / total_state_pop_under19) |>
  mutate(group_weight = pop_frac/NMEMB) |>
  group_by(STATE, MONTH, YEAR) |>
  summarize(weighted_cases = sum(NVISITS*group_weight)) |>
  mutate(weighted_cases_per_thousand = weighted_cases*1000) |>
  mutate(region = tolower(STATE))|>
  left_join(regions) |>
  select(STATE, MONTH, YEAR, weighted_cases_per_thousand, part) 

state_weighted_vis_avg_under19 <- state_weighted_vis_allyears_under19 |> 
  group_by(STATE, MONTH, part) |>
  summarize(mean_weighted_cases_per_thousand = mean(weighted_cases_per_thousand), sd = sd(weighted_cases_per_thousand))

state_weighted_vis_allyears_over19 <- over_19_pop |>
  select(MONTH, STATE, SEX, AGEGRP, NVISITS, YEAR, NMEMB, POPSIZE) |>
  left_join(total_state_pops_over19) |>
  mutate(pop_frac = POPSIZE / total_state_pop_over19) |>
  mutate(group_weight = pop_frac/NMEMB) |>
  group_by(STATE, MONTH, YEAR) |>
  summarize(weighted_cases = sum(NVISITS*group_weight)) |>
  mutate(weighted_cases_per_thousand = weighted_cases*1000) |>
  mutate(region = tolower(STATE))|>
  left_join(regions) |>
  select(STATE, MONTH, YEAR, weighted_cases_per_thousand, part) 

state_weighted_vis_avg_over19 <- state_weighted_vis_allyears_over19 |> 
  group_by(STATE, MONTH, part) |>
  summarize(mean_weighted_cases_per_thousand = mean(weighted_cases_per_thousand), sd = sd(weighted_cases_per_thousand))


#now fit sinusoids to the data from all years
month_maxes_under19 <- state_weighted_vis_allyears_under19 |> 
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  group_by(STATE, YEAR) |>
  summarize(max = max(weighted_cases_per_thousand), max_month = MONTH[which(weighted_cases_per_thousand== max)], 
            min = min(weighted_cases_per_thousand), diff = max-min, mean = mean(weighted_cases_per_thousand))
phase_estimate_under19 <- mean(month_maxes_under19$max_month)
amp_estimate_under19 <- mean(month_maxes_under19$diff)/2
offset_estimate_under19 <- mean(month_maxes_under19$mean)


#initialize a dataframe to store state sinusoid predictions and parameters
state_sinusoids_under19 <- data.frame()
state_sinusoids_parameters_under19 <- data.frame("State" = unique(state_weighted_vis_allyears_under19$STATE),
                                         "Amplitude" = NA,
                                         "Phase" = NA,
                                         "Offset" = NA,
                                         "RSS" = NA,
                                         "Amplitude_SE" = NA,
                                         "Phase_SE" = NA,
                                         "Offset_SE" = NA)


#fit sinusoids for each state, store parameters and predictions
for(i in 1:length(state_sinusoids_parameters_under19$State)){
  state <- state_sinusoids_parameters_under19$State[i] #assign state
  tmp <- state_weighted_vis_allyears_under19 |> 
    filter(STATE == state) #this is a dataframe with just the state
  mod <- tmp |> 
    use_nls_func_params() #this creates the sinusoidal model
  state_sinusoids_parameters_under19[i,2] <- coef(mod)[[1]]
  state_sinusoids_parameters_under19[i,3] <- coef(mod)[[2]]
  state_sinusoids_parameters_under19[i,4] <- coef(mod)[[3]]
  state_sinusoids_parameters_under19[i,5] <- sum(residuals(mod)^2) #this probably needs to be fixed now that there are multiple years
  state_sinusoids_parameters_under19[i,6] <- summary(mod)$coefficients[1,2]
  state_sinusoids_parameters_under19[i,7] <- summary(mod)$coefficients[2,2]
  state_sinusoids_parameters_under19[i,8] <- summary(mod)$coefficients[3,2]
  tmp2 <- state_weighted_vis_avg_under19 |>
    filter(STATE == state) |>
    mutate(prediction = coef(mod)[[1]]* cos(omega*(MONTH - coef(mod)[[2]])) + coef(mod)[[3]] ) 
  state_sinusoids_under19 <- rbind(state_sinusoids_under19, tmp2)
}

#add confidence intervals to state sinusoids

state_ci_df_under19 <- data.frame()

for(i in 1:length(state_sinusoids_parameters_under19$State)){
  state_amps <- rnorm(10000, mean = state_sinusoids_parameters_under19$Amplitude[i], sd = state_sinusoids_parameters_under19$Amplitude_SE[i]) #SE vs SD?
  state_phases <- rnorm(10000, mean = state_sinusoids_parameters_under19$Phase[i], sd = state_sinusoids_parameters_under19$Phase_SE[i])
  state_offsets <- rnorm(10000, mean = state_sinusoids_parameters_under19$Offset[i], sd = state_sinusoids_parameters_under19$Offset_SE[i])
  state_boot <- data.frame("Amp" = state_amps, "Phase" = state_phases, "Offset" = state_offsets)
  state_boot <- state_boot |>
    mutate("Jan" = Amp* cos(omega * (1-Phase))+ Offset,
           "Feb" = Amp* cos(omega * (2-Phase))+ Offset,
           "Mar" = Amp* cos(omega * (3-Phase))+ Offset,
           "Apr" = Amp* cos(omega * (4-Phase))+ Offset,
           "May" = Amp* cos(omega * (5-Phase))+ Offset,
           "Jun" = Amp* cos(omega * (6-Phase))+ Offset,
           "Jul" = Amp* cos(omega * (7-Phase))+ Offset,
           "Aug" = Amp* cos(omega * (8-Phase))+ Offset,
           "Sep" = Amp* cos(omega * (9-Phase))+ Offset,
           "Oct" = Amp* cos(omega * (10-Phase))+ Offset,
           "Nov" = Amp* cos(omega * (11-Phase))+ Offset,
           "Dec" = Amp* cos(omega * (12-Phase))+ Offset)
  lower_upper <- t(apply(state_boot[4:15], 2, range_col))
  names <- rownames(lower_upper)
  lower_upper <- as.tibble(lower_upper)
  rownames(lower_upper) = names
  colnames(lower_upper) = c("lower", "upper")
  lower_upper_state <- lower_upper |>
    mutate(MONTH = c(1:12),
           STATE = state_sinusoids_parameters_under19$State[i])
  state_ci_df_under19 <- rbind(state_ci_df_under19, lower_upper_state)
  
}
state_sinusoids_withCIs_under19 <- left_join(state_sinusoids_under19, state_ci_df_under19)
state_sinusoids_withCIs_cont_under19 <- state_sinusoids_withCIs_under19 |> filter(!STATE %in% c("Hawaii", "Alaska", "South Carolina"))
state_sinusoids_withCIs_cont_sub_under19 <- subregions |> 
  mutate(Region = part) |> 
  select(-part) |> 
  left_join(subcolors_df) |> 
  right_join(state_sinusoids_withCIs_cont_under19) |>
  mutate(Region = factor(Region, levels = c("East South Central", "West South Central",
                                            "West North Central", "South Atlantic", "Middle Atlantic",
                                            "Mountain West", "East North Central", "New England", "Pacific West")))

#Now do the same in >19
state_weighted_vis_allyears_over19 <- over_19_pop |>
  select(MONTH, STATE, SEX, AGEGRP, NVISITS, YEAR, NMEMB, POPSIZE) |>
  left_join(total_state_pops) |>
  mutate(pop_frac = POPSIZE / total_state_pop) |>
  mutate(group_weight = pop_frac/NMEMB) |>
  group_by(STATE, MONTH, YEAR) |>
  summarize(weighted_cases = sum(NVISITS*group_weight)) |>
  mutate(weighted_cases_per_thousand = weighted_cases*1000) |>
  mutate(region = tolower(STATE))|>
  left_join(regions) |>
  select(STATE, MONTH, YEAR, weighted_cases_per_thousand, part) 

state_weighted_vis_avg_over19 <- state_weighted_vis_allyears_over19 |> 
  group_by(STATE, MONTH, part) |>
  summarize(mean_weighted_cases_per_thousand = mean(weighted_cases_per_thousand), sd = sd(weighted_cases_per_thousand))

#now fit sinusoids to the data from all years
month_maxes_over19 <- state_weighted_vis_allyears_over19 |> 
  filter(!STATE %in% c("Hawaii", "Alaska")) |>
  group_by(STATE, YEAR) |>
  summarize(max = max(weighted_cases_per_thousand), max_month = MONTH[which(weighted_cases_per_thousand== max)], 
            min = min(weighted_cases_per_thousand), diff = max-min, mean = mean(weighted_cases_per_thousand))
phase_estimate_over19 <- mean(month_maxes_over19$max_month)
amp_estimate_over19 <- mean(month_maxes_over19$diff)/2
offset_estimate_over19 <- mean(month_maxes_over19$mean)


#initialize a dataframe to store state sinusoid predictions and parameters
state_sinusoids_over19 <- data.frame()
state_sinusoids_parameters_over19 <- data.frame("State" = unique(state_weighted_vis_allyears_over19$STATE),
                                                 "Amplitude" = NA,
                                                 "Phase" = NA,
                                                 "Offset" = NA,
                                                 "RSS" = NA,
                                                 "Amplitude_SE" = NA,
                                                 "Phase_SE" = NA,
                                                 "Offset_SE" = NA)


#fit sinusoids for each state, store parameters and predictions
for(i in 1:length(state_sinusoids_parameters_over19$State)){
  state <- state_sinusoids_parameters_over19$State[i] #assign state
  tmp <- state_weighted_vis_allyears_over19 |> 
    filter(STATE == state) #this is a dataframe with just the state
  mod <- tmp |> 
    use_nls_func_params() #this creates the sinusoidal model
  state_sinusoids_parameters_over19[i,2] <- coef(mod)[[1]]
  state_sinusoids_parameters_over19[i,3] <- coef(mod)[[2]]
  state_sinusoids_parameters_over19[i,4] <- coef(mod)[[3]]
  state_sinusoids_parameters_over19[i,5] <- sum(residuals(mod)^2) #this probably needs to be fixed now that there are multiple years
  state_sinusoids_parameters_over19[i,6] <- summary(mod)$coefficients[1,2]
  state_sinusoids_parameters_over19[i,7] <- summary(mod)$coefficients[2,2]
  state_sinusoids_parameters_over19[i,8] <- summary(mod)$coefficients[3,2]
  tmp2 <- state_weighted_vis_avg_over19 |>
    filter(STATE == state) |>
    mutate(prediction = coef(mod)[[1]]* cos(omega*(MONTH - coef(mod)[[2]])) + coef(mod)[[3]] ) 
  state_sinusoids_over19 <- rbind(state_sinusoids_over19, tmp2)
}

#add confidence intervals to state sinusoids

state_ci_df_over19 <- data.frame()

for(i in 1:length(state_sinusoids_parameters_over19$State)){
  state_amps <- rnorm(10000, mean = state_sinusoids_parameters_over19$Amplitude[i], sd = state_sinusoids_parameters_over19$Amplitude_SE[i]) #SE vs SD?
  state_phases <- rnorm(10000, mean = state_sinusoids_parameters_over19$Phase[i], sd = state_sinusoids_parameters_over19$Phase_SE[i])
  state_offsets <- rnorm(10000, mean = state_sinusoids_parameters_over19$Offset[i], sd = state_sinusoids_parameters_over19$Offset_SE[i])
  state_boot <- data.frame("Amp" = state_amps, "Phase" = state_phases, "Offset" = state_offsets)
  state_boot <- state_boot |>
    mutate("Jan" = Amp* cos(omega * (1-Phase))+ Offset,
           "Feb" = Amp* cos(omega * (2-Phase))+ Offset,
           "Mar" = Amp* cos(omega * (3-Phase))+ Offset,
           "Apr" = Amp* cos(omega * (4-Phase))+ Offset,
           "May" = Amp* cos(omega * (5-Phase))+ Offset,
           "Jun" = Amp* cos(omega * (6-Phase))+ Offset,
           "Jul" = Amp* cos(omega * (7-Phase))+ Offset,
           "Aug" = Amp* cos(omega * (8-Phase))+ Offset,
           "Sep" = Amp* cos(omega * (9-Phase))+ Offset,
           "Oct" = Amp* cos(omega * (10-Phase))+ Offset,
           "Nov" = Amp* cos(omega * (11-Phase))+ Offset,
           "Dec" = Amp* cos(omega * (12-Phase))+ Offset)
  lower_upper <- t(apply(state_boot[4:15], 2, range_col))
  names <- rownames(lower_upper)
  lower_upper <- as.tibble(lower_upper)
  rownames(lower_upper) = names
  colnames(lower_upper) = c("lower", "upper")
  lower_upper_state <- lower_upper |>
    mutate(MONTH = c(1:12),
           STATE = state_sinusoids_parameters_over19$State[i])
  state_ci_df_over19 <- rbind(state_ci_df_over19, lower_upper_state)
  
}
state_sinusoids_withCIs_over19 <- left_join(state_sinusoids_over19, state_ci_df_over19)
state_sinusoids_withCIs_cont_over19 <- state_sinusoids_withCIs_over19 |> filter(!STATE %in% c("Hawaii", "Alaska", "South Carolina"))
state_sinusoids_withCIs_cont_sub_over19 <- subregions |> 
  mutate(Region = part) |> 
  select(-part) |> 
  left_join(subcolors_df) |> 
  right_join(state_sinusoids_withCIs_cont_over19) |>
  mutate(Region = factor(Region, levels = c("East South Central", "West South Central",
                                            "West North Central", "South Atlantic", "Middle Atlantic",
                                            "Mountain West", "East North Central", "New England", "Pacific West")))


#---------
#plot state sinusoid residuals (for non-logged model): 
state_sinusoids_resid <- left_join(state_weighted_vis_allyears, state_sinusoids) |> filter(!STATE %in% c("Alaska", "Hawaii", "South Carolina")) |>
  mutate(resid = weighted_cases_per_thousand - prediction) |> select(-part) |> left_join(subregions)
residuals_plot <- state_sinusoids_resid |> ggplot(aes(x = MONTH, y = resid, colo)) + geom_point(size = 0.2) + facet_wrap(~STATE)
residuals_plot_notlogged <- state_sinusoids |> mutate(resid = mean_weighted_cases_per_thousand - prediction) |>
  select(-part) |> left_join(subregions) |> filter(!STATE %in% c("Alaska", "Hawaii", "South Carolina")) |>
  mutate(Region = factor(part, levels = c("East South Central", "West South Central",
                                            "West North Central", "South Atlantic", "Middle Atlantic",
                                            "Mountain West", "East North Central", "New England", "Pacific West"))) |>
  ggplot(aes(x = MONTH, y = resid, color = Region)) + geom_point(size = 0.35) + facet_wrap(~STATE) +
  scale_color_manual(values= subcolors_df$region_color, labels=gsub("_", " ",subcolors_df$Region , fixed=TRUE)) +
  scale_fill_manual(values = subcolors_df$region_color, labels=gsub("_", " ",subcolors_df$Region , fixed=TRUE)) +
  facet_wrap(~STATE) +
  labs(color = "Subregion", fill = "Subregion") + 
  xlab("Month") +
  ylab("Visits per 1,000 People") +
  scale_x_discrete(limits = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT","OCT", "NOV", "DEC")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 7.5))











