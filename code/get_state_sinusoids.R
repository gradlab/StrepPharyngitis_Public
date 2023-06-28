#fitting state data to sinusoids

#define a function for sinusoid fitting
use_nls_func_params = function(df) {
  nls(weighted_cases_per_thousand ~ A * cos(omega * (MONTH - phase)) + offset,
      start = list(A = amp_estimate, phase = phase_estimate, offset = phase_estimate),
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


#fit sinusoids for each state, store parameters and predictions
for(i in 1:length(state_sinusoids_parameters$State)){
  state <- state_sinusoids_parameters$State[i] #assign state
  tmp <- state_weighted_vis_allyears |> 
    filter(STATE == state) #this is a dataframe with just the state
  mod <- tmp |> 
    use_nls_func_params() #this creates the sinusoidal model
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









