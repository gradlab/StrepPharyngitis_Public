#functions for fitting sinusoids to regions or subregions
coh_dat_pop <- left_join(dat,coh) |> left_join(pop_sizes)
#NLS function for fitting sinusoids
use_nls_func_params_2 = function(df, omega, amp_estimate, phase_estimate, offset_estimate) {
  nls(sum_region_cases_per_thousand ~ A * cos(omega * (MONTH - phase)) + offset,
      start = list(A = amp_estimate, phase = phase_estimate, offset = offset_estimate),
      data = df)
}

#function to take quantiles in a dataframe
range_col <- function(vec){
  quantile(vec, c(0.025, 0.975))}


#function to take bootstrap samples for a sinusoid
bootstrap_region <- function(df, region){ 
  #Input df should be region_sinusoids_parameters
  #region is a string
  omega = 2 * pi / 12
  regions <- df$Region
  region_num <- match(region, regions)
  region_amps <- rnorm(10000, mean = df$Amplitude[region_num],sd = df$Amplitude_SE[region_num])
  region_phases <- rnorm(10000, mean = df$Phase[region_num],sd = df$Phase_SE[region_num])
  region_offsets <- rnorm(10000, mean = df$Offset[region_num],sd = df$Offset_SE[region_num])
  
  region_boot <-  data.frame("Amp" = region_amps, 
                             "Phase" = region_phases, 
                             "Offset" = region_offsets)
  
  region_boot <- region_boot |>
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
  
  lower_upper <- t(apply(region_boot[4:15], 2, range_col))
  names <- rownames(lower_upper)
  lower_upper <- as.tibble(lower_upper)
  rownames(lower_upper) = names
  colnames(lower_upper) = c("lower", "upper")
  lower_upper_region <- lower_upper |>
    mutate(MONTH = c(1:12),
           part = region)
  return(lower_upper_region)
}


#function to get region sinusoid parameters and predictions with confidence intervals
get_region_sinusoids <- function(states_to_remove){
  
  #create a dataframe with the total population in each region
  region_totals <- pop_sizes |> 
    mutate(region = tolower(STATE)) |>
    filter(!STATE %in% states_to_remove) |>
    left_join(regions) |> 
    select(-region) |>
    group_by(part) |>
    summarize(region_total = sum(POPSIZE))
  
  #create a dataframe that has the average visits per month in each 
  regions_weighted_allyears <- coh_dat_pop |> 
    mutate(region = tolower(STATE)) |>
    filter(!STATE %in% states_to_remove) |>
    left_join(regions) |>
    left_join(region_totals) |> 
    mutate(pop_frac = POPSIZE/region_total) |>
    mutate(group_weight = pop_frac/NMEMB) |>
    mutate(weighted_cases = NVISITS*group_weight) |>
    group_by(part, YEAR, MONTH) |>
    summarize(sum_region_cases = sum(weighted_cases)) |>
    mutate(sum_region_cases_per_thousand = sum_region_cases*1000)
  
  #create a dataframe that has  monthly visits per region averaged across all years
  regions_weighted <- coh_dat_pop |> 
    mutate(region = tolower(STATE)) |>
    filter(!STATE %in% states_to_remove) |>
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
  
  #initialize empty dataframes to fill with sinusoidal fits and parameters
  region_sinusoids <- data.frame()
  region_sinusoids_parameters <- data.frame("Region" = unique(regions_weighted_allyears$part),
                                            "Amplitude" = NA,
                                            "Phase" = NA,
                                            "Offset" = NA,
                                            "RSS" = NA,
                                            "Amplitude_SE" = NA,
                                            "Phase_SE" = NA,
                                            "Offset_SE" = NA)
  
  #obtaining baseline parameters for sinusoid fitting
  month_maxes <- regions_weighted_allyears |>
    group_by(YEAR) |>
    summarize(max = max(sum_region_cases_per_thousand), max_month = MONTH[which(sum_region_cases_per_thousand== max)], 
              min = min(sum_region_cases_per_thousand), diff = max-min, mean = mean(sum_region_cases_per_thousand))
  phase_estimate <- mean(month_maxes$max_month)
  amp_estimate <- mean(month_maxes$diff)/2
  offset_estimate <- mean(month_maxes$mean)
  omega = 2 * pi / 12
  
  for(i in 1:length(region_sinusoids_parameters$Region)){
    region <- region_sinusoids_parameters$Region[i] #assign region
    tmp <- regions_weighted_allyears |> 
      filter(part == region)
    mod <- tmp |> 
      use_nls_func_params_2(omega = omega, 
                            amp_estimate = amp_estimate, 
                            phase_estimate = phase_estimate,
                            offset_estimate = offset_estimate) #this fits the sinusoids
    region_sinusoids_parameters[i,2] <- coef(mod)[[1]]
    region_sinusoids_parameters[i,3] <- coef(mod)[[2]]
    region_sinusoids_parameters[i,4] <- coef(mod)[[3]]
    region_sinusoids_parameters[i,5] <- sum(residuals(mod)^2) 
    region_sinusoids_parameters[i,6] <- summary(mod)$coefficients[1,2]
    region_sinusoids_parameters[i,7] <- summary(mod)$coefficients[2,2]
    region_sinusoids_parameters[i,8] <- summary(mod)$coefficients[3,2]
    tmp2 <- regions_weighted|>
      filter(part == region) |>
      mutate(prediction = coef(mod)[[1]]* cos(omega*(MONTH - coef(mod)[[2]])) + coef(mod)[[3]] ) 
    region_sinusoids <- rbind(region_sinusoids, tmp2) 
  }
  
  bounds <- rbind(bootstrap_region(region_sinusoids_parameters, "Midwest"),
                  bootstrap_region(region_sinusoids_parameters, "Northeast"),
                  bootstrap_region(region_sinusoids_parameters, "South"),
                  bootstrap_region(region_sinusoids_parameters, "West"))
  
  region_sinusoids <- left_join(region_sinusoids, bounds) 
  return(list(region_sinusoids, region_sinusoids_parameters))
  
}


#get subregion sinusoids
get_subregion_sinusoids <- function(states_to_remove){
  #create a dataframe with the total population in each region
  region_totals <- pop_sizes |> 
    filter(!STATE %in% states_to_remove) |>
    left_join(subregions) |> 
    group_by(part) |>
    summarize(region_total = sum(POPSIZE))
  
  #create a dataframe that has the average visits per month in each 
  regions_weighted_allyears <- coh_dat_pop |> 
    filter(!STATE %in% states_to_remove) |>
    left_join(subregions) |>
    left_join(region_totals) |> 
    mutate(pop_frac = POPSIZE/region_total) |>
    mutate(group_weight = pop_frac/NMEMB) |>
    mutate(weighted_cases = NVISITS*group_weight) |>
    group_by(part, YEAR, MONTH) |>
    summarize(sum_region_cases = sum(weighted_cases)) |>
    mutate(sum_region_cases_per_thousand = sum_region_cases*1000)
  
  #create a dataframe that has  monthly visits per region averaged across all years
  regions_weighted <- coh_dat_pop |> 
    filter(!STATE %in% states_to_remove) |>
    left_join(subregions) |>
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
  
  #initialize empty dataframes to fill with sinusoidal fits and parameters
  region_sinusoids <- data.frame()
  region_sinusoids_parameters <- data.frame("Region" = unique(regions_weighted_allyears$part),
                                            "Amplitude" = NA,
                                            "Phase" = NA,
                                            "Offset" = NA,
                                            "RSS" = NA,
                                            "Amplitude_SE" = NA,
                                            "Phase_SE" = NA,
                                            "Offset_SE" = NA)
  
  #obtaining baseline parameters for sinusoid fitting
  month_maxes <- regions_weighted_allyears |>
    group_by(YEAR) |>
    summarize(max = max(sum_region_cases_per_thousand), max_month = MONTH[which(sum_region_cases_per_thousand== max)], 
              min = min(sum_region_cases_per_thousand), diff = max-min, mean = mean(sum_region_cases_per_thousand))
  
  
  
  phase_estimate <- mean(month_maxes$max_month)
  amp_estimate <- mean(month_maxes$diff)/2
  offset_estimate <- mean(month_maxes$mean)
  omega = 2 * pi / 12
  
  
  
  
  for(i in 1:length(region_sinusoids_parameters$Region)){
    region <- region_sinusoids_parameters$Region[i] #assign region
    tmp <- regions_weighted_allyears |> 
      filter(part == region)
    mod <- tmp |> 
      use_nls_func_params_2(omega = omega, 
                            amp_estimate = amp_estimate, 
                            phase_estimate = phase_estimate,
                            offset_estimate = offset_estimate) #this fits the sinusoids
    region_sinusoids_parameters[i,2] <- coef(mod)[[1]]
    region_sinusoids_parameters[i,3] <- coef(mod)[[2]]
    region_sinusoids_parameters[i,4] <- coef(mod)[[3]]
    region_sinusoids_parameters[i,5] <- sum(residuals(mod)^2) 
    region_sinusoids_parameters[i,6] <- summary(mod)$coefficients[1,2]
    region_sinusoids_parameters[i,7] <- summary(mod)$coefficients[2,2]
    region_sinusoids_parameters[i,8] <- summary(mod)$coefficients[3,2]
    tmp2 <- regions_weighted|>
      filter(part == region) |>
      mutate(prediction = coef(mod)[[1]]* cos(omega*(MONTH - coef(mod)[[2]])) + coef(mod)[[3]] ) 
    region_sinusoids <- rbind(region_sinusoids, tmp2) 
  }
  
  
  
  bounds <- rbind(bootstrap_region(region_sinusoids_parameters, "New England"),
                  bootstrap_region(region_sinusoids_parameters, "Middle Atlantic"),
                  bootstrap_region(region_sinusoids_parameters, "East North Central"),
                  bootstrap_region(region_sinusoids_parameters, "West North Central"),
                  bootstrap_region(region_sinusoids_parameters, "South Atlantic"),
                  bootstrap_region(region_sinusoids_parameters, "East South Central"),
                  bootstrap_region(region_sinusoids_parameters, "West South Central"),
                  bootstrap_region(region_sinusoids_parameters, "Mountain West"),
                  bootstrap_region(region_sinusoids_parameters, "Pacific West")
  )
  
  region_sinusoids <- left_join(region_sinusoids, bounds) 
  return(list(region_sinusoids, region_sinusoids_parameters))
  
}





