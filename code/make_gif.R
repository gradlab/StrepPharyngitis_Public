library(tidyverse)
library(ggplot2)
library(gridExtra)
library(rgdal)
library(broom)
library(ggpattern)
library(gifski)
library(animation)

#calculate total state populations
total_state_pops <- pop_sizes |>
  group_by(STATE) |>
  summarize(total_state_pop = sum(POPSIZE))

weight_states_all_years <- coh_dat_pop |>
  select(MONTH, STATE, SEX, AGEGRP, NVISITS, YEAR, NMEMB, POPSIZE) |>
  left_join(total_state_pops) |>
  mutate(pop_frac = POPSIZE / total_state_pop) |>
  mutate(group_weight = pop_frac/NMEMB) |>
  group_by(STATE, MONTH, YEAR) |>
  summarize(weighted_cases = sum(NVISITS*group_weight)) |>
  mutate(weighted_cases_per_thousand = weighted_cases*1000) |>
  mutate(region = tolower(STATE))|>
  left_join(regions) |> 
  mutate(id = STATE) |>  
  select(STATE, MONTH, YEAR, weighted_cases_per_thousand, part, id) 

#add a time component that is running; will have 1-108 months in total
weight_states_all_years <- weight_states_all_years |>
  mutate(TIME = MONTH + 12*(YEAR-2010))

#loading in map components
my_spdf <- readOGR(dsn = "../data/cb_2018_us_state_500k",
                   layer = "cb_2018_us_state_500k")
spdf_fortified <- tidy(my_spdf)
non_continental_ids <- which(!(my_spdf$NAME %in% c("American Samoa" , 
                                                   "Commonwealth of the Northern Mariana Islands", 
                                                   "Guam",
                                                   "Hawaii",
                                                   "Alaska",
                                                   "Puerto Rico",
                                                   "United States Virgin Islands")))
continental <- my_spdf[non_continental_ids,]
continental_fortified <- tidy(continental)
temp_df <- data.frame(my_spdf@data$NAME)
names(temp_df) <- c("NAME")
temp_df$id <-as.character(seq(0, nrow(temp_df)-1))
state_names <- left_join(continental_fortified, temp_df) |> mutate(State = NAME) |> select(-NAME)
continental_fortified_tojoin <- left_join(continental_fortified, state_names) |> select(-id) |> mutate(id = State) |> select(-State)

master_df <- left_join(continental_fortified_tojoin, weight_states_all_years)


nframes <- unique(weight_states_all_years$TIME) #108 frames


saveGIF({
  for (f in 1:length(nframes)) { # upper lim: nframes; start by seeing if you can do 5 frames 
    #filter master_df to just that timestep
    #define SC here
    sc <- master_df[which(master_df$id == "South Carolina"),]
    df <- master_df |> filter(TIME == f)
    p1 <- df |>
      ggplot() + geom_polygon(aes(x = long, y = lat, group = group, fill = weighted_cases_per_thousand), color="black") +
      geom_polygon_pattern(data = sc, aes(x = long, y = lat, group= group), fill = "gray", pattern = 'stripe',
                           pattern_spacing = 0.005, pattern_alpha = 0.5) +
      coord_map("mercator") +
      theme_void() + ggtitle("") + scale_fill_gradient(low = "#FFFFFF", high =  "red", limits = c(0, 10.5)) + #need to make sure this is the same for all of them
      labs(fill = "Visits per 1000", 
           title = "GAS Pharyngitis Visits per 1000 People") +
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            plot.subtitle = element_text(hjust = 0.5, size = 13))
    p2 <- ggplot() + 
      geom_vline(xintercept=seq(0,12,1), size=0.5, color="grey") + 
      geom_rect(aes(xmin=0, xmax=12, ymin=0, ymax=1), col="black", fill=NA) + 
      geom_vline(xintercept=seq(1,12,1), size=0.5) + 
      geom_vline(xintercept=df$MONTH[1] - 0.5, size=14.5, color="black")+  
      coord_fixed() + 
      scale_x_continuous(breaks=seq(0.5,11.5,1), labels=c("Jan","Feb","Mar","Apr","May",
                                                          "Jun", "Jul", "Aug", "Sep",
                                                          "Oct", "Nov", "Dec")) + 
      theme(panel.background=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            plot.margin=grid::unit(c(0,0,0,0),"mm"),
            text=element_text(size=14),
            axis.text.x = element_text(angle = 45, vjust = 0.6))
    p3 <- ggplot() + 
      geom_vline(xintercept=seq(0,9,1), size=0.5, color="grey") + 
      geom_rect(aes(xmin=0, xmax=9, ymin=0, ymax=1), col="black", fill=NA) + 
      geom_vline(xintercept=seq(1,9,1), size=0.5) + 
      geom_vline(xintercept=df$YEAR[1]- 2009.5, size=13.5, color="black")+  #year - 2009.5
      coord_fixed() + 
      scale_x_continuous(breaks=seq(0.5,8.5,1), labels=c("2010","2011","2012","2013","2014","2015", "2016", "2017", "2018")) + 
      theme(panel.background=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            plot.margin=grid::unit(c(0,0,0,0),"mm"),
            text=element_text(size=14),
            axis.text.x = element_text(angle = 45, vjust = 0.6))
    lay <- rbind(c(1,1),
                 c(1,1),
                 c(1,1),
                 c(1,1),
                 c(1,1),
                 c(1,1),
                 c(1,1),
                 c(2,2),
                 c(3,3))
    p <- grid.arrange(p1, p2, p3, ncol=1, layout_matrix = lay)
    
    print(p)
  }
}, movie.name = "finalgif.gif", interval = 0.25, width = 9, height = 6.5)


