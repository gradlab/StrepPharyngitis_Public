#get regions
library(usmap)
library(tidyverse)
to_statename <- function(list){
  new_list <- c()
  for(i in 1:length(list)){
    name <- state.name[grep(list[i], state.abb)] 
    new_list <- append(new_list, name)
  }
  new_list
}

northeast_states <- tolower(to_statename(.northeast_region))
midwest_states <- tolower(to_statename(.midwest_region))
south_states <- tolower(to_statename(.south_region))
west_states <- tolower(to_statename(.west_region))

northeast_df <- data.frame(region = northeast_states, part = "northeast") 
midwest_df <- data.frame(region = midwest_states, part = "midwest")   
south_df <- data.frame(region = south_states, part = "south") 
west_df <- data.frame(region = west_states, part = "west")

dc_df <- data.frame(region = "washington dc", part = "south")
regions <- rbind(northeast_df, midwest_df, south_df, west_df, dc_df)
regions <- regions |> mutate(STATE = str_to_title(region),
                             part = str_to_title(part)) |> 
  select(STATE, part)
regions[51,1] <- "Washington DC"
#write_csv(regions, "regions.csv")
