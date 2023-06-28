library(tidyverse)
library(ggplot2)
library(gridExtra)

New_England <- data.frame(STATE = c("Connecticut", "Maine", "Massachusetts", 
                                    "New Hampshire", "Rhode Island", "Vermont"), part = "New_England")
Middle_Atlantic <- data.frame(STATE = c("New Jersey", "New York", "Pennsylvania"), part = "Middle_Atlantic")
East_North_Central <- data.frame(STATE = c("Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin"), part = "East_North_Central")
West_North_Central <- data.frame(STATE = c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", 
                                           "North Dakota","South Dakota"), part = "West_North_Central")
South_Atlantic <- data.frame(STATE = c("Delaware", "Washington DC", "Florida", "Georgia", 
                                       "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia"), part = "South_Atlantic")
East_South_Central <- data.frame(STATE = c("Alabama", "Kentucky", "Mississippi", "Tennessee"), part = "East_South_Central")
West_South_Central <- data.frame(STATE = c("Arkansas", "Louisiana", "Oklahoma", "Texas"), part = "West_South_Central")
Mountain_West <- data.frame(STATE = c("Arizona", "Colorado", "Idaho", "New Mexico", "Montana",
                                      "Utah", "Nevada", "Wyoming"), part = "Mountain_West")
Pacific_West <- data.frame(STATE = c("Alaska", "California", "Hawaii", "Oregon", "Washington"), part = "Pacific_West")

subregions <- rbind(New_England, Middle_Atlantic, East_North_Central, West_North_Central, South_Atlantic ,
                    East_South_Central, West_South_Central,  Mountain_West, Pacific_West)

subregions <- subregions |> mutate(part = gsub("_", " ", part, fixed=TRUE))
#write_csv(subregions, "subregions.csv")
