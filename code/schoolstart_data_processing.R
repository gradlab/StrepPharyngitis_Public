library(tidyverse)
library(lubridate)
library(readxl)
library(RColorBrewer)
library(gridExtra)
set.seed(2023)

#load in full data
ss_dat <- read_excel("/Users/madeleinekline/Dropbox (Harvard University)/GradLab/Strep_project/private_data/pew_school_start data_for_MKline.xlsx")
ss_dat <- ss_dat |> mutate(date = as_date(as.numeric(ss_dat$`Start date, 2019-20`) - 1))
#adjust dates to match spreadsheet

year(ss_dat$date) <- 2019
pal <- brewer.pal(n= 9, name = "Paired")
subcolors_df <- data.frame(Subregion = c("East South Central", "West South Central",
                                         "West North Central", "South Atlantic", "Middle Atlantic",
                                         "Mountain West", "East North Central", "New England", "Pacific West"),
                           region_color = c(pal[6], pal[5], pal[4], pal[9], pal[2], pal[8], pal[3], pal[1], pal[7]))
#select necessary columns only, filter out Hawaii and Alaska
ss_dat_trim <- ss_dat |> select(`District Name`, `County Name*`, State, `Census Division`, date) |> filter(!State %in% c("HI", "AK"))

#manually input dates that were ranges in original dataest, picking earlier date
ss_dat_trim[which(ss_dat_trim$`District Name` == "Adams 12 Five Star Schools"),5] <-  ymd("2019 08 14")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Boulder Valley School District No. Re2'),5] <-  ymd("2019 08 14")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Red Clay Consolidated School District'),5] <-  ymd("2019 08 26")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Christina School District'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Appoquinimink School District'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Brandywine School District'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Colonial School District'),5] <-  ymd("2019 08 27")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Capital School District'),5] <-  ymd("2019 08 22")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Smyrna School District'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'IDAHO FALLS DISTRICT'),5] <-  ymd("2019 08 26")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Olathe Public Schools USD 233'),5] <-  ymd("2019 08 14")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Shawnee Mission School District'),5] <-  ymd("2019 08 12")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Kansas City Kansas Public Schools'),5] <-  ymd("2019 08 07")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Topeka Public Schools'),5] <-  ymd("2019 08 13")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Lawrence Public Schools USD 497'),5] <-  ymd("2019 08 14")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Andover Public Schools USD 385'),5] <-  ymd("2019 08 15")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Anne Arundel County Public Schools'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Lewiston Public Schools'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'RSU 06/MSAD 06'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Auburn School Department'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Oxford Hills School District'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Windham Raymond Schools RSU 14'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'RSU 57/MSAD 57'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'South Portland School Department'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'SOUTH WASHINGTON COUNTY SCHOOL DIST'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'LAMAR COUNTY SCHOOL DISTRICT'),5] <-  ymd("2019 08 07")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Missoula County Public Schools'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Helena Public Schools'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Kalispell Public Schools'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'GRAND FORKS PUBLIC SCHOOLS 1'),5] <-  ymd("2019 08 26")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'GRAND ISLAND PUBLIC SCHOOLS'),5] <-  ymd("2019 08 15")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'RIO RANCHO PUBLIC SCHOOLS'),5] <-  ymd("2019 08 13")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Toledo Public Schools'),5] <-  ymd("2019 08 17")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'South-Western City School District'),5] <-  ymd("2019 08 22")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Akron Public Schools'),5] <-  NA #take this out, unclear
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Salem-Keizer Public Schools SD 24J'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Bend-LaPine Administrative SD 1'),5] <-  ymd("2019 09 04")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Eugene School District 4J'),5] <-  ymd("2019 09 05")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Tigard-Tualatin School District 23J'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Gresham-Barlow School District 10J'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Pittsburgh SD'),5] <-  ymd("2019 08 26")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Warwick Public Schools'),5] <-  ymd("2019 08 29")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Woonsocket Education Department'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'East Providence School Department'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Cumberland School Department'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'North Kingstown School Department'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'North Providence School Department'),5] <-  ymd("2019 09 05")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'CHESTERFIELD COUNTY PUBLIC SCHOOLS'),5] <-  ymd("2019 08 19")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Jordan District'),5] <-  ymd("2019 09 03")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'South Burlington School District'),5] <-  ymd("2019 08 26")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Mount Anthony UHSD #14'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Springfield School District'),5] <-  ymd("2019 08 28")
ss_dat_trim[which(ss_dat_trim$`District Name` == 'Madison Metropolitan School District'),5] <-  ymd("2019 09 03")

#which ones are still NA?
ss_dat_trim[which(is.na(ss_dat_trim$date)),] #Akron and last row, remove them both
#remove last row
ss_dat_trim <- ss_dat_trim[1:498,]
ss_dat_trim <- ss_dat_trim |> filter(`District Name` != "Akron Public Schools")
#add region, get averages and ranges
ss_dat_trim[which(ss_dat_trim$`Census Division` == "Mountain"),4] <- "Mountain West"
ss_dat_trim[which(ss_dat_trim$`Census Division` == "Pacific"),4] <- "Pacific West"

#don't write this to CSV because this should be kept private
#write this to a CSV but do not share it on the public github

#write_csv(ss_dat_trim, "/Users/madeleinekline/Dropbox (Harvard University)/GradLab/Strep_project/private_data/ss_dat_trim.csv")

start_date_summary <- ss_dat_trim |> mutate(Subregion = `Census Division`) |> group_by(Subregion) |>
  summarize(Date = mean(date), lower_date = min(date), upper_date= max(date)) |> mutate(cat = "SchoolStart")

start_date_summary_state <- ss_dat_trim |> group_by(State) |>
  summarize(Date = mean(date), lower_date = min(date), upper_date= max(date)) |> mutate(cat = "SchoolStart")

#write_csv(start_date_summary, "/Users/madeleinekline/Dropbox (Harvard University)/GradLab/StrepPharyngitis/output/school_starts_summary.csv")

#write_csv(start_date_summary_state, "/Users/madeleinekline/Dropbox (Harvard University)/GradLab/StrepPharyngitis/output/school_starts_summary_state.csv")





