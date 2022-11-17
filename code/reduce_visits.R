library(tidyverse) 

GeoCohort10 <- read_csv("output/private/GeoCohort10_2022-11-16.csv")
GeoCohort11 <- read_csv("output/private/GeoCohort11_2022-11-16.csv")
GeoCohort12 <- read_csv("output/private/GeoCohort12_2022-11-16.csv")
GeoCohort13 <- read_csv("output/private/GeoCohort13_2022-11-16.csv")
GeoCohort14 <- read_csv("output/private/GeoCohort14_2022-11-16.csv")
GeoCohort15 <- read_csv("output/private/GeoCohort15_2022-11-16.csv")
GeoCohort16 <- read_csv("output/private/GeoCohort16_2022-11-16.csv")
GeoCohort17 <- read_csv("output/private/GeoCohort17_2022-11-16.csv")
GeoCohort18 <- read_csv("output/private/GeoCohort18_2022-11-16.csv")

GeoVisits10 <- read_csv("output/private/GeoVisits10_2022-11-16.csv")
GeoVisits11 <- read_csv("output/private/GeoVisits11_2022-11-16.csv")
GeoVisits12 <- read_csv("output/private/GeoVisits12_2022-11-16.csv")
GeoVisits13 <- read_csv("output/private/GeoVisits13_2022-11-16.csv")
GeoVisits14 <- read_csv("output/private/GeoVisits14_2022-11-16.csv")
GeoVisits15 <- read_csv("output/private/GeoVisits15_2022-11-16.csv")
GeoVisits16 <- read_csv("output/private/GeoVisits16_2022-11-16.csv")
GeoVisits17 <- read_csv("output/private/GeoVisits17_2022-11-16.csv")
GeoVisits18 <- read_csv("output/private/GeoVisits18_2022-11-16.csv")

GeoCohort <- bind_rows(
	mutate(GeoCohort10,YEAR="2010"),
	mutate(GeoCohort11,YEAR="2011"),
	mutate(GeoCohort12,YEAR="2012"),
	mutate(GeoCohort13,YEAR="2013"),
	mutate(GeoCohort14,YEAR="2014"),
	mutate(GeoCohort15,YEAR="2015"),
	mutate(GeoCohort16,YEAR="2016"),
	mutate(GeoCohort17,YEAR="2017"),
	mutate(GeoCohort18,YEAR="2018"))

GeoVisits <- bind_rows(
	mutate(GeoVisits10,YEAR="2010"),
	mutate(GeoVisits11,YEAR="2011"),
	mutate(GeoVisits12,YEAR="2012"),
	mutate(GeoVisits13,YEAR="2013"),
	mutate(GeoVisits14,YEAR="2014"),
	mutate(GeoVisits15,YEAR="2015"),
	mutate(GeoVisits16,YEAR="2016"),
	mutate(GeoVisits17,YEAR="2017"),
	mutate(GeoVisits18,YEAR="2018")) %>% 
	filter(PRIMARYCOND=="Strep pharyngitis")

write_csv(GeoCohort, file="output/GeoCohort.csv")
write_csv(GeoVisits, file="output/GeoVisits.csv")