library(tidyverse) 

GeoCohort10flu <- read_csv("output/private/GeoCohort10flu_2022-11-16.csv")
GeoCohort11flu <- read_csv("output/private/GeoCohort11flu_2022-11-16.csv")
GeoCohort12flu <- read_csv("output/private/GeoCohort12flu_2022-11-16.csv")
GeoCohort13flu <- read_csv("output/private/GeoCohort13flu_2022-11-16.csv")
GeoCohort14flu <- read_csv("output/private/GeoCohort14flu_2022-11-16.csv")
GeoCohort15flu <- read_csv("output/private/GeoCohort15flu_2022-11-16.csv")
GeoCohort16flu <- read_csv("output/private/GeoCohort16flu_2022-11-16.csv")
GeoCohort17flu <- read_csv("output/private/GeoCohort17flu_2022-11-16.csv")
GeoCohort18flu <- read_csv("output/private/GeoCohort18flu_2022-11-16.csv")

GeoVisits10flu <- read_csv("output/private/GeoVisits10flu_2022-11-16.csv")
GeoVisits11flu <- read_csv("output/private/GeoVisits11flu_2022-11-16.csv")
GeoVisits12flu <- read_csv("output/private/GeoVisits12flu_2022-11-16.csv")
GeoVisits13flu <- read_csv("output/private/GeoVisits13flu_2022-11-16.csv")
GeoVisits14flu <- read_csv("output/private/GeoVisits14flu_2022-11-16.csv")
GeoVisits15flu <- read_csv("output/private/GeoVisits15flu_2022-11-16.csv")
GeoVisits16flu <- read_csv("output/private/GeoVisits16flu_2022-11-16.csv")
GeoVisits17flu <- read_csv("output/private/GeoVisits17flu_2022-11-16.csv")
GeoVisits18flu <- read_csv("output/private/GeoVisits18flu_2022-11-16.csv")

GeoCohortFlu <- bind_rows(
	mutate(GeoCohort10flu,YEAR="2010"),
	mutate(GeoCohort11flu,YEAR="2011"),
	mutate(GeoCohort12flu,YEAR="2012"),
	mutate(GeoCohort13flu,YEAR="2013"),
	mutate(GeoCohort14flu,YEAR="2014"),
	mutate(GeoCohort15flu,YEAR="2015"),
	mutate(GeoCohort16flu,YEAR="2016"),
	mutate(GeoCohort17flu,YEAR="2017"),
	mutate(GeoCohort18flu,YEAR="2018"))

GeoVisitsFlu <- bind_rows(
	mutate(GeoVisits10flu,YEAR="2010"),
	mutate(GeoVisits11flu,YEAR="2011"),
	mutate(GeoVisits12flu,YEAR="2012"),
	mutate(GeoVisits13flu,YEAR="2013"),
	mutate(GeoVisits14flu,YEAR="2014"),
	mutate(GeoVisits15flu,YEAR="2015"),
	mutate(GeoVisits16flu,YEAR="2016"),
	mutate(GeoVisits17flu,YEAR="2017"),
	mutate(GeoVisits18flu,YEAR="2018")) %>% 
	filter(PRIMARYCOND=="Influenza")

write_csv(GeoCohortFlu, file="output/GeoCohortFlu.csv")
write_csv(GeoVisitsFlu, file="output/GeoVisitsFlu.csv")