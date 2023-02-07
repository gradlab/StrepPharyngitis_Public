library(tidyverse) 

GeoCohort10 <- read_csv("output/private/GeoCohort10_2023-02-06.csv")
GeoCohort11 <- read_csv("output/private/GeoCohort11_2023-02-06.csv")
GeoCohort12 <- read_csv("output/private/GeoCohort12_2023-02-06.csv")
GeoCohort13 <- read_csv("output/private/GeoCohort13_2023-02-06.csv")
GeoCohort14 <- read_csv("output/private/GeoCohort14_2023-02-06.csv")
GeoCohort15 <- read_csv("output/private/GeoCohort15_2023-02-06.csv")
GeoCohort16 <- read_csv("output/private/GeoCohort16_2023-02-06.csv")
GeoCohort17 <- read_csv("output/private/GeoCohort17_2023-02-06.csv")
GeoCohort18 <- read_csv("output/private/GeoCohort18_2023-02-06.csv")

GeoVisitsc110 <- read_csv("output/private/GeoVisitsc110_2023-02-06.csv")
GeoVisitsc111 <- read_csv("output/private/GeoVisitsc111_2023-02-06.csv")
GeoVisitsc112 <- read_csv("output/private/GeoVisitsc112_2023-02-06.csv")
GeoVisitsc113 <- read_csv("output/private/GeoVisitsc113_2023-02-06.csv")
GeoVisitsc114 <- read_csv("output/private/GeoVisitsc114_2023-02-06.csv")
GeoVisitsc115 <- read_csv("output/private/GeoVisitsc115_2023-02-06.csv")
GeoVisitsc116 <- read_csv("output/private/GeoVisitsc116_2023-02-06.csv")
GeoVisitsc117 <- read_csv("output/private/GeoVisitsc117_2023-02-06.csv")
GeoVisitsc118 <- read_csv("output/private/GeoVisitsc118_2023-02-06.csv")

GeoVisitsc210 <- read_csv("output/private/GeoVisitsc210_2023-02-06.csv")
GeoVisitsc211 <- read_csv("output/private/GeoVisitsc211_2023-02-06.csv")
GeoVisitsc212 <- read_csv("output/private/GeoVisitsc212_2023-02-06.csv")
GeoVisitsc213 <- read_csv("output/private/GeoVisitsc213_2023-02-06.csv")
GeoVisitsc214 <- read_csv("output/private/GeoVisitsc214_2023-02-06.csv")
GeoVisitsc215 <- read_csv("output/private/GeoVisitsc215_2023-02-06.csv")
GeoVisitsc216 <- read_csv("output/private/GeoVisitsc216_2023-02-06.csv")
GeoVisitsc217 <- read_csv("output/private/GeoVisitsc217_2023-02-06.csv")
GeoVisitsc218 <- read_csv("output/private/GeoVisitsc218_2023-02-06.csv")

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

GeoVisitsc1 <- bind_rows(
	mutate(GeoVisitsc110,YEAR="2010"),
	mutate(GeoVisitsc111,YEAR="2011"),
	mutate(GeoVisitsc112,YEAR="2012"),
	mutate(GeoVisitsc113,YEAR="2013"),
	mutate(GeoVisitsc114,YEAR="2014"),
	mutate(GeoVisitsc115,YEAR="2015"),
	mutate(GeoVisitsc116,YEAR="2016"),
	mutate(GeoVisitsc117,YEAR="2017"),
	mutate(GeoVisitsc118,YEAR="2018")) %>% 
	select(COND=COND1, MONTH, YEAR, STATE, SEX, AGEGRP, NVISITS1=NVISITS)

GeoVisitsc2 <- bind_rows(
	mutate(GeoVisitsc210,YEAR="2010"),
	mutate(GeoVisitsc211,YEAR="2011"),
	mutate(GeoVisitsc212,YEAR="2012"),
	mutate(GeoVisitsc213,YEAR="2013"),
	mutate(GeoVisitsc214,YEAR="2014"),
	mutate(GeoVisitsc215,YEAR="2015"),
	mutate(GeoVisitsc216,YEAR="2016"),
	mutate(GeoVisitsc217,YEAR="2017"),
	mutate(GeoVisitsc218,YEAR="2018")) %>% 
	select(COND=COND2, MONTH, YEAR, STATE, SEX, AGEGRP, NVISITS2=NVISITS)

GeoVisits <- full_join(GeoVisitsc1, GeoVisitsc2, by=c("COND","MONTH","YEAR","STATE","SEX","AGEGRP")) %>% 
	replace_na(list(NVISITS1=0, NVISITS2=0)) %>% 
	mutate(NVISITS=NVISITS1+NVISITS2) %>% 
	select(COND, MONTH, YEAR, STATE, SEX, AGEGRP, NVISITS)

condlist <- c("Acute sinusitis", "Bacterial pneumonia", "Influenza", "Other pharyngitis", "Other/None", "Otitis media", "SSTIs", "UTIs", "Viral pneumonia", "Strep pharyngitis", "Fungal pneumonia", "GI infections")
monthlist <- as.double(c(1:12))
yearlist <- as.character(2010:2018)
statelist <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","Washington DC","West Virginia","Wisconsin","Wyoming")
sexlist <- c(1,2)
agegrplist <- c("00_04", "05_09", "10_19", "20_29", "30_39", "40_49", "50_59", "60_69")

fullset <- expand_grid(condlist, monthlist, yearlist, statelist, sexlist, agegrplist) %>% 
	rename(COND=condlist, MONTH=monthlist, YEAR=yearlist, STATE=statelist, SEX=sexlist, AGEGRP=agegrplist)

GeoVisits <- full_join(fullset, GeoVisits, by=c("COND","MONTH","YEAR","STATE","SEX","AGEGRP")) %>% 
	replace_na(list(NVISITS=0))

write_csv(GeoCohort, file="output/GeoCohort.csv")
write_csv(GeoVisits, file="output/GeoVisits.csv")