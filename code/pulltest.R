library(tidyverse) 

GeoCohort <- read_csv("output/GeoCohort.csv")
GeoVisits <- read_csv("output/GeoVisits.csv")

GeoVisits %>% 
	filter(COND=="Strep pharyngitis") %>% 
	left_join(GeoCohort, by=c("STATE","SEX","AGEGRP","YEAR")) %>% 
	mutate(VPKP=NVISITS/NMEMB*1000) %>% 
	group_by(MONTH) %>% 
	summarise(VPKP=mean(VPKP))

