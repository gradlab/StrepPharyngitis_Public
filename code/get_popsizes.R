#Get census data and convert to same age ranges as visit data
#0-4, 5-9, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69

#largely borrowed from here https://github.com/skissler/PediatricPrescribing_Chronic/blob/main/code/get_popsizes_under5.R
## need to cite?

get_census_agegroup <- function(code_male, code_female, agegrp){
  get_acs(
    geography="state", 
    variables=c(Male=code_male,Female=code_female),
    year=2015) %>% #this gives 2011-2015 5-year time frame
    select(-GEOID, -moe) %>% 
    rename(STATE="NAME") %>% 
    rename(POPSIZE="estimate") %>% 
    mutate(SEX=case_when(substr(variable,1,1)=="M"~1,TRUE~2)) %>% 
    select(STATE, SEX, POPSIZE) %>% 
    filter(STATE!="Puerto Rico") %>%
    mutate(STATE=case_when(STATE=="District of Columbia"~"Washington DC", TRUE~STATE)) |>
    mutate(AGEGRP = agegrp)
}

#using age/sex codes from ACS from here: https://www.socialexplorer.com/data/ACS2015/metadata/?ds=ACS15&table=B01001 

#00_04
popsizes_under5 <- get_census_agegroup("B01001_003", "B01001_027", "00_04")

#05_09
popsizes_5_9 <- get_census_agegroup("B01001_004", "B01001_028", "05_09")

#10-19
popsizes_10_14 <- get_census_agegroup("B01001_005", "B01001_029", "10_14")
popsizes_15_17 <- get_census_agegroup("B01001_006", "B01001_030", "15_17")
popsizes_18_19 <- get_census_agegroup("B01001_007", "B01001_031", "18_19")
popsizes_10_19 <- data.frame(STATE = popsizes_10_14$STATE, 
                             SEX = popsizes_10_14$SEX, 
                             POPSIZE = popsizes_10_14$POPSIZE + 
                               popsizes_15_17$POPSIZE + 
                               popsizes_18_19$POPSIZE,
                             AGEGRP = "10_19")
#20_29
popsizes_20 <- get_census_agegroup("B01001_008","B01001_032", "20")
popsizes_21 <- get_census_agegroup("B01001_009","B01001_033", "21")
popsizes_22_24 <- get_census_agegroup("B01001_010","B01001_034", "22_24")
popsizes_25_29 <- get_census_agegroup("B01001_011","B01001_035", "25_29")
popsizes_20_29 <- data.frame(STATE = popsizes_20$STATE,
                             SEX = popsizes_20$SEX,
                             POPSIZE = popsizes_20$POPSIZE +
                               popsizes_21$POPSIZE +
                               popsizes_22_24$POPSIZE +
                               popsizes_25_29$POPSIZE,
                             AGEGRP = "20_29")

#30-39
popsizes_30_34 <- get_census_agegroup("B01001_012","B01001_036", "30_34")
popsizes_35_39 <- get_census_agegroup("B01001_013","B01001_037", "35_39")
popsizes_30_39 <- data.frame(STATE = popsizes_30_34$STATE,
                             SEX = popsizes_30_34$SEX,
                             POPSIZE = popsizes_30_34$POPSIZE + 
                               popsizes_35_39$POPSIZE,
                             AGEGRP = "30_39")
#40-49
popsizes_40_44 <- get_census_agegroup("B01001_014","B01001_038", "40_44")
popsizes_45_49 <- get_census_agegroup("B01001_015","B01001_039", "45_49")
popsizes_40_49 <- data.frame(STATE = popsizes_40_44$STATE,
                             SEX = popsizes_40_44$SEX,
                             POPSIZE = popsizes_40_44$POPSIZE + 
                               popsizes_45_49$POPSIZE,
                             AGEGRP = "40_49")
#50-59
popsizes_50_54 <- get_census_agegroup("B01001_016","B01001_040", "50_54")
popsizes_55_59 <- get_census_agegroup("B01001_017","B01001_041", "55_59")
popsizes_50_59 <- data.frame(STATE = popsizes_50_54$STATE,
                             SEX = popsizes_50_54$SEX,
                             POPSIZE = popsizes_50_54$POPSIZE + 
                               popsizes_55_59$POPSIZE,
                             AGEGRP = "50_59")


#60-69
popsizes_60_61 <- get_census_agegroup("B01001_018","B01001_042", "60_61")
popsizes_62_64 <- get_census_agegroup("B01001_019","B01001_043", "62_64")
popsizes_65_66 <- get_census_agegroup("B01001_020","B01001_044", "65_66")
popsizes_67_69 <- get_census_agegroup("B01001_021","B01001_045", "67_69")
popsizes_60_69 <- data.frame(STATE = popsizes_60_61$STATE,
                             SEX = popsizes_60_61$SEX,
                             POPSIZE = popsizes_60_61$POPSIZE +
                               popsizes_62_64$POPSIZE +
                               popsizes_65_66$POPSIZE +
                               popsizes_67_69$POPSIZE,
                             AGEGRP = "60_69")

#bind all the dataframes together
pop_sizes <- rbind(popsizes_under5, 
                           popsizes_5_9, 
                           popsizes_10_19, 
                           popsizes_20_29, 
                           popsizes_30_39, 
                           popsizes_40_49, 
                           popsizes_50_59, 
                           popsizes_60_69)

#write_csv(popsizes_all_ages, file="popsizes_all_ages.csv")
















