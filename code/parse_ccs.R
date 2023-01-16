# Author: Stephen Kissler (skissler@hsph.harvard.edu)
# Date: 10 Jan 2023
# 
# Description: This file parses the ICD-9 and ICD-10 CCS mappings from the 
# AHRQ Clinical Classification Software files. It reads the files, extracts 
# that we're intereted in, deals with duplicates, and outputs a formatted data 
# table that can be used for extracting MarketScan data. 
# 
# The CCS data are from the following webpages:
# dxref2015.csv (ICD-9 CCS):
#   https://www.hcup-us.ahrq.gov/toolssoftware/ccs/ccs.jsp
# DXCCSR_v2023-1.csv (ICD-10 CCS):
#   https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/dxccsr.jsp

# =============================================================================
# Import
# =============================================================================

library(tidyverse)

ccs9 <- read_csv("data/dxref2015formatted.csv", 
	quote="\'", 
	col_types=list(
		col_character(),
		col_double(),
		col_character(),
		col_character(),
		col_character(),
		col_character()
		))
names(ccs9) <- c("ICD9","CCS","CCSDESC","ICD9DESC","CCSOPT","CCSOPTDESC")


ccs10 <- read_csv("data/DXCCSR_v2023-1formatted.csv",
	quote="\'", 
	col_types=list(
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character()
	))
names(ccs10) <- c("ICD10","ICD10DESC","CCS","CCSDESC","CCSOP","CCSOPDESC","CCS1","CCS1DESC","CCS2","CCS2DESC","CCS3","CCS3DESC","CCS4","CCS4DESC","CCS5","CCS5DESC","CCS6","CCS6DESC")

# =============================================================================
# ICD9 mappings
# =============================================================================

ccs9 <- ccs9 %>% 
	mutate(COND=NA_character_) %>% 
	mutate(COND=case_when(
		CCS==126 & grepl("^461",ICD9) ~ "Acute sinusitis",
		CCS==126 & ICD9=="460" ~ "Other pharyngitis",
		CCS==126 & ICD9=="462" ~ "Other pharyngitis",
		CCS==126 & ICD9=="4650" ~ "Other pharyngitis",
		CCS==126 & ICD9=="340" ~ "Strep pharyngitis",
		CCS==123 & !(ICD9=="4870") ~ "Influenza",
		CCS==92 & grepl("^382",ICD9) & !(ICD9 %in% c("3821","3822","3823")) ~ "Otitis media",
		CCS==122 & (ICD9%in%c("1124","1140","1144","1145","11505","11515","11595","1363","4846","4847")) ~ "Fungal pneumonia",
		CCS==122 & (ICD9%in%c("521","551","4800","4801","4802","4803","4808","4809","4841")) ~ "Viral pneumonia",
		CCS==123 & ICD9=="4870" ~ "Viral pneumonia",
		CCS==122 & !(ICD9%in%c("1124","1140","1144","1145","11505","11515","11595","1363","4846","4847")) & !(ICD9%in%c("521","551","4800","4801","4802","4803","4808","4809","4841")) & !(ICD9%in%c("5171","1304")) ~ "Bacterial pneumonia",
		CCS==197 & !(ICD9%in%c("68601","6861")) ~ "SSTIs",
		CCS==159 & !(ICD9%in%c("5952","59581","59582")) ~ "UTIs",
		CCS==135 ~ "GI infections"
		))

ccs10 <- ccs10 %>% 
	mutate(COND=NA_character_) %>% 
	mutate(COND=case_when(
		CCS=="RSP001" & grepl("^J01",ICD10) ~ "Acute sinusitis",
		CCS=="RSP006" & ICD10=="J00" ~ "Other pharyngitis",
		CCS=="RSP006" & ICD10=="J028" ~ "Other pharyngitis",
		CCS=="RSP006" & ICD10=="J029" ~ "Other pharyngitis",
		CCS=="RSP006" & ICD10=="J060" ~ "Other pharyngitis",
		CCS=="RSP006" & ICD10=="J020" ~ "Strep pharyngitis",
		CCS=="RSP003" & !(ICD10%in%c("J09X1","J1000","J1001","J1008","J1108","J1100")) ~ "Influenza",
		CCS=="EAR001" & grepl("^H660",ICD10) ~ "Otitis media",
		CCS=="EAR001" & grepl("^H664",ICD10) ~ "Otitis media",
		CCS=="EAR001" & grepl("^H669",ICD10) ~ "Otitis media",
		CCS=="RSP002" & grepl("^B3",ICD10) ~ "Fungal pneumonia",
		CCS=="RSP002" & ICD10=="B59" ~ "Fungal pneumonia",
		CCS=="RSP002" & ICD10%in%c("B012","B052","J120","J121","J122","J123","J1281","J1289","B0681","B250","J129") ~ "Viral pneumonia",
		CCS=="RSP003" & ICD10%in%c("J09X1","J1000","J1001","J1008","J1108","J1100") ~ "Viral pneumonia",
		CCS=="RSP002" & !(grepl("^B3",ICD10)) & !(ICD10=="B59") & !(ICD10%in%c("B012","B052","J120","J121","J122","J123","J1281","J1289","B0681","B250","J129")) & !(ICD10%in%c("B583","B7781")) ~ "Bacterial pneumonia", 
		CCS=="SKN001" ~ "SSTIs",
		CCS=="GEN004" & !(ICD10%in%c("N3010","N3011","N3040","N3041")) ~ "UTIs",
		CCS=="DIG001" ~ "GI infections"
		))



ccsmap9 <- ccs9 %>% 
	filter(!is.na(COND)) %>% 
	select(COND, DX=ICD9) %>%
	mutate(ICD="9") %>% 
	mutate(PRIORITY=1) %>% 
	select(COND, DX, ICD, PRIORITY) 

ccsmap10 <- ccs10 %>% 
	filter(!is.na(COND)) %>% 
	select(COND, DX=ICD10) %>%
	mutate(ICD="0") %>% 
	mutate(PRIORITY=1) %>% 
	select(COND, DX, ICD, PRIORITY) 


ccsmap <- bind_rows(ccsmap9, ccsmap10) 

write_csv(ccsmap, file="data/ccs_map_full.csv")


