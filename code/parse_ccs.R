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


ccs9 %>%
	group_by(`CCS CATEGORY DESCRIPTION`) %>% 
	summarise() %>% 
	print(n=Inf)


ccs9 %>% 
	filter(`CCS CATEGORY`==92) %>% 
	select(`ICD-9-CM CODE`,`ICD-9-CM CODE DESCRIPTION`) %>% 
	print(n=Inf)


ccs9: 
122: Pneumonia
126: Other upper respiratory infections
92: Otitis media and related conditions
197: Skin and subcutaneous tissue infections
159: urinary tract infections
135: Intestinal infections

Sinusitis
	126 | 461* (Acute sinusitis)
Pharyngitis
	126 | 460 (Acute nasopharyngitis)
	126 | 462 (Acute pharyngitis)
	126 | 4650 (Acute laryngopharyngitis)
Strep Pharyngitis
	126 | 340 (Strep sore throat)
Influenza
	All 123 except 4870 (Influenza with pneumonia)
Otitis media
	92 | 382* (Suppurative and unspecified otitis media)
Bacterial Pneumonia:
	All 122 not in Viral Pneumonia
Viral Pneumonia: 	
	122 | 521  VARICELLA PNEUMONITIS
	122 | 551  POSTMEASLES PNEUMONIA
	122	| 4800 ADENOVIRAL PNEUMONIA
	122	| 4801 RESP SYNCYT VIRAL PNEUM
	122	| 4802 PARINFLUENZA VIRAL PNEUM
	122	| 4803 PNEUMONIA DUE TO SARS-ASSOCIATED CORONAVIRUS (Begin 2003)
	122	| 4808 VIRAL PNEUMONIA NEC
	122	| 4809 VIRAL PNEUMONIA NOS
	123 | 4870 INFLUENZA WITH PNEUMONIA
SSTIs:
	All 197
UTIs
	All 159
GI infections
	All 135



ccs10: 


ccs10 %>% 
	group_by(`Default CCSR CATEGORY IP`) %>% 
	slice(1) %>% 
	select(CCS=`Default CCSR CATEGORY IP`, CCSDESC=`Default CCSR CATEGORY DESCRIPTION IP`) %>% 
	print(n=Inf)

ccs10 %>% 
	filter(`Default CCSR CATEGORY IP`=="RSP003") %>% 
	select(ICD=`ICD-10-CM CODE`,ICDDESC=`ICD-10-CM CODE DESCRIPTION`) %>% 
	print(n=Inf)

Sinusitis
	RSP001 | J01* (Acute sinusitis)
Pharyngitis
	RSP006 | J00 (Acute nasopharyngitis)
	RSP006 | J028 (Acute pharyngitis due to other specified organisms)
	RSP006 | J029 (Acute pharyngitis unspecified)
	RSP006 | J060 (Acute laryngopharyngitis)
Strep Pharyngitis
	RSP006 | J020 (Streptococcal pharyngitis)
Influenza
	RSP003 Except [J09X1 J1000 J1001 J1008 J1108]
Otitis media
	EAR001 | H660* (Acute suppurative otitis media)
Bacterial Pneumonia:
	All RSP002 not in Viral Pneumonia
Viral Pneumonia: 	
	RSP002 | B012 (Varicella pneumonia)
	RSP002 | B052 (Measles complicated by pneumonia)
	RSP002 | J120 (Adenoviral pneumonia)
	RSP002 | J121 (RSV pneumonia)
	RSP002 | J122 (Parainfluenza virus pneumonia)
	RSP002 | J123 (Human metapneumovirus pneumonia)
	RSP002 | J1281 (Pneumonia due to SARS-associated coronavirus)
	RSP002 | J1289 (Other viral pneumonia)
	RSP002 | J129 (Viral pneumonia unspecified)
	RSP003 | J09X1 (Influenza A with pneumonia)
	RSP003 | J1000 (Other influenza with pneumonia)
	RSP003 | J1001 (Other influenza with pneumonia)
	RSP003 | J1008 (Other influenza with pneumonia)
	RSP003 | J1108 (Other influenza with pneumonia)
SSTIs:
	All SKN001
UTIs
	All GEN004
GI infections
	All DIG001

