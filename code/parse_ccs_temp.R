gsubv <- Vectorize(gsub)

ccs9temp <- read_tsv("data/dxref2015tsv.txt", 
	# quote="\'", 
	col_types=list(
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character(),
		col_character()
		)) 
names(ccs9temp) <- c("ICD9","CCS","CCSDESC","ICD9DESC","CCSOPT","CCSOPTDESC")
ccs9temp <- ccs9temp %>% 
	# Note both CCS and ICD all have 7 characters, of which the first and the last are apostrophes: 
	mutate(CCS=substr(CCS,2,6)) %>% 
	mutate(CCS=gsubv(" ","",CCS)) %>% 
	mutate(CCS=as.numeric(CCS)) %>% 
	mutate(ICD9=substr(ICD9,2,6)) %>% 
	mutate(ICD9=gsubv(" ","",ICD9))

# filter(ccs9temp, grepl("^382",ICD9))
# filter(ccs9temp, CCS == "135") %>% print(n=Inf)


GeoVisits <- read_csv("output/GeoVisits.csv")

GeoVisits %>% 
	filter(COND=="Strep pharyngitis") %>% 
	group_by(MONTH, YEAR, AGEGRP) %>% 
	summarise(NVISITS=sum(NVISITS))