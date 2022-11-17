__Jump to:__ 

- [16 Nov 2022](#16-Nov-2022)
- [17 Nov 2022](#17-Nov-2022)

# 16 Nov 2022 

Beginning with a pull from the server. We want counts of strep pharyngitis, and counts of total individuals, by the following attributes: 

- Year 
- Month 
- Age decile 
- Sex 
- State 

Do this for all US states between 2010 and 2018. I think we should also restrict for people who are present in the dataset for the entire year, so that each year has its own (consistent) denominator. 

---

I've ported over and edited the code from AbxGeography to pull just visits and cohort sizes by the categories listed above. Now going to do some tests on the server to make sure it's behaving right. 

# 17 Nov 2022 

Got the full pull finished, I think. It took about six hours, which surprised me, because I think the pull I did for the geographic antibiotic prescribing project only took about four... but I'll take it. 

I also wrote an R script to reduce the data from SAS into more user-friendly files, one for cohort population sizes and one for strep pharyngitis visit counts. 

I haven't done any qualtiy control on these - no plotting or anything - but so far things seem to be working as I hoped. 

