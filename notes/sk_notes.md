__Jump to:__ 

- [16 Nov 2022](#16-Nov-2022)

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

