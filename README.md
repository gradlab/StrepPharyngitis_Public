# Geographic and temporal trends in Strep Pharyngitis in the United States

__Data:__ 

Data on strep pharyngitis cases in the United States are extracted from the MarketScan database. These are saved in `output/GeoVisits.csv`. They are measured for the following categories: 

- State
- Sex 
- Age group (decile, except 0-4 and 5-9 are separated out) 
- Month 
- Year 

These counts pertain to individuals in the dataset who are followed continuously for an entire year. (Some individuals in the dataset drop in and out within a given year; these are excluded). So, the population denominators are constant across each calendar year. These counts are included in `output/GeoCohort.csv`. Cohort sizes are reported by: 

- State
- Sex 
- Age group (decile, except 0-4 and 5-9 are separated out) 
- Year 

To estimate monthly strep pharyngitis case rates by population group, divide __NVISITS__ in `GeoVisits.R` by __NMEMB__ in `GeoCohort.R`, matching by the appropriate year. 

__Note:__ These data have not yet been validated; no checks for consistency or plausibility have yet been done. Proceed with caution!