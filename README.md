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

The extraction scripts are saved as `code/extract_visits.sas` and `code/reduce_visits.R`. These scripts should be run (in this order) to get the datasets saved in `output/`. Note that the initial SAS extraction takes about six hours to run! 

__Note:__ These data have not yet been validated; no checks for consistency or plausibility have yet been done. Proceed with caution!

__Other data sources:__

Data census population data was derived from the American Community Survey (ACS) from 2011-2015 and obtained through the tidycensus R package (source 15). Data on US population centroids by state according to the 2010 U.S. census was obtained through the USpopcenters R package (source 16). Data on school start times in 2019 were obtained from Pew Research Foundation (source 17). A summarized version of these data can be found in `output/school_start_summary.csv'.

 
