library(here)
library(dplyr)
#Load in data
brfss_data <- read.csv(here('data','2015.csv'))
nehrs_data <- read.csv(here('data', 'NEHRS_2008-2017.csv'))
fips_data <- read.csv(here('data', 'us-state-ansi-fips.csv'),
                      stringsAsFactors = F)

#limiting data from NEHRS to 2015, and adding fips code for merge
nehrs_data <- nehrs_data %>% filter(period == 2015,) %>% 
  left_join(fips_data, by = c('region', 'region_code'))
rm(fips_data)

#excluding Guam, Puerto Rico from BRFSS data since 
#is not present in NEHRS data 
brfss_data <- brfss_data %>% rename(fips= X_STATE) %>%
  filter(fips != 66 & fips != 72)

#when we are ready to merge both the nehrs and the BRFSS 
full_data <- left_join(brfss_data, nehrs_data, by = 'fips')
rm(brfss_data)
rm(nehrs_data)
