library(here)
library(tidyverse)
library(dplyr)
#Load in data
brfss_data <- read.csv(here('data','2015.csv') )
nehrs_data <- read.csv(here('data', 'NEHRS_2008-2017.csv'))
fips_data <- read.csv(here('data', 'us-state-ansi-fips.csv'),
                      stringsAsFactors = F)

#limiting data from NEHRS to 2015, and adding fips code for merge
nehrs_data <- nehrs_data %>% filter(period == 2015,) %>% 
  left_join(fips_data, by = c('region', 'region_code'))
rm(fips_data)

#excluding Guam, Puerto Rico from BRFSS data since 
#is not present in NEHRS data 
brfss_data <- brfss_data %>% rename(fips= X_STATE) %>% filter((fips != 66 & fips != 72)) 
#excluding survey observations made in 2015
brfss_data <- brfss_data %>% mutate(IYEAR = str_extract(as.character(.$IYEAR), '[0-9]{4}')) %>% filter(IYEAR == '2015') 
#excluding observations that were terminated early
#because called condcuting landline survey and cell phone answere
#or conducting cellphone survey and landline answered
brfss_data <- filter(brfss_data, !((!is.na(CELLFON3)& CELLFON3 == 2)| (!is.na(CELLFON2) & CELLFON2 == 2)))
#excluding observations that do not reside in the sate where they are answer they are.
brfss_data <- filter(brfss_data, is.na(CSTATE) | CSTATE !=2)

#creates new predictor variable for checkup1
brfss_data$CHECKUP1CLEAN <- rep(NA, nrow(brfss_data))
brfss_data$CHECKUP1CLEAN[brfss_data$X_AGEG5YR <= 4 & brfss_data$CHECKUP1 <= 2 ] <- 1
brfss_data$CHECKUP1CLEAN[brfss_data$X_AGEG5YR > 4 & brfss_data$X_AGEG5YR < 14 & brfss_data$CHECKUP1 <= 1 ] <- 1
brfss_data$CHECKUP1CLEAN[brfss_data$X_AGEG5YR <= 4 & brfss_data$CHECKUP1 > 2 &  brfss_data$CHECKUP1 < 9] <- 0
brfss_data$CHECKUP1CLEAN[brfss_data$X_AGEG5YR > 4 & brfss_data$X_AGEG5YR < 14 & brfss_data$CHECKUP1 > 1 &  brfss_data$CHECKUP1 < 9] <- 0
brfss_data$CHECKUP1CLEAN[brfss_data$CHECKUP1 == 9] <- 9 #people who refused to provide check up information
sum(is.na(brfss_data$CHECKUP1CLEAN)) #32476 missing/refused to provide age

# cut down variables (saves as new var called brfss_data_f)
brfss_data_f = subset(brfss_data, select = c('X_RFHLTH', 'CHECKUP1CLEAN', 'NUMADULT', 'NUMMEN', 'NUMWOMEN', 'PVTRESD2', 'CCLGHOUS', 'HHADULT', 'SEX', 'MARITAL', 'EDUCA', 'RENTHOM1', 'VETERAN3', 'CHILDREN', 'INCOME2', 'WEIGHT2', 'PREGNANT', 'SCNTWRK1' ,'SCNTLWK1', 'SXORIENT', 'TRNSGNDR', 'MSCODE'))

#creates a list of proportion tables for each of the 
#predictor variabels 
prop_var_list <- c('NUMADULT', 'NUMMEN', 'NUMWOMEN', 'PVTRESD2', 'CCLGHOUS', 'HHADULT', 'SEX', 'MARITAL', 'EDUCA', 'RENTHOM1', 'VETERAN3', 'CHILDREN', 'INCOME2', 'WEIGHT2', 'PREGNANT', 'SCNTWRK1' ,'SCNTLWK1', 'SXORIENT', 'TRNSGNDR', 'MSCODE')
prop_table <- brfss_data %>% select(prop_var_list)
list_of_prop_tables <- list()
#i know theres a better way to do it than this but this felt the easiest 
j <- 0 
for (i in prop_table){
  j= j+1
  test <- table(i)
  list_of_prop_tables[[j]] <- prop.table(test)
}
names(list_of_prop_tables) <- names(prop_table)
rm(prop_table)
#when we are ready to merge both the nehrs and the BRFSS 
full_data <- left_join(brfss_data, nehrs_data, by = 'fips')

rm(brfss_data)
rm(nehrs_data)
