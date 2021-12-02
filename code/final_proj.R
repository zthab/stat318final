library(here)
library(tidyverse)
library(dplyr)
library(mice)
#Load in data
brfss_data <- read.csv(here('data','2015.csv') )
nehrs_data <- read.csv(here('data', 'NEHRS_2008-2017.csv'))
fips_data <- read.csv(here('data', 'us-state-ansi-fips.csv'),
                      stringsAsFactors = F)


##############################
#Data cleaning
##############################

#limiting data from NEHRS to 2015, and adding fips code for merge
nehrs_data <- nehrs_data %>% filter(period == 2015 & region != 'National',) %>% 
  left_join(fips_data, by = c('region', 'region_code'))
nehrs_data <- nehrs_data[,colSums(is.na(nehrs_data))<nrow(nehrs_data)]
rm(fips_data)

#excluding Guam, Puerto Rico from BRFSS data since 
#is not present in NEHRS data 
brfss_data <- brfss_data %>% rename(fips = X_STATE) %>% filter((fips != 66 & 
                                                                 fips != 72)) 
#excluding survey observations made in 2015
brfss_data <- brfss_data %>% mutate(IYEAR = str_extract(as.character(.$IYEAR), 
                                                        '[0-9]{4}')) %>% 
              filter(IYEAR == '2015') 
#excluding observations that were terminated early
#because called condcuting landline survey and cell phone answere
#or conducting cellphone survey and landline answered
brfss_data <- filter(brfss_data, !((!is.na(CELLFON3)& CELLFON3 == 2)| 
                                     (!is.na(CELLFON2) & CELLFON2 == 2)))
#excluding observations that do not reside in the sate where they are answer they are.
brfss_data <- filter(brfss_data, is.na(CSTATE) | CSTATE !=2)

#excluding observations that do not have _RFHLTH
brfss_data <- filter(brfss_data, X_RFHLTH != 9)

#creates new predictor variable for checkup1
brfss_data$CHECKUP1CLEAN <- rep(NA, nrow(brfss_data))
brfss_data$CHECKUP1CLEAN[brfss_data$X_AGEG5YR <= 4 & brfss_data$CHECKUP1 <= 2 ] <- 1
brfss_data$CHECKUP1CLEAN[brfss_data$X_AGEG5YR > 4 & brfss_data$X_AGEG5YR < 14 &
                           brfss_data$CHECKUP1 <= 1 ] <- 1
brfss_data$CHECKUP1CLEAN[brfss_data$X_AGEG5YR <= 4 & brfss_data$CHECKUP1 > 2 &  
                           brfss_data$CHECKUP1 < 9] <- 0
brfss_data$CHECKUP1CLEAN[brfss_data$X_AGEG5YR > 4 & brfss_data$X_AGEG5YR < 14 & 
                           brfss_data$CHECKUP1 > 1 &  brfss_data$CHECKUP1 < 9] <- 0
brfss_data$CHECKUP1CLEAN[brfss_data$CHECKUP1 == 9] <- 9
brfss_data$CHECKUP1CLEAN[is.na(brfss_data$CHECKUP1CLEAN)] <- 9

#excluding observations that do not have CHECKUP1CLEAN
brfss_data <- filter(brfss_data,  CHECKUP1CLEAN != 9)

#creates landline variable
brfss_data$LANDLINE<-0
brfss_data$LANDLINE[brfss_data$QSTVER<=13] <- 1
#codes men, people older than 44 as non pregnant
brfss_data$PREGNANT[brfss_data$AGE>44|brfss_data$SEX ==1] <- 2
#codes those who dont work as having worked 0 hours
brfss_data$SCNTWRK1[brfss_data$EMPLOY1 %in% 3:8]<- 98 
#people who refused to provide check up information
sum(is.na(brfss_data$CHECKUP1CLEAN)) #32476 missing/refused to provide age
brfss_data$LANDLINE[brfss_data$QSTVER<=13] <- 1#people who refused to provide check up information
sum(is.na(brfss_data$CHECKUP1CLEAN)) #3476 missing/refused to provide age


# combines cell and landline (NUMADULT)
brfss_data$NUMADULT[brfss_data$HHADULT < 76 & !is.na(brfss_data$HHADULT)] <- 
  brfss_data$HHADULT[brfss_data$HHADULT < 76 & !is.na(brfss_data$HHADULT)]

# combines cell and landline (PVTRESD1)
brfss_data$PVTRESD1[!is.na(brfss_data$PVTRESD2)] <- brfss_data$PVTRESD2[!is.na(brfss_data$PVTRESD2)]

brfss_data$LANDLINE<-0
brfss_data$LANDLINE[brfss_data$QSTVER<=13] <- 1


brfss_data$EMPLOY1[brfss_data$EMPLOY1 == 9] <- NA
brfss_data$EMPLOY1 <- as.factor(brfss_data$EMPLOY1)

brfss_data$CHILDREN[brfss_data$CHILDREN == 99] <- NA
brfss_data$CHILDREN[brfss_data$CHILDREN == 88] <- 0

brfss_data$INCOME2[brfss_data$INCOME2 == 77 | 
                     brfss_data$INCOME2 == 99] <- NA
brfss_data$INCOME2 <- as.Factor(brfss_data$INCOME2)

brfss_data$WEIGHT2[brfss_data$WEIGHT2== 9999] <- NA
#converts kg reported to pounds
brfss_data$WEIGHT2[brfss_data$WEIGHT2>=9000] <- (brfss_data$WEIGHT2-9000)*2.205



##############################
#removal of variables
##############################

# cuts down variables (saves as new var called brfss_data_f)
brfss_data_f = subset(brfss_data, select = c('X_RFHLTH', 'fips', 'CHECKUP1CLEAN', 'NUMADULT', 'PVTRESD1', 'SEX', 'MARITAL', 'EDUCA', 'RENTHOM1', 'VETERAN3','EMPLOY1','CHILDREN',  'INCOME2', 'X_BMI5', 'PREGNANT', 'SCNTWRK1' ,'SCNTLWK1', 'SXORIENT', 'TRNSGNDR', 'MSCODE','X_PRACE1', 'X_HISPANC','HLTHPLN1','INTERNET','EXERANY2','X_SMOKER3', 'LANDLINE'))




##############################
#Checking missingness with proportion tables
##############################


#creates a list of proportion tables for each of the 
#predictor variabels 
prop_tables <- list()
#i know theres a better way to do it than this but this felt the easiest 
j <- 0 
for (i in brfss_data_f){
  j= j+1
  temp_prop <- table(i, useNA = "always")
  prop_tables[[j]] <- prop.table(temp_prop)
}
names(prop_tables) <- names(brfss_data_f)
rm(temp_prop)

#crosstabs by landline/cellphone
cross_tables <- list()
#i know theres a better way to do it than this but this felt the easiest 
j <- 0 
for (i in brfss_data_f){
  j= j+1
  temp_prop <- table(i,brfss_data_f$LANDLINE, useNA = "always")
  cross_tables[[j]] <- prop.table(temp_prop)
}
names(cross_tables) <- names(brfss_data_f)
rm(temp_prop)



##############################
#Removing variables with many missing values
##############################


brfss_data = subset(brfss_data, select = c('X_RFHLTH', 'fips', 'CHECKUP1CLEAN', 'NUMADULT', 'PVTRESD1', 'SEX', 'MARITAL', 'EDUCA', 'RENTHOM1', 'VETERAN3', 'EMPLOY1', 'CHILDREN', 'INCOME2', 'WEIGHT', 'PREGNANT', 'SCNTWRK1' , 'X_PRACE1', 'X_HISPANC','HLTHPLN1','INTERNET','EXERANY2','X_SMOKER3', 'LANDLINE'))




##############################
#Imputation data 
##############################

### (Cindy start list here)
meth_list <- c('')

imp <- mice(brfss_data, method = meth_list)


brfss_data$PREGNANT[brfss_data$PREGNANT]
#when we are ready to merge both the nehrs and the BRFSS 
full_data <- left_join(brfss_data_f, nehrs_data, by = 'fips')

rm(brfss_data)
rm(nehrs_data)
