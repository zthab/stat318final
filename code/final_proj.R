library(here)
library(tidyverse)
library(dplyr)
library(mice)
#Load in data
brfss_data <- read.csv(here::here('data','2015.csv') )
nehrs_data <- read.csv(here::here('data', 'NEHRS_2008-2017.csv'))
fips_data  <- read.csv(here::here('data', 'us-state-ansi-fips.csv'),
                      stringsAsFactors = F)


##############################
#Data cleaning
##############################

#limiting data from NEHRS to 2015, and adding fips code to merge it later
nehrs_data <- nehrs_data %>% filter(period == 2015 & region != 'National',) %>% 
  left_join(fips_data, by = c('region', 'region_code'))
#removing variables that weren't recorded in 2015 (and hence are NA)
nehrs_data <- nehrs_data[,colSums(is.na(nehrs_data))<nrow(nehrs_data)]
#removing variables from NEHRS that we wont use in regression
nehrs_data <- dplyr::select(nehrs_data, -region, -region_code, -period)
#removing fips dataset 
rm(fips_data)

#excluding Guam, Puerto Rico, and blank statses 
#from BRFSS data since they aren't in NEHRS data 
brfss_data <- brfss_data %>% rename(fips = X_STATE) %>% filter((fips != 66 & 
                                                        fips != 72)|is.na(fips)) 
brfss_data$fips <- as.factor(brfss_data$fips)
#excluding survey observations not made in 2015
brfss_data <- brfss_data %>% mutate(IYEAR = str_extract(as.character(.$IYEAR), 
                                                        '[0-9]{4}')) %>% 
              filter(IYEAR == '2015') 
#excluding observations for which survey was terminated early
brfss_data <- filter(brfss_data, !((!is.na(CELLFON3)& CELLFON3 == 2)| 
                                     (!is.na(CELLFON2) & CELLFON2 == 2)))
#excluding observations that do not reside in state
brfss_data <- filter(brfss_data, is.na(CSTATE) | CSTATE !=2)
#excluding observations that do not have _RFHLTH (our response variable)
brfss_data <- filter(brfss_data, X_RFHLTH != 9)
brfss_data_comp_1$X_RFHLTH[brfss_data_comp_1$X_RFHLTH == 2] <- 0
brfss_data$X_RFHLTH <- as.factor(brfss_data$X_RFHLTH)

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
brfss_data$CHECKUP1CLEAN <- as.factor(brfss_data$CHECKUP1CLEAN)

#codes men, people older than 44 as non pregnant
brfss_data$PREGNANT[brfss_data$AGE>44|brfss_data$SEX ==1] <- 2
#codes those who dont work as having worked 0 hours
brfss_data$SCNTWRK1[brfss_data$EMPLOY1 %in% 3:8]<- 98 
# combines cell and landline versions of same survey question (NUMADULT)
brfss_data$NUMADULT[brfss_data$HHADULT < 76 & !is.na(brfss_data$HHADULT)] <- 
  brfss_data$HHADULT[brfss_data$HHADULT < 76 & !is.na(brfss_data$HHADULT)]
# combines cell and landline (PVTRESD1)
brfss_data$PVTRESD1[!is.na(brfss_data$PVTRESD2)] <- 
  brfss_data$PVTRESD2[!is.na(brfss_data$PVTRESD2)]
brfss_data$PVTRESD1 <- as.factor(brfss_data$PVTRESD1)

# sets 'missing' observation to NA for predictor variable imputaiton purporses
# and factorizes categorical predictor variables.
brfss_data$MARITAL[brfss_data$MARITAL == 9] <- NA
brfss_data$MARITAL <- as.factor(brfss_data$MARITAL)

brfss_data$EDUCA[brfss_data$EDUCA == 9] <- NA
brfss_data$EDUCA <- as.factor(brfss_data$EDUCA)

brfss_data$RENTHOM1[brfss_data$RENTHOM1 == 7 |brfss_data$RENTHOM1== 9] <- NA
brfss_data$RENTHOM1 <- as.factor(brfss_data$RENTHOM1)

brfss_data$VETERAN3[brfss_data$VETERAN3 == 7 |brfss_data$VETERAN3== 9] <- NA
brfss_data$VETERAN3 <- as.factor(brfss_data$VETERAN3)

brfss_data$EMPLOY1[brfss_data$EMPLOY1 == 9] <- NA
brfss_data$EMPLOY1 <- as.factor(brfss_data$EMPLOY1)

brfss_data$CHILDREN[brfss_data$CHILDREN == 99] <- NA
#sets having 0 children to 0 
brfss_data$CHILDREN[brfss_data$CHILDREN == 88] <- 0

brfss_data$INCOME2[brfss_data$INCOME2 == 77 | 
                     brfss_data$INCOME2 == 99] <- NA
brfss_data$INCOME2 <- as.factor(brfss_data$INCOME2)

brfss_data$INTERNET[brfss_data$INTERNET == 7|
                      brfss_data$INTERNET == 9] <- NA
brfss_data$INTERNET <- as.factor(brfss_data$INTERNET)

brfss_data$PREGNANT[brfss_data$PREGNANT == 7 |brfss_data$PREGNANT== 9] <- NA
brfss_data$PREGNANT <- as.factor(brfss_data$PREGNANT)

brfss_data$SCNTWRK1[brfss_data$SCNTWRK1 == 98] <- 0
brfss_data$SCNTWRK1[brfss_data$SCNTWRK1 == 97 |
                      brfss_data$SCNTWRK1 == 99  ] <- NA

brfss_data$X_PRACE1[brfss_data$X_PRACE1==77 | 
                      brfss_data$X_PRACE1 == 99] <- NA
#removing category of race for which there are 7 responses 
#(multiracial but preferred race not answered)
brfss_data <- filter(brfss_data,X_PRACE1 != 8|is.na(X_PRACE1))
brfss_data$X_PRACE1 <- as.factor(brfss_data$X_PRACE1)

brfss_data$X_HISPANC[brfss_data$X_HISPANC == 9] <- NA
brfss_data$X_HISPANC <- as.factor(brfss_data$X_HISPANC)

brfss_data$EXERANY2[brfss_data$EXERANY2 == 7|
                      brfss_data$EXERANY2 == 9] <- NA
brfss_data$EXERANY2 <- as.factor(brfss_data$EXERANY2)

brfss_data$X_SMOKER3[brfss_data$X_SMOKER3 == 9] <- NA
brfss_data$X_SMOKER3 <- as.factor(brfss_data$X_SMOKER3)

brfss_data$SEX <- as.factor(brfss_data$SEX)

brfss_data$HLTHPLN1[brfss_data$HLTHPLN1 == 7 |
                      brfss_data$HLTHPLN1 == 9 ] <- NA
brfss_data$HLTHPLN1 <- as.factor(brfss_data$HLTHPLN1)

brfss_data$SXORIENT[brfss_data$SXORIENT == 9] <- NA
brfss_data$SXORIENT <- as.factor(brfss_data$SXORIENT)

brfss_data$TRNSGNDR[brfss_data$TRNSGNDR == 9] <- NA
brfss_data$TRNSGNDR <- as.factor(brfss_data$TRNSGNDR)

brfss_data$MSCODE <- as.factor(brfss_data$MSCODE)
##############################
#removal of variables
##############################

# cuts down variables (saves as new var called brfss_data_f)
brfss_data_f = subset(brfss_data, select = c('X_RFHLTH', 'fips', 
                                             'CHECKUP1CLEAN', 'NUMADULT', 
                                             'PVTRESD1', 'SEX', 'MARITAL', 
                                             'EDUCA', 'RENTHOM1', 'VETERAN3',
                                             'EMPLOY1','CHILDREN',  'INCOME2',
                                             'X_BMI5', 'PREGNANT', 'SCNTWRK1' ,
                                             'SCNTLWK1', 'SXORIENT', 'TRNSGNDR',
                                             'MSCODE','X_PRACE1', 'X_HISPANC',
                                             'HLTHPLN1','INTERNET','EXERANY2',
                                             'X_SMOKER3'))


##############################
#Checking missingness with proportion tables
##############################
#creates a list of proportion tables for each of the predictor variables 
prop_tables <- list()
j <- 0 
for (i in brfss_data_f){
  j= j+1
  temp_prop <- table(i, useNA = "always")
  prop_tables[[j]] <- prop.table(temp_prop)
}
names(prop_tables) <- names(brfss_data_f)
rm(temp_prop)

##############################
#Removing variables with many missing values
##############################

brfss_data_f = subset(brfss_data_f, select = c('X_RFHLTH', 'fips', 
                                               'CHECKUP1CLEAN', 'NUMADULT', 
                                               'PVTRESD1', 'SEX', 'MARITAL', 
                                               'EDUCA', 'RENTHOM1', 'VETERAN3', 
                                               'EMPLOY1', 'CHILDREN', 'INCOME2',
                                               'X_BMI5', 'PREGNANT', 'SCNTWRK1',
                                               'X_PRACE1', 'X_HISPANC', 
                                               'HLTHPLN1', 'INTERNET', 
                                               'EXERANY2','X_SMOKER3'))
######################
# Imputations
# We used mice (Multivariate Imputation by Chained Equations) to compute
# imputations for all of our variables at the same time. We chose this because 
# we knew we needed some recursive method to deal with the problem of imputing
# multiple missing values for ovservations
# to impute real values variables, we used norm.nob (stochastic regression 
# imputation) to more accurately, capture the variance of the predictors present
# for binary variabels, we used logistic regressions imputation with a 50% cutoff
# for categorical variables with no natural ordering, we used multinomal 
# logistic regression to impute. 
# for categorical variables with natural ordering, we used proportional odds 
# logistic regression to impute.
# We bounded possibilities for predictors when using stochastic regression by 
# using the existing bounds of that predictor.
#####################
meth_list <- c('', '', '', 'norm.nob', '', '', 'polyreg', 'polr', 'polyreg', 
               'logreg', 'polyreg', 'norm.nob', 'polr', 'norm.nob', 'logreg',
               'norm.nob', 'polyreg', 'logreg', 'logreg', 'logreg', 'logreg', 
               'polyreg') 

post <- make.post(brfss_data_f)
#creating ranges for outcomes of imputations when using stochastic regression
range(brfss_data_f$NUMADULT, na.rm=TRUE)
range(brfss_data_f$CHILDREN, na.rm=TRUE)
range(brfss_data_f$X_BMI5, na.rm=TRUE)
range(brfss_data_f$SCNTWRK1, na.rm=TRUE)
post["NUMADULT"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(1, 60))"
post["CHILDREN"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 22))"
post["X_BMI5"]   <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(1215, 9765))"
post["SCNTWRK1"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 96))"

imp1 <- mice(brfss_data_f, method = meth_list, m=1, seed = 85444, post =post)
#plugging in imputed values into brfss dataset 
brfss_data_comp_1 <- complete(imp1)
#saving imputation file, brfss with imputation file
save(brfss_data_comp_1, file = 'brfss_comp_1.rda')
save(imp1, file = 'imp1.rda')

nehrs_data$fips <- as.factor(nehrs_data$fips)
#merged brfss with imputed values and nehrs data 
full_data <- left_join(brfss_data_comp_1, nehrs_data, by = 'fips')
save(full_data, file='full_comp_1.rda')

rm(brfss_data)
rm(nehrs_data)
