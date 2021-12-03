library(car)
library(dplyr)
load("full_comp_1.rda")
colnames(full_data)
set.seed(123)

# creates training and testing data (divided into 2/3 and 1/3)
indicesTrainingSet<-sample(nrow(full_data), length(full_data$X_RFHLTH)*2/3, replace=FALSE)
train<-full_data[indicesTrainingSet,]
test<-full_data[-indicesTrainingSet,]


##################
# VIF Analysis
##################

# Included any variable with more than 5 levels
model.logit <- glm(X_RFHLTH~NUMADULT + MARITAL + 
                     EDUCA + EMPLOY1 + CHILDREN + INCOME2 + 
                     X_BMI5 + SCNTWRK1 + X_PRACE1 +  pct_phys_any_ehr + 
                     pct_phys_basic_ehr + pct_phys_cert_ehr + pct_primary_care_phys_cert_ehr +
                     pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr + 
                     pct_phys_send_receive_any_patient_info + pct_phys_e_share_patients +
                     pct_phys_patient_secure_message + pct_phys_vdt +pct_phys_vd_and_t + 
                     pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                     pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                     pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                     pct_phys_integrate_summary_care_record,
                   family=binomial(link="logit"), data = train)
vif(model.logit) # pct_phys_cert_ehr highest
model.logit <- glm(X_RFHLTH~NUMADULT + MARITAL + 
                     EDUCA + EMPLOY1 + CHILDREN + INCOME2 + 
                     X_BMI5 + SCNTWRK1 + X_PRACE1 +  pct_phys_any_ehr +
                     pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                     pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr + 
                     pct_phys_send_receive_any_patient_info + pct_phys_e_share_patients + 
                     pct_phys_patient_secure_message + pct_phys_vdt +pct_phys_vd_and_t +
                     pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                     pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                     pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                     pct_phys_integrate_summary_care_record,
                   family=binomial(link="logit"), data = train)
vif(model.logit) # ct_phys_send_receive_any_patient_info highest
model.logit <- glm(X_RFHLTH~NUMADULT + MARITAL + 
                     EDUCA + EMPLOY1 + CHILDREN + INCOME2 + 
                     X_BMI5 + SCNTWRK1 + X_PRACE1 + pct_phys_any_ehr + 
                     pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                     pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr + 
                     pct_phys_e_share_patients + pct_phys_patient_secure_message + 
                     pct_phys_vdt +pct_phys_vd_and_t + pct_phys_find_clin_info + 
                     pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                     pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                     pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record,
                   family=binomial(link="logit"), data = train)
vif(model.logit) # EMPLOY1 highest
model.logit <- glm(X_RFHLTH~NUMADULT + MARITAL + 
                     EDUCA + CHILDREN + INCOME2 + X_BMI5 + 
                     SCNTWRK1 + X_PRACE1 +  pct_phys_any_ehr + 
                     pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                     pct_surg_med_spec_phys_cert_ehr + pct_small_practice_phys_cert_ehr + 
                     pct_phys_e_share_patients + pct_phys_patient_secure_message + 
                     pct_phys_vdt +pct_phys_vd_and_t + pct_phys_find_clin_info + 
                     pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                     pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                     pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record,
                   family=binomial(link="logit"), data = train)
vif(model.logit) # pct_phys_e_share_patients highest
model.logit <- glm(X_RFHLTH~NUMADULT + MARITAL + 
                     EDUCA + CHILDREN + INCOME2 + X_BMI5 + 
                     SCNTWRK1 + X_PRACE1 +  pct_phys_any_ehr + 
                     pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                     pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr + 
                     pct_phys_patient_secure_message + pct_phys_vdt +pct_phys_vd_and_t + 
                     pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                     pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                     pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                     pct_phys_integrate_summary_care_record,
                   family=binomial(link="logit"), data = train)
vif(model.logit) # none other are higher than 8 (next highest is pct_phys_receive_summary_care_record at 7.7)

# final full models
final_hlth <- glm(X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + 
                    MARITAL + EDUCA + RENTHOM1 + VETERAN3 + 
                    CHILDREN + INCOME2 + X_BMI5 + PREGNANT + 
                    SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + 
                    INTERNET + EXERANY2 + X_SMOKER3 + pct_phys_any_ehr + 
                    pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                    pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr + 
                    pct_phys_patient_secure_message + pct_phys_vdt +pct_phys_vd_and_t + 
                    pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                    pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                    pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                    pct_phys_integrate_summary_care_record,
                  family=binomial(link="logit"), data = train)

final_checkup <- glm(CHECKUP1CLEAN ~ NUMADULT + PVTRESD1 + SEX + 
                       MARITAL + EDUCA + RENTHOM1 + VETERAN3 + 
                       CHILDREN + INCOME2 + X_BMI5 + PREGNANT + SCNTWRK1 + 
                       X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + 
                       X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + 
                       pct_primary_care_phys_cert_ehr + pct_surg_med_spec_phys_cert_ehr + 
                       pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                       pct_phys_vdt +pct_phys_vd_and_t + pct_phys_find_clin_info + 
                       pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                       pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                       pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record,
                     family=binomial(link="logit"), data = train)

final_hlth_2 <- glm(X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + 
                    MARITAL + EDUCA + RENTHOM1 + VETERAN3 + 
                    CHILDREN + INCOME2 + X_BMI5 + PREGNANT + 
                    SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + 
                    INTERNET + EXERANY2 + X_SMOKER3 + pct_phys_any_ehr + 
                    pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                    pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr + 
                    pct_phys_patient_secure_message + pct_phys_vdt +pct_phys_vd_and_t + 
                    pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                    pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                    pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                    pct_phys_integrate_summary_care_record,
                  family=binomial(link="cloglog"), data = train)

final_checkup_2 <- glm(CHECKUP1CLEAN ~ NUMADULT + PVTRESD1 + SEX + 
                       MARITAL + EDUCA + RENTHOM1 + VETERAN3 + 
                       CHILDREN + INCOME2 + X_BMI5 + PREGNANT + SCNTWRK1 + 
                       X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + 
                       X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + 
                       pct_primary_care_phys_cert_ehr + pct_surg_med_spec_phys_cert_ehr + 
                       pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                       pct_phys_vdt +pct_phys_vd_and_t + pct_phys_find_clin_info + 
                       pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                       pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                       pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record,
                     family=binomial(link="cloglog"), data = train)


##################
# Stepwise
##################

hlth_step_aic <- step(final_hlth, direction = "both", k = 2)
save(hlth_step_aic, file = 'data/hlth_step_aic.rda')
hlth_step_bic <- step(final_hlth, direction = "both", k = log(length(train$X_RFHLTH)))
save(hlth_step_bic, file = 'data/hlth_step_bic.rda')

checkup_step_aic <- step(final_checkup, direction = "both", k = 2)
save(checkup_step_aic, file = 'data/checkup_step_aic.rda')
checkup_step_bic <- step(final_checkup, direction = "both", k = log(length(train$X_RFHLTH)))
save(checkup_step_bic, file = 'data/checkup_step_bic.rda')



hlth_aic_model<- glm(formula = X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                       EDUCA + RENTHOM1 + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + 
                       PREGNANT + SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + 
                       EXERANY2 + X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + 
                       pct_primary_care_phys_cert_ehr + pct_surg_med_spec_phys_cert_ehr + 
                       pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                       pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                       pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                       pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record, 
                     family = binomial(link = "logit"), data = full_data)

hlth_bic_model <- glm(formula = X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                        EDUCA + RENTHOM1 + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + 
                        PREGNANT + SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + 
                        EXERANY2 + X_SMOKER3 + pct_surg_med_spec_phys_cert_ehr + 
                        pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                        pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                        pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                        pct_phys_integrate_summary_care_record, family = binomial(link = "logit"), 
                      data = full_data)

checkup_aic_model <- glm(formula = CHECKUP1CLEAN ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                           EDUCA + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + PREGNANT + 
                           SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + 
                           X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                           pct_surg_med_spec_phys_cert_ehr + pct_small_practice_phys_cert_ehr + 
                           pct_phys_patient_secure_message + pct_phys_vdt + pct_phys_vd_and_t + 
                           pct_phys_find_clin_info + pct_phys_send_any_clin_info + pct_phys_receive_any_clin_info + 
                           pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                           pct_phys_integrate_summary_care_record, family = binomial(link = "logit"), 
                         data = full_data)

checkup_bic_model <- glm(formula = CHECKUP1CLEAN ~ PVTRESD1 + SEX + MARITAL + VETERAN3 + 
                           CHILDREN + INCOME2 + X_BMI5 + PREGNANT + SCNTWRK1 + X_PRACE1 + 
                           X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + X_SMOKER3 + 
                           pct_phys_any_ehr + pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                           pct_surg_med_spec_phys_cert_ehr + pct_phys_patient_secure_message + 
                           pct_phys_vdt + pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                           pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                           pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record, 
                         family = binomial(link = "logit"), data = full_data)
##########################################
#Model validation with 5 fold cross validation 
##########################################

n <- dim(full_data)[1] #sample size
K <- 5
n.fold <- floor(n/K)
n.shuffle <- sample(1:n, n, replace=FALSE) #shuffle the n indexes
index.fold <- list()
for(i in 1:K)
   {
     if(i<K)
       {
       index.fold[[i]] <- n.shuffle[((i-1)*n.fold+1):(i*n.fold)]
         }else
           {
             index.fold[[i]] <- n.shuffle[((K-1)*n.fold+1):n]
            }
     }
index.fold #the indexes in your 5 folds may be different due to random partition
CV.score <- 0
for(i in 1:K){
  
  hlth_aic_model_fold<- glm(formula = X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                         EDUCA + RENTHOM1 + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + 
                         PREGNANT + SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + 
                         EXERANY2 + X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + 
                         pct_primary_care_phys_cert_ehr + pct_surg_med_spec_phys_cert_ehr + 
                         pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                         pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                         pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                         pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record, 
                       family = binomial(link = "logit"), data = full_data[index.fold[[i]],])
  
  hlth_bic_model <- glm(formula = X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                          EDUCA + RENTHOM1 + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + 
                          PREGNANT + SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + 
                          EXERANY2 + X_SMOKER3 + pct_surg_med_spec_phys_cert_ehr + 
                          pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                          pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                          pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                          pct_phys_integrate_summary_care_record, family = binomial(link = "logit"), 
                        data = full_data[index.fold[[i]],])
  
  checkup_aic_model <- glm(formula = CHECKUP1CLEAN ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                             EDUCA + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + PREGNANT + 
                             SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + 
                             X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                             pct_surg_med_spec_phys_cert_ehr + pct_small_practice_phys_cert_ehr + 
                             pct_phys_patient_secure_message + pct_phys_vdt + pct_phys_vd_and_t + 
                             pct_phys_find_clin_info + pct_phys_send_any_clin_info + pct_phys_receive_any_clin_info + 
                             pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                             pct_phys_integrate_summary_care_record, family = binomial(link = "logit"), 
                           data = full_data[index.fold[[i]],])
  
  checkup_bic_model <- glm(formula = CHECKUP1CLEAN ~ PVTRESD1 + SEX + MARITAL + VETERAN3 + 
                             CHILDREN + INCOME2 + X_BMI5 + PREGNANT + SCNTWRK1 + X_PRACE1 + 
                             X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + X_SMOKER3 + 
                             pct_phys_any_ehr + pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                             pct_surg_med_spec_phys_cert_ehr + pct_phys_patient_secure_message + 
                             pct_phys_vdt + pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                             pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                             pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record, 
                           family = binomial(link = "logit"), data = full_data[index.fold[[i]],])
  
  
  ###############################
  # do model testing below 
  ###############################
  
  
}