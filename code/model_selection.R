library(car)
library(dplyr)
load("full_comp_1.rda")
set.seed(123)

##################
# VIF Analysis
# We run an iterative VIF analysis where we remove variables that have a VIF of
# over 10. For our purposes, we only consider the VIF of numeric variables. 
##################
hlth_vif_del <- c()

hlth.logit <- glm(X_RFHLTH~NUMADULT + CHILDREN + X_BMI5 + SCNTWRK1 +pct_phys_any_ehr + 
                     pct_phys_basic_ehr + pct_phys_cert_ehr + pct_primary_care_phys_cert_ehr +
                     pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr + 
                     pct_phys_send_receive_any_patient_info + pct_phys_e_share_patients +
                     pct_phys_patient_secure_message + pct_phys_vdt +pct_phys_vd_and_t + 
                     pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                     pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                     pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                     pct_phys_integrate_summary_care_record,
                   family=binomial(link="logit"), data = full_data)

c(which.max(car::vif(hlth.logit)), max(car::vif(hlth.logit)))
hlth_vif_del <- append(hlth_vif_del, which.max(car::vif(hlth.logit)))

hlth.logit <- glm(X_RFHLTH~NUMADULT + CHILDREN + X_BMI5 + SCNTWRK1 +  pct_phys_any_ehr + 
                     pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr +
                     pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr + 
                     pct_phys_send_receive_any_patient_info + pct_phys_e_share_patients +
                     pct_phys_patient_secure_message + pct_phys_vdt +pct_phys_vd_and_t + 
                     pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                     pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                     pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                     pct_phys_integrate_summary_care_record,
                   family=binomial(link="logit"), data = full_data)

c(which.max(car::vif(hlth.logit)), max(car::vif(hlth.logit)))
hlth_vif_del <- append(hlth_vif_del, which.max(car::vif(hlth.logit)))

hlth.logit <- glm(X_RFHLTH~NUMADULT + CHILDREN + X_BMI5 + SCNTWRK1 +  pct_phys_any_ehr + 
                     pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr +
                     pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr+
                     pct_phys_e_share_patients +pct_phys_patient_secure_message + 
                     pct_phys_vdt +pct_phys_vd_and_t + pct_phys_find_clin_info + 
                     pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                     pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                     pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record,
                   family=binomial(link="logit"), data = full_data)

c(which.max(car::vif(hlth.logit)), max(car::vif(hlth.logit)))
hlth_vif_del <- append(hlth_vif_del, which.max(car::vif(hlth.logit)))

hlth.logit <- glm(X_RFHLTH~NUMADULT + CHILDREN + X_BMI5 + SCNTWRK1 +  pct_phys_any_ehr + 
                    pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr +
                    pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr+
                    pct_phys_patient_secure_message + pct_phys_vdt +pct_phys_vd_and_t + 
                    pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                    pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                    pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                    pct_phys_integrate_summary_care_record,
                  family=binomial(link="logit"), data = full_data)

c(which.max(car::vif(hlth.logit)), max(car::vif(hlth.logit)))

# none other are higher than 8 (next highest is pct_phys_receive_summary_care_record at 7.7)

chckup_vif_del <- c()

chckup.logit <- glm(CHECKUP1CLEAN~NUMADULT + CHILDREN + X_BMI5 + SCNTWRK1 +  pct_phys_any_ehr + 
                    pct_phys_basic_ehr + pct_phys_cert_ehr + pct_primary_care_phys_cert_ehr +
                    pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr + 
                    pct_phys_send_receive_any_patient_info + pct_phys_e_share_patients +
                    pct_phys_patient_secure_message + pct_phys_vdt +pct_phys_vd_and_t + 
                    pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                    pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                    pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                    pct_phys_integrate_summary_care_record,
                  family=binomial(link="logit"), data = full_data)

c(which.max(car::vif(chckup.logit)), max(car::vif(chckup.logit)))
chckup_vif_del <- append(chckup_vif_del, which.max(car::vif(chckup.logit)))

chckup.logit <- glm(CHECKUP1CLEAN~NUMADULT + CHILDREN + X_BMI5 + SCNTWRK1 +  pct_phys_any_ehr + 
                    pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr +
                    pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr + 
                    pct_phys_send_receive_any_patient_info + pct_phys_e_share_patients +
                    pct_phys_patient_secure_message + pct_phys_vdt +pct_phys_vd_and_t + 
                    pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                    pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                    pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                    pct_phys_integrate_summary_care_record,
                  family=binomial(link="logit"), data = full_data)

c(which.max(car::vif(chckup.logit)), max(car::vif(chckup.logit)))
chckup_vif_del <- append(chckup_vif_del, which.max(car::vif(chckup.logit)))

chckup.logit <- glm(CHECKUP1CLEAN~NUMADULT + CHILDREN + X_BMI5 + SCNTWRK1 +  pct_phys_any_ehr + 
                    pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr +
                    pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr+
                    pct_phys_e_share_patients + pct_phys_patient_secure_message + 
                    pct_phys_vdt +pct_phys_vd_and_t + pct_phys_find_clin_info + 
                    pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                    pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                    pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record,
                  family=binomial(link="logit"), data = full_data)

c(which.max(car::vif(chckup.logit)), max(car::vif(chckup.logit)))
chckup_vif_del <- append(chckup_vif_del, which.max(car::vif(chckup.logit)))

chckup.logit <- glm(CHECKUP1CLEAN~NUMADULT + CHILDREN + X_BMI5 + SCNTWRK1 +  pct_phys_any_ehr + 
                      pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr +
                      pct_surg_med_spec_phys_cert_ehr +  pct_small_practice_phys_cert_ehr +
                      pct_phys_patient_secure_message + pct_phys_vdt + pct_phys_vd_and_t + 
                      pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                      pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                      pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                      pct_phys_integrate_summary_care_record,
                    family=binomial(link="logit"), data = full_data)

c(which.max(car::vif(chckup.logit)), max(car::vif(chckup.logit)))


# final full models
final_hlth <- glm(X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + MARITAL + EDUCA + RENTHOM1 + 
                    VETERAN3 + EMPLOY1 + CHILDREN + INCOME2 + X_BMI5 + PREGNANT + 
                    SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + 
                    X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + 
                    pct_primary_care_phys_cert_ehr + pct_surg_med_spec_phys_cert_ehr +  
                    pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                    pct_phys_vdt + pct_phys_vd_and_t + pct_phys_find_clin_info + 
                    pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                    pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                    pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record,
                  family=binomial(link="logit"), data = full_data)

final_checkup <- glm(CHECKUP1CLEAN ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                       EDUCA + RENTHOM1 + VETERAN3 + EMPLOY1 + CHILDREN + 
                       INCOME2 + X_BMI5 + PREGNANT + SCNTWRK1 + X_PRACE1 + 
                       X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + X_SMOKER3 + 
                       pct_phys_any_ehr + pct_phys_basic_ehr + 
                       pct_primary_care_phys_cert_ehr + pct_surg_med_spec_phys_cert_ehr + 
                       pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                       pct_phys_vdt +pct_phys_vd_and_t + pct_phys_find_clin_info + 
                       pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                       pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                       pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record,
                     family=binomial(link="logit"), data = full_data)


#############################################################
# Stepwise regression selection using AIC and BIC criterion
#############################################################

hlth_step_aic <- step(final_hlth, direction = "both", k = 2)
save(hlth_step_aic, file = 'data/hlth_step_aic.rda')

hlth_step_bic <- step(final_hlth, direction = "both", k = 
                        log(length(full_model$X_RFHLTH)))
save(hlth_step_bic, file = 'data/hlth_step_bic.rda')

checkup_step_aic <- step(final_checkup, direction = "both", k = 2)
save(checkup_step_aic, file = 'data/checkup_step_aic.rda')

checkup_step_bic <- step(final_checkup, direction = "both", k = 
                           log(length(full_model$X_RFHLTH)))
save(checkup_step_bic, file = 'data/checkup_step_bic.rda')

hlth_aic_model<- glm(formula = X_RFHLTH ~ NUMADULT + SEX + MARITAL + EDUCA + RENTHOM1 + 
                       VETERAN3 + EMPLOY1 + CHILDREN + INCOME2 + X_BMI5 + PREGNANT + 
                       CNTWRK1 + X_PRACE1 + X_HISPANC + INTERNET + EXERANY2 + X_SMOKER3 + 
                       pct_phys_any_ehr + pct_primary_care_phys_cert_ehr + 
                       pct_surg_med_spec_phys_cert_ehr + pct_small_practice_phys_cert_ehr + 
                       pct_phys_patient_secure_message + pct_phys_send_summary_care_record + 
                       pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                       pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record, 
                     family = binomial(link = "logit"), data = full_data)

hlth_bic_model <- glm(formula = X_RFHLTH ~ NUMADULT + MARITAL + EDUCA + RENTHOM1 + 
                        VETERAN3 + EMPLOY1 + CHILDREN + INCOME2 + X_BMI5 + PREGNANT + 
                        SCNTWRK1 + X_HISPANC + INTERNET + EXERANY2 + X_SMOKER3 + 
                        pct_primary_care_phys_cert_ehr + pct_surg_med_spec_phys_cert_ehr + 
                        pct_small_practice_phys_cert_ehr + pct_phys_send_summary_care_record + 
                        pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                        pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record, 
                      family = binomial(link = "logit"), data = full_data)

checkup_aic_model <- glm(formula = CHECKUP1CLEAN ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                           EDUCA + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + PREGNANT + 
                           SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + 
                           X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + 
                           pct_primary_care_phys_cert_ehr + pct_surg_med_spec_phys_cert_ehr + 
                           pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                           pct_phys_vdt + pct_phys_vd_and_t + pct_phys_find_clin_info + 
                           pct_phys_send_any_clin_info + pct_phys_receive_any_clin_info + 
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
