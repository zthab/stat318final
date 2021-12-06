library(dplyr)
library(car)
library("pROC")
library(cvAUC)
##########################################
#Model validation with 5 fold cross validation 
##########################################
load("full_comp_1.rda")
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
#performance matrices of accuracy, sensitivity, specifcity, precision
hlth_aic_perf <- c(0,0,0,0)
hlth_bic_perf <- c(0,0,0,0)
checkup_aic_perf <- c(0,0,0,0)
checkup_bic_perf <- c(0,0,0,0)
hlth_aic_preds <- vector("list", length = K) 
hlth_aic_actuals <- vector("list", length = K)
hlth_bic_preds <- vector("list", length = K) 
hlth_bic_actuals <- vector("list", length = K)
checkup_aic_preds <- vector("list", length = K) 
checkup_aic_actuals <- vector("list", length = K)
checkup_bic_preds <- vector("list", length = K) 
checkup_bic_actuals <- vector("list", length = K)

for(i in 1:K){
  
  hlth_aic_fold<- glm(formula = X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                              EDUCA + RENTHOM1 + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + 
                              PREGNANT + SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + 
                              EXERANY2 + X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + 
                              pct_primary_care_phys_cert_ehr + pct_surg_med_spec_phys_cert_ehr + 
                              pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                              pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                              pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                              pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record, 
                            family = binomial(link = "logit"), data = full_data[-index.fold[[i]],])
  
  hlth_bic_fold <- glm(formula = X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                               EDUCA + RENTHOM1 + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + 
                               PREGNANT + SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + 
                               EXERANY2 + X_SMOKER3 + pct_surg_med_spec_phys_cert_ehr + 
                               pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                               pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                               pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                               pct_phys_integrate_summary_care_record, family = binomial(link = "logit"), 
                             data = full_data[-index.fold[[i]],])
  
  checkup_aic_fold <- glm(formula = CHECKUP1CLEAN ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                                  EDUCA + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + PREGNANT + 
                                  SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + 
                                  X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                                  pct_surg_med_spec_phys_cert_ehr + pct_small_practice_phys_cert_ehr + 
                                  pct_phys_patient_secure_message + pct_phys_vdt + pct_phys_vd_and_t + 
                                  pct_phys_find_clin_info + pct_phys_send_any_clin_info + pct_phys_receive_any_clin_info + 
                                  pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                                  pct_phys_integrate_summary_care_record, family = binomial(link = "logit"), 
                                data = full_data[-index.fold[[i]],])
  
  checkup_bic_fold <- glm(formula = CHECKUP1CLEAN ~ PVTRESD1 + SEX + MARITAL + VETERAN3 + 
                                  CHILDREN + INCOME2 + X_BMI5 + PREGNANT + SCNTWRK1 + X_PRACE1 + 
                                  X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + X_SMOKER3 + 
                                  pct_phys_any_ehr + pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                                  pct_surg_med_spec_phys_cert_ehr + pct_phys_patient_secure_message + 
                                  pct_phys_vdt + pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                                  pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                                  pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record, 
                                family = binomial(link = "logit"), data = full_data[-index.fold[[i]],])
  
  ###################
  # Predicted Values
  ###################
  pred_data <- full_data[index.fold[[i]],]
  pred_n <- dim(pred_data)[1]
  hlth_aic_pi_hat <- predict.glm(hlth_aic_fold, newdata  = pred_data)
  hlth_aic_y_hat <- ifelse(hlth_aic_pi_hat>0.5,2,1)
  
  hlth_aic_preds[[i]] <- hlth_aic_pi_hat
  y <- as.character(pred_data$X_RFHLTH)
  y[y=="good"] = 1
  y[y=="bad"] = 2
  y <- as.integer(y)
  hlth_aic_actuals[[i]] <- y
  
  hlth_bic_pi_hat <- predict.glm(hlth_bic_fold, newdata = pred_data)
  hlth_bic_y_hat <- ifelse(hlth_bic_pi_hat>0.5,2,1)
  
  hlth_bic_preds[[i]] <- hlth_bic_pi_hat
  y <- as.character(pred_data$X_RFHLTH)
  y[y=="good"] = 1
  y[y=="bad"] = 2
  y <- as.integer(y)
  hlth_bic_actuals[[i]] <- y
  
  checkup_aic_pi_hat <- predict.glm(checkup_aic_fold, newdata = pred_data)
  checkup_aic_y_hat <- ifelse(checkup_aic_pi_hat>0.5,1,0)
  
  checkup_aic_preds[[i]] <- checkup_aic_pi_hat
  y <- as.character(pred_data$CHECKUP1CLEAN)
  y[y=="good"] = 1
  y[y=="bad"] = 0
  y <- as.integer(y)
  checkup_aic_actuals[[i]] <- y
  
  checkup_bic_pi_hat <- predict.glm(checkup_bic_fold, newdata = pred_data)
  checkup_bic_y_hat <- ifelse(checkup_bic_pi_hat>0.5,1,0)
  
  checkup_bic_preds[[i]] <- checkup_bic_pi_hat
  y <- as.character(pred_data$CHECKUP1CLEAN)
  y[y=="good"] = 1
  y[y=="bad"] = 0
  y <- as.integer(y)
  checkup_bic_actuals[[i]] <- y
  
  ###############################
  # do model testing below 
  ###############################
  #confusion matrices
  conf_hlth_aic <- table(hlth_aic_y_hat,pred_data$X_RFHLTH)
  conf_hlth_bic <- table(hlth_bic_y_hat,pred_data$X_RFHLTH)
  
  conf_checkup_aic <- table(checkup_aic_y_hat,pred_data$CHECKUP1CLEAN)
  conf_checkup_bic <- table(checkup_bic_y_hat,pred_data$CHECKUP1CLEAN)
  
  #adding to performance metrics
  hlth_aic_perf <- hlth_aic_perf + (1/K)*
    c((conf_hlth_aic[1,1]+conf_hlth_aic[2,2])/pred_n, 
      conf_hlth_aic[2,2]/(conf_hlth_aic[2,2]+conf_hlth_aic[1,2]),
      conf_hlth_aic[1,1]/(conf_hlth_aic[1,1]+conf_hlth_aic[2,1]),
      conf_hlth_aic[2,2]/(conf_hlth_aic[2,2]+conf_hlth_aic[2,1]))
  
  hlth_bic_perf <- hlth_bic_perf + (1/K)*
    c((conf_hlth_bic[1,1]+conf_hlth_bic[2,2])/pred_n, 
      conf_hlth_bic[2,2]/(conf_hlth_bic[2,2]+conf_hlth_bic[1,2]),
      conf_hlth_bic[1,1]/(conf_hlth_bic[1,1]+conf_hlth_bic[2,1]),
      conf_hlth_bic[2,2]/(conf_hlth_bic[2,2]+conf_hlth_bic[2,1]))
  
  checkup_aic_perf <- checkup_aic_perf + (1/K)*
    c((conf_checkup_aic[1,1]+conf_checkup_aic[2,2])/pred_n, 
      conf_checkup_aic[2,2]/(conf_checkup_aic[2,2]+conf_checkup_aic[1,2]),
      conf_checkup_aic[1,1]/(conf_checkup_aic[1,1]+conf_checkup_aic[2,1]),
      conf_checkup_aic[2,2]/(conf_checkup_aic[2,2]+conf_checkup_aic[2,1]))
  
  checkup_bic_perf <- checkup_bic_perf + (1/K)*
    c((conf_checkup_bic[1,1]+conf_checkup_bic[2,2])/pred_n, 
      conf_checkup_bic[2,2]/(conf_checkup_bic[2,2]+conf_checkup_bic[1,2]),
      conf_checkup_bic[1,1]/(conf_checkup_bic[1,1]+conf_checkup_bic[2,1]),
      conf_checkup_bic[2,2]/(conf_checkup_bic[2,2]+conf_checkup_bic[2,1]))
}

# AUC values and graphs
par(mfrow=c(2,2))
auc_1 <- cvAUC(hlth_aic_preds, hlth_aic_actuals)
plot(auc_1$perf, col="black", lty=3, main=paste0("Health AIC - 5-fold CV AUC"))
plot(auc_1$perf, col="blue", lty=1, avg="vertical", add=TRUE)
text(0.8,0.2, paste("mean AUC:", format(round(auc_1$cvAUC, 4), nsmall=4)))
text(0.8,0.1, paste("AUC:", format(round(min(auc_1$fold.AUC), 4), nsmall=4), "-", format(round(max(auc_1$fold.AUC), 4), nsmall=4)))

auc_2 <- cvAUC(hlth_bic_preds, hlth_bic_actuals)
plot(auc_2$perf, col="black", lty=3, main=paste0("Health BIC - 5-fold CV AUC"))
plot(auc_2$perf, col="blue", lty=1, avg="vertical", add=TRUE)
text(0.8,0.2, paste("mean AUC:", format(round(auc_2$cvAUC, 4), nsmall=4)))
text(0.8,0.1, paste("AUC:", format(round(min(auc_2$fold.AUC), 4), nsmall=4), "-", format(round(max(auc_2$fold.AUC), 4), nsmall=4)))

auc_3 <- cvAUC(checkup_aic_preds, checkup_aic_actuals)
plot(auc_3$perf, col="black", lty=3, main=paste0("Checkup AIC - 5-fold CV AUC"))
plot(auc_3$perf, col="blue", lty=1, avg="vertical", add=TRUE)
text(0.8,0.2, paste("mean AUC:", format(round(auc_3$cvAUC, 4), nsmall=4)))
text(0.8,0.1, paste("AUC:", format(round(min(auc_3$fold.AUC), 4), nsmall=4), "-", format(round(max(auc_3$fold.AUC), 4), nsmall=4)))

auc_4 <- cvAUC(checkup_bic_preds, checkup_bic_actuals)
plot(auc_4$perf, col="black", lty=3, main=paste0("Checkup BIC - 5-fold CV AUC"))
plot(auc_4$perf, col="blue", lty=1, avg="vertical", add=TRUE)
text(0.8,0.2, paste("mean AUC:", format(round(auc_4$cvAUC, 4), nsmall=4)))
text(0.8,0.1, paste("AUC:", format(round(min(auc_4$fold.AUC), 4), nsmall=4), "-", format(round(max(auc_4$fold.AUC), 4), nsmall=4)))

# Likelihood Ratio Test
full <- glm(X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + 
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
            family=binomial(link="logit"), data = full_data)

full_2 <- glm(CHECKUP1CLEAN ~ NUMADULT + PVTRESD1 + SEX + 
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
            family=binomial(link="logit"), data = full_data)

base <- glm(formula = X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                      EDUCA + RENTHOM1 + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + 
                      PREGNANT + SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + 
                      EXERANY2 + X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + 
                      pct_primary_care_phys_cert_ehr + pct_surg_med_spec_phys_cert_ehr + 
                      pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                      pct_phys_send_any_clin_info + pct_phys_send_summary_care_record + 
                      pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                      pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record, 
                    family = binomial(link = "logit"), data = full_data)
base_2 <- glm(formula = X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                      EDUCA + RENTHOM1 + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + 
                      PREGNANT + SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + 
                      EXERANY2 + X_SMOKER3 + pct_surg_med_spec_phys_cert_ehr + 
                      pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                      pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                      pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                      pct_phys_integrate_summary_care_record, family = binomial(link = "logit"), data = full_data)
base_3 <- glm(formula = CHECKUP1CLEAN ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                EDUCA + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + PREGNANT + 
                SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + 
                X_SMOKER3 + pct_phys_any_ehr + pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                pct_surg_med_spec_phys_cert_ehr + pct_small_practice_phys_cert_ehr + 
                pct_phys_patient_secure_message + pct_phys_vdt + pct_phys_vd_and_t + 
                pct_phys_find_clin_info + pct_phys_send_any_clin_info + pct_phys_receive_any_clin_info + 
                pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                pct_phys_integrate_summary_care_record, family = binomial(link = "logit"), data = full_data)

base_4 <- glm(formula = CHECKUP1CLEAN ~ PVTRESD1 + SEX + MARITAL + VETERAN3 + 
                CHILDREN + INCOME2 + X_BMI5 + PREGNANT + SCNTWRK1 + X_PRACE1 + 
                X_HISPANC + HLTHPLN1 + INTERNET + EXERANY2 + X_SMOKER3 + 
                pct_phys_any_ehr + pct_phys_basic_ehr + pct_primary_care_phys_cert_ehr + 
                pct_surg_med_spec_phys_cert_ehr + pct_phys_patient_secure_message + 
                pct_phys_vdt + pct_phys_find_clin_info + pct_phys_send_any_clin_info + 
                pct_phys_receive_any_clin_info + pct_phys_receive_summary_care_record + 
                pct_phys_integrate_any_clin_info + pct_phys_integrate_summary_care_record, 
              family = binomial(link = "logit"), data = full_data)

library(lmtest)
lrtest(base, full) #0.2496 (Health AIC)
lrtest(base_2, full) #0.0001622 (Health BIC)
lrtest(base_3, full_2) #.2848 (Checkup AIC)
lrtest(base_4, full_2) #8.183e-05 (Checkup BIC)




#######
#Residiual Analysis
#####
hlth_bic_model <- glm(formula = X_RFHLTH ~ NUMADULT + PVTRESD1 + SEX + MARITAL + 
                       EDUCA + RENTHOM1 + VETERAN3 + CHILDREN + INCOME2 + X_BMI5 + 
                       PREGNANT + SCNTWRK1 + X_PRACE1 + X_HISPANC + HLTHPLN1 + INTERNET + 
                       EXERANY2 + X_SMOKER3 + pct_surg_med_spec_phys_cert_ehr + 
                       pct_small_practice_phys_cert_ehr + pct_phys_patient_secure_message + 
                       pct_phys_send_summary_care_record + pct_phys_receive_any_clin_info + 
                       pct_phys_receive_summary_care_record + pct_phys_integrate_any_clin_info + 
                       pct_phys_integrate_summary_care_record, family = binomial(link = "logit"), 
                     data = full_data)
plot(hlth_bic_model)

