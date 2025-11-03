#install.packages(HMDHFDplus)
#install.packages(dplyr)
#install.packages(tidyr)
library(HMDHFDplus)
library(dplyr)
library(tidyr)

load(file="CountryCombined.RData")
head(males_combined)
head(females_combined)

#install.packages("CvmortalityMult")
library(CvmortalityMult)
##########################################################
#CROSS-VALIDATION FOR MULTI-POPULATION MORTALITY MODELS
#help functions 
?CvmortalityMult::fitLCmulti
#1. Rolling-Origin (RO) recalibration using a common-CV method for 
#four considered populations in male 
#which is defined by default with fixed_train_origin = TRUE
?CvmortalityMult::multipopulation_cv
add_cv_males <- multipopulation_cv(qxt = males_combined$qxt,
                               model = c('additive'),
                               periods =  c(1975:2022), 
                               ages = c(0:99), nPop = 4,
                               lxt = NULL,
                               trainset1 = 38, nahead = 10,
                               ktmethod = c('arimapdq'), 
                               measures = c("MSE"))
mult_cv_males <- multipopulation_cv(qxt = males_combined$qxt,
                               model = c('multiplicative'),
                               periods =  c(1975:2022), 
                               ages = c(0:99), nPop = 4,
                               lxt = NULL,
                               trainset1 = 38, nahead = 10,
                               ktmethod = c('arimapdq'), 
                               measures = c("MSE"))
cfm_cv_males <- multipopulation_cv(qxt = males_combined$qxt,
                                    model = c('CFM'),
                                    periods =  c(1975:2022), 
                                    ages = c(0:99), nPop = 4,
                                    lxt = NULL,
                                    trainset1 = 38, nahead = 10,
                                    ktmethod = c('arimapdq'), 
                                    measures = c("MSE"))

add_cv_males
mult_cv_males
cfm_cv_males

add_cv_males$meas_total
add_cv_males$meas_ages
add_cv_males$meas_periodsfut
add_cv_males$meas_pop

plot(c(1:4), add_cv_males$meas_pop, type="l", col ="black", lwd=2)
lines(c(1:4), mult_cv_males$meas_pop, lwd =2, col = "red")
lines(c(1:4), cfm_cv_males$meas_pop, lwd =2, col = "green")
legend("topleft", c("Addi", "Mult", "CFM"), 
       col = c("black", "red", "green"), lwd = 2)

plot(c(0:99), add_cv_males$meas_ages, type="l", col ="black", lwd=2)
lines(c(0:99), mult_cv_males$meas_ages, lwd =2, col = "red")
lines(c(0:99), cfm_cv_males$meas_ages, lwd =2, col = "green")
legend("topleft", c("Addi", "Mult", "CFM"), 
       col = c("black", "red", "green"), lwd = 2)

####################################################################
#2. Rolling-Origin (RO) recalibration using the LOOCV 
#keeping fixed the train origin (fixed_train_origin = TRUE, default value)
#It will take some time due to the number of recalibrations.

#Equally, as in the RO recalibration with the standard CV, users can access
#some important information regarding the forecasting accuracy by writing
#the created object as follows:
add_loocv_males <- multipopulation_cv(qxt = males_combined$qxt,
                                   model = c('additive'),
                                   periods =  c(1975:2022), 
                                   ages = c(0:99), nPop = 4,
                                   lxt = NULL,
                                   trainset1 = 45, nahead = 1,
                                   ktmethod = c('arimapdq'), 
                                   measures = c("MSE"))
mult_loocv_males <- multipopulation_cv(qxt = males_combined$qxt,
                                    model = c('multiplicative'),
                                    periods =  c(1975:2022), 
                                    ages = c(0:99), nPop = 4,
                                    lxt = NULL,
                                    trainset1 = 45, nahead = 1,
                                    ktmethod = c('arimapdq'), 
                                    measures = c("MSE"))
cfm_loocv_males <- multipopulation_cv(qxt = males_combined$qxt,
                                   model = c('CFM'),
                                   periods =  c(1975:2022), 
                                   ages = c(0:99), nPop = 4,
                                   lxt = NULL,
                                   trainset1 = 45, nahead = 1,
                                   ktmethod = c('arimapdq'), 
                                   measures = c("MSE"))
#save(add_loocv_males, mult_loocv_males, cfm_loocv_males, file = "loocv.RData")
load(file = "loocv.RData")

add_loocv_males
mult_loocv_males
cfm_loocv_males

plot(c(1:4), add_loocv_males$meas_pop, type="l", col ="black", lwd=2)
lines(c(1:4), mult_loocv_males$meas_pop, lwd =2, col = "red")
lines(c(1:4), cfm_loocv_males$meas_pop, lwd =2, col = "green")
legend("topleft", c("Addi", "Mult", "CFM"), 
       col = c("black", "red", "green"), lwd = 2)

plot(c(0:99), add_loocv_males$meas_ages, type="l", col ="black", lwd=2)
lines(c(0:99), mult_loocv_males$meas_ages, lwd =2, col = "red")
lines(c(0:99), cfm_loocv_males$meas_ages, lwd =2, col = "green")
legend("topleft", c("Addi", "Mult", "CFM"), 
       col = c("black", "red", "green"), lwd = 2)

#############################################################
#3. CROSS-VALIDATION with fixed_train_origin = FALSE (default-value)
#which implies a rolling-origin recalibration evaluation
#First, as above, we apply the best ARIMA (p,d,q) for each trend parameter with
#ktmethod = 'arimapdq'
cv_SM_CFM <- multipopulation_cv(qxt = males_combined$qxt,
                                model = c('CFM'),
                                periods =  c(1975:2022), 
                                ages = c(0:99), nPop = 4,
                                lxt = NULL,
                                trainset1 = 45, nahead = 1,
                                ktmethod = c('arimapdq'), 
                                measures = c("MSE"), 
                                fixed_train_origin = FALSE)
cv_SM_CFM

#######################################################
#4. CROSS-VALIDATION with fixed_train_origin = 'add_remove1'
#which implies a rolling-origin recalibration evaluation but
#only adding and deleting one period in each iteration.
cv_SM_CFM2 <- multipopulation_cv(qxt = males_combined$qxt,
                                model = c('CFM'),
                                periods =  c(1975:2022), 
                                ages = c(0:99), nPop = 4,
                                lxt = NULL,
                                trainset1 = 40, nahead = 6,
                                ktmethod = c('arimapdq'), 
                                measures = c("MSE"), 
                                fixed_train_origin = 'add_remove1')
cv_SM_CFM2

save(cv_SM_CFM, cv_SM_CFM2, file = "ResultsCV.RData")
load(file = "ResultsCV.RData")
