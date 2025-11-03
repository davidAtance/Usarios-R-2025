#install.packages(HMDHFDplus)
#install.packages(dplyr)
#install.packages(tidyr)
library(HMDHFDplus)
library(dplyr)
library(tidyr)

#I download the data for SPAIN FEMALE and FRANCE FEMALE, Exp.C and Deaths (dxt)
#It is necessary to register in HMD
#The Human mortality database (HMD) was launched in May 2002 to provide detailed 
#mortality and population data to those interested in the history of human longevity. 
#It has been put together by the Department of Demography at the University of California, 
#Berkeley, USA, and the Max Planck Institute for Demographic Research in Rostock, Germany. 
#It is freely available at http://www.mortality.org 
#and provides a highly valuable source of mortality statistics.
sp_ <- readHMDweb("ESP", "mltper_1x1", username = Sys.getenv("HMDusername"), password = Sys.getenv("HMDpassword"))
sp_2 <- readHMDweb("ESP", "fltper_1x1", username = Sys.getenv("HMDusername"), password = Sys.getenv("HMDpassword"))
sp_E <- readHMDweb("ESP", "Exposures_1x1", username = Sys.getenv("HMDusername"), password = Sys.getenv("HMDpassword"))
sp_d <- readHMDweb("ESP", "Deaths_1x1", username = Sys.getenv("HMDusername"), password = Sys.getenv("HMDpassword"))

#save(sp_, sp_E, sp_d, file = "Spain.RData")
load(file="Spain.RData")

#I estimate Ext0 for estimating qxt 
sp_male <- sp_ %>%
  mutate(Ext0 = sp_E$Male + (1 - sp_$ax) * sp_d$Male,
         Extc = sp_E$Male, 
         Dxt = sp_d$Male,
         qxt = Dxt / Ext0,
         mxt = Dxt / Extc) %>%
  select(Year, Age, Ext0, Extc, Dxt, qxt, mxt) %>%
  filter(Year > 1974, Year < 2024, Age < 100)

sp_female <- sp_2 %>%
  mutate(Ext0 = sp_E$Female + (1 - sp_2$ax) * sp_d$Female,
         Extc = sp_E$Female, 
         Dxt = sp_d$Female,
         qxt = Dxt / Ext0,
         mxt = Dxt / Extc) %>%
  select(Year, Age, Ext0, Extc, Dxt, qxt, mxt) %>%
  filter(Year > 1974, Year < 2024, Age < 100)

#Transform to matrix structure to allow the fitting of the models: 
nperiods <- max(sp_female$Year) - min(sp_female$Year) + 1
nages <- max(sp_female$Age) - min(sp_female$Age) + 1
sp <- list(Male = list(Ext0 = matrix(sp_male$Ext0, nages, nperiods, dimnames=list(c(0:99), c(1975:2023))),
                       Extc = matrix(sp_male$Extc, nages, nperiods, dimnames=list(c(0:99), c(1975:2023))),
                       Dxt = matrix(sp_male$Dxt, nages, nperiods, dimnames=list(c(0:99), c(1975:2023))),
                       mxt = matrix(sp_male$mxt, nages, nperiods, dimnames=list(c(0:99), c(1975:2023))),
                       qxt = matrix(sp_male$qxt, nages, nperiods, dimnames=list(c(0:99), c(1975:2023)))), 
           Female=list(Ext0 = matrix(sp_female$Ext0, nages, nperiods, dimnames=list(c(0:99), c(1975:2023))),
                       Extc = matrix(sp_female$Extc, nages, nperiods, dimnames=list(c(0:99), c(1975:2023))),
                       Dxt = matrix(sp_female$Dxt, nages, nperiods, dimnames=list(c(0:99), c(1975:2023))),
                       mxt = matrix(sp_female$qxt, nages, nperiods, dimnames=list(c(0:99), c(1975:2023))),
                       qxt = matrix(sp_female$mxt, nages, nperiods, dimnames=list(c(0:99), c(1975:2023)))))

#install.packages("ggplot2")
library(ggplot2)

#To see the evolution of sames ages throught the periods
sp_female %>%
  filter(Age == 0) %>%
  select(Year, qxt)

#To visualize the behaviour of one specific age 
sp_male %>%
  filter(Age == 30) %>%
  ggplot(aes(x = Year, y = log(qxt))) +
  geom_line(color = "blue", size = 1.2) +
  labs(title = "qxt para edad 40", x = "Año", y = "qxt") +
  theme_minimal()

#Some plots
#Select the chosen years
years_to_plot <- c(1975, 1985, 1995, 2005, 2018, 2020, 2023)

plot1 <- sp_female %>%
  filter(Year %in% years_to_plot) %>%
  ggplot(aes(x = Age, y = log(qxt), color = as.factor(Year))) +
  geom_line(size = 1.2) +
  labs(x = "Age", y = "log(qxt)", color = "Period", title = "qxt Spain Female") +
  theme_minimal()

plot2 <- sp_male %>%
  filter(Year %in% years_to_plot) %>%
  ggplot(aes(x = Age, y = log(qxt), color = as.factor(Year))) +
  geom_line(size = 1.2) +
  labs(x = "Age", y = "log(qxt)", color = "Period", title = "qxt Spain Male") +
  theme_minimal()

plot1
plot2

#install.packages("StMoMo")
#install.packages("CvmortalityMult")
library(StMoMo)
library(CvmortalityMult)
#Some functions have to implemented before fitting the models

#####################################################
###########  LC-UNIFACTORIAL ########################
#####################################################
constLC <- function(ax, bx, kt, b0x, gc, wxt, ages){
  c1 <- mean(kt[1, ], na.rm = TRUE)
  c2 <- sum(bx[, 1], na.rm = TRUE)
  list(ax = ax + c1 * bx, bx = bx / c2, kt =  c2 * (kt - c1))  
}
LC <- lc(link = "logit")

#####################################################
############  LC-BIFACTORIAL ########################
#####################################################
restricLC2 <- function(ax, bx, kt, b0x, gc, wxt, ages){
  N <- dim(bx)[2]
  for (y in 1:N){
    #Vamos a buscar las restricciones clásicas de LC2 sum(bxi) = 1; sum(kti) =0
    c0 <-  sum(bx[,y])
    kt[y,] <- kt[y,]*c0
    bx[,y] <- bx[,y]/c0
    c1 <- mean(kt[y,])
    ax <- ax + c1 * bx[,y]
    kt[y,] <- kt[y,] - c1 }
  list(ax = ax, bx = bx, kt=kt)}

LC2 <- StMoMo(link = "logit", staticAgeFun = TRUE, periodAgeFun = c("NP", "NP"),
              cohortAgeFun = NULL, constFun = restricLC2)
inv.logit <- function(x){exp(x)/(1+exp(x))}

##################################################################################
#1. LEE-CARTER UNI-FACTORIAL
##################################################################################
wxt_StM <- genWeightMat(ages = c(0:99), years = c(1975:2023))
LCfit_male <- fit(LC, Dxt = sp$Male$Dxt, Ext = sp$Male$Ext0, 
                  ages = c(0:99), years = c(1975:2023),
                  ages.fit = c(0:99), years.fit = c(1975:2023), 
                  wxt = wxt_StM)
LCfit_female <- fit(LC, Dxt = sp$Female$Dxt, Ext = sp$Female$Ext0, 
                    ages = c(0:99), years = c(1975:2023), 
                    ages.fit = c(0:99), years.fit = c(1975:2023),  
                    wxt = wxt_StM)

LCfut_male <- forecast(LCfit_male, h = 10, kt.method = "iarima")
LCfut_female <- forecast(LCfit_female, h = 10, kt.method = "iarima")

LCfut_male$rates
LCfut_female$rates

par(mfrow = c(1,3))
plot(c(0:99), LCfit_male$ax, type="l", lwd = 2, col = "black", 
     ylab = "age", main = "ax")
lines(c(0:99), LCfit_female$ax, lwd = 2, col= "red")
legend("bottomright", c("Male", "Female"), col = c("black", "red"), 
       lwd = 2)
plot(c(0:99), LCfit_male$bx, type="l", lwd = 2, col = "black", 
     ylab = "age", main = "bx")
lines(c(0:99), LCfit_female$bx, lwd = 2, col= "red")
legend("topright", c("Male", "Female"), col = c("black", "red"), 
       lwd = 2)
plot(c(1975:2023), LCfit_male$kt[1,], type="l", lwd = 2, col = "black", 
     ylab = "period", main = "kt")
lines(c(1975:2023), LCfit_female$kt[1,], lwd = 2, col= "red")
legend("topright", c("Male", "Female"), col = c("black", "red"), 
       lwd = 2)

#For measuring the goodness-of-fit of the model 
#we can use the functions created in CvmortalityMult

CvmortalityMult:: MeasureAccuracy(measure = "SSE",
                                  qxt_crude = sp$Male$qxt, 
                                  qxt_aju = fitted(LCfit_male, type = "rates"), 
                                  wxt = wxt_StM)
CvmortalityMult:: MeasureAccuracy(measure = "MSE",
                                  qxt_crude = sp$Male$qxt, 
                                  qxt_aju = fitted(LCfit_male, type = "rates"), 
                                  wxt = wxt_StM)
CvmortalityMult:: MeasureAccuracy(measure = "MAE",
                                  qxt_crude = sp$Male$qxt, 
                                  qxt_aju = fitted(LCfit_male, type = "rates"), 
                                  wxt = wxt_StM)
CvmortalityMult:: MeasureAccuracy(measure = "MAPE",
                                  qxt_crude = sp$Male$qxt, 
                                  qxt_aju = fitted(LCfit_male, type = "rates"), 
                                  wxt = wxt_StM)

####################################################################
#2. LEE-CARTER BI-FACTORIAL
####################################################################
LC2fit_male <- fit(LC2, Dxt = sp$Male$Dxt, Ext = sp$Male$Ext0, 
                  ages = c(0:99), years = c(1975:2023),
                  ages.fit = c(0:99), years.fit = c(1975:2023), 
                  wxt = wxt_StM)
LC2fit_female <- fit(LC2, Dxt = sp$Female$Dxt, Ext = sp$Female$Ext0, 
                    ages = c(0:99), years = c(1975:2023), 
                    ages.fit = c(0:99), years.fit = c(1975:2023),  
                    wxt = wxt_StM)

LC2fut_male <- forecast(LC2fit_male, h = 10, kt.method = "iarima")
LC2fut_female <- forecast(LC2fit_female, h = 10, kt.method = "iarima")

LC2fut_male$rates
LC2fut_female$rates

par(mfrow = c(2,3))
plot(c(0:99), LC2fit_male$ax, type="l", lwd = 2, col = "black", 
     ylab = "age", main = "ax")
lines(c(0:99), LC2fit_female$ax, lwd = 2, col= "red")
plot(c(0:99), LC2fit_male$bx[,1], type="l", lwd = 2, col = "black", 
     ylab = "age", main = "bx1")
lines(c(0:99), LC2fit_female$bx[,1], lwd = 2, col= "red")
plot(c(0:99), LC2fit_male$bx[,2], type="l", lwd = 2, col = "black", 
     ylab = "age", main = "bx2")
lines(c(0:99), LC2fit_female$bx[,1], lwd = 2, col= "red")
plot(c(1975:2023), LC2fit_male$kt[1,], type="l", lwd = 2, col = "black", 
     ylab = "period", main = "kt1")
lines(c(1975:2023), LC2fit_female$kt[1,], lwd = 2, col= "red")
plot(c(1975:2023), LC2fit_male$kt[2,], type="l", lwd = 2, col = "black", 
     ylab = "period", main = "kt1")
lines(c(1975:2023), LC2fit_female$kt[2,], lwd = 2, col= "red")

CvmortalityMult:: MeasureAccuracy(measure = "SSE",
                                  qxt_crude = sp$Male$qxt, 
                                  qxt_aju = fitted(LC2fit_male, type = "rates"), 
                                  wxt = wxt_StM)
CvmortalityMult:: MeasureAccuracy(measure = "MSE",
                                  qxt_crude = sp$Male$qxt, 
                                  qxt_aju = fitted(LC2fit_male, type = "rates"), 
                                  wxt = wxt_StM)
CvmortalityMult:: MeasureAccuracy(measure = "MAE",
                                  qxt_crude = sp$Male$qxt, 
                                  qxt_aju = fitted(LC2fit_male, type = "rates"), 
                                  wxt = wxt_StM)
CvmortalityMult:: MeasureAccuracy(measure = "MAPE",
                                  qxt_crude = sp$Male$qxt, 
                                  qxt_aju = fitted(LC2fit_male, type = "rates"), 
                                  wxt = wxt_StM)

