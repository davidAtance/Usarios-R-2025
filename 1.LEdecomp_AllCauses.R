#install.packages("devtools")
library(devtools)

#1. Using the GitHub account of one of the owner of the library.
#devtools::install_github("timriffe/LEdecomp")
library(LEdecomp)

#I download the data for SPAIN FEMALE and FRANCE FEMALE, Exp.C and Deaths (dxt)
#It is necessary to register in HMD
#The Human mortality database (HMD) was launched in May 2002 to provide detailed 
#mortality and population data to those interested in the history of human longevity. 
#It has been put together by the Department of Demography at the University of California, 
#Berkeley, USA, and the Max Planck Institute for Demographic Research in Rostock, Germany. 
#It is freely available at http://www.mortality.org 
#and provides a highly valuable source of mortality statistics.

load(file="Spain.RData")

#install.packages("dplyr")
library(dplyr)


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


#We need the mxt for two specif periods to apply the life-expectancy 
#decomposition methods. 
#Thus, we use the next function in order to select some specific 
#period for male and female population for Spain population.
sp_male_2015 <- sp_male %>%
  select(Year, Age, mxt) %>%
  filter(Year == 2015)

sp_female_2015 <- sp_female %>%
  select(Year, Age, mxt) %>%
  filter(Year == 2015)

plot(c(0:99), log(sp_male_2015$mxt), type ="l", col= "black", 
     lwd =2, xlab = "Age", ylab = "log(mx,2015)", 
     main = "Spain in 2015")
lines(c(0:99), log(sp_female_2015$mxt), col = "red", lwd=2)
legend("topleft", col = c("black", "red"), lty = 1, 
       lwd = 2, c("Male", "Female"))

#The function to decomposing Life-expectancy
??LEdecomp::LEdecomp

#Check the different methods that the user can executed
LEdecomp::method_registry$method

LE_arri_spain_2015 <- LEdecomp(mx1 = sp_male_2015$mxt, 
                               mx2 = sp_female_2015$mxt,
                               age = c(0:99), 
                               method = "arriaga")
LE_arri_spain_2015_2 <- LEdecomp(mx1 = sp_female_2015$mxt, 
                                 mx2 = sp_male_2015$mxt,
                               age = c(0:99), 
                               method = "arriaga")
LE_arri_spain_2015_sym <- LEdecomp(mx1 = sp_female_2015$mxt, 
                                 mx2 = sp_male_2015$mxt,
                                 age = c(0:99), 
                                 method = "arriaga_sym")

LE_arri_spain_2015
LE_arri_spain_2015_2
LE_arri_spain_2015_sym

LE_arri_spain_2015$LE1
LE_arri_spain_2015$LE2
LE_arri_spain_2015$LE2 - LE_arri_spain_2015$LE1
LE_arri_spain_2015

plot(c(0:99), LE_arri_spain_2015$LEdecomp,type="l", lwd = 2, 
     col = "black", xlab = "Age", main = "LE gender gap decomp Spain for 2015")
lines(c(0:99), -LE_arri_spain_2015_2$LEdecomp, col = "red", 
      lwd = 2)
lines(c(0:99), -LE_arri_spain_2015_sym$LEdecomp, col = "green", 
      lwd = 2)
legend("topleft", c("mx1->mx2", "mx2->mx1", "arri-sym"),
       col = c("black", "red", "blue"), lwd = 2, lty = 1)

#install.packages("ggplot2)
library(ggplot2)

plot(LE_arri_spain_2015)

df <- LE_arri_spain_2015$LEdecomp
df <- data.frame(Age = c(0:99), Difference = LE_arri_spain_2015$LEdecomp)

# Gráfico solo con barras verdes
ggplot(df, aes(x = Age, y = Difference)) +
  geom_col(alpha = 0.9, fill = "lightgreen", col = "black") +
  labs(title = "Arriaga LE Decomposition", 
       x = "Age", y = "Contribution by age (in years)") +
  theme_minimal()


#Repeat the process for 2019
#First choose the dataset
sp_male_2019 <- sp_male %>%
  select(Year, Age, mxt) %>%
  filter(Year == 2019)

sp_female_2019 <- sp_female %>%
  select(Year, Age, mxt) %>%
  filter(Year == 2019)

LE_ar_spain_2019 <- LEdecomp(mx1 = sp_male_2019$mxt, 
                               mx2 = sp_female_2019$mxt,
                               age = c(0:99), 
                               method = "arriaga_sym")

LE_hor_spain_2019 <- LEdecomp(mx1 = sp_female_2015$mxt, 
                                   mx2 = sp_male_2015$mxt,
                                   age = c(0:99), 
                                   method = "horiuchi")

LE_num_spain_2019 <- LEdecomp(mx1 = sp_female_2015$mxt, 
                              mx2 = sp_male_2015$mxt,
                              age = c(0:99), 
                              method = "numerical")


LE_ar_spain_2019
LE_hor_spain_2019
LE_num_spain_2019

plot(c(0:99), LE_ar_spain_2019$LEdecomp,type="l", lwd = 2, 
     col = "black", xlab = "Age", 
     main = "LE gender gap decomp Spain for 2019")
lines(c(0:99), -LE_hor_spain_2019$LEdecomp, col = "red", 
      lwd = 2)
lines(c(0:99), -LE_num_spain_2019$LEdecomp, col = "green", 
      lwd = 2)
legend("topleft", c("arri-sym", "horiuchi", "num"),
       col = c("black", "red", "blue"), lwd = 2, lty = 1)


#Changes with covid-effect pre-pandemic, in pandemic and post-pandemic
LE_ar_spain_2019 <- LEdecomp(mx1 = sp_male %>%
                               filter(Year == 2019) %>%
                               select(mxt), 
                             mx2 = sp_female %>%
                               filter(Year == 2019) %>%
                               select(mxt),
                             age = c(0:99), 
                             method = "arriaga_sym")
LE_ar_spain_2020 <- LEdecomp(mx1 = sp_male %>%
                               filter(Year == 2020) %>%
                               select(mxt), 
                             mx2 = sp_female %>%
                               filter(Year == 2020) %>%
                               select(mxt),
                             age = c(0:99), 
                             method = "arriaga_sym")
LE_ar_spain_2022 <- LEdecomp(mx1 = sp_male %>%
                               filter(Year == 2022) %>%
                               select(mxt), 
                             mx2 = sp_female %>%
                               filter(Year == 2022) %>%
                               select(mxt),
                             age = c(0:99), 
                             method = "arriaga_sym")

LE_ar_spain_2019
LE_ar_spain_2020
LE_ar_spain_2022

plot(c(0:99), LE_ar_spain_2019$LEdecomp,type="l", lwd = 2, 
     col = "black", xlab = "Age", ylab = "Age-contribution", 
     main = "LE gender gap decomp Spain for 2019, 2020, 2022")
lines(c(0:99), LE_ar_spain_2020$LEdecomp, col = "red", 
      lwd = 2)
lines(c(0:99), LE_ar_spain_2022$LEdecomp, col = "green", 
      lwd = 2)
legend("topleft", c("2019", "2020", "2022"),
       col = c("black", "red", "blue"), lwd = 2, lty = 1)
