#install.packages(HMDHFDplus)
#install.packages(dplyr)
#install.packages(tidyr)
library(HMDHFDplus)
library(dplyr)
library(tidyr)

load(file="CountryCombined.RData")
head(males)
head(females)
head(countr)

#install.packages("ggplot2")
library(ggplot2)

#To see the evolution of sames ages throught the periods
females %>%
  filter(Age == 0) %>%
  select(Year, qxt)

#To visualize the behaviour of one specific age 
males %>%
  filter(Age == 30, Year >= 1975, Year < 2023, Country %in% c("ESP", "FRA", "ITA")) %>%
  ggplot(aes(x = Year, y = log(qxt), color = Country)) +
  geom_line(size = 1.2) +
  labs(title = "Evolution of log(qxt) for age 30 in males",
       x = "Period", y = "log(qxt)", color = "Country") +
  theme_minimal()
#To visualize the behaviour of one specific age 
males %>%
  filter(Age == 66, Year >= 1975, Year < 2023, Country %in% c("ESP", "FRA", "ITA")) %>%
  ggplot(aes(x = Year, y = log(qxt), color = Country)) +
  geom_line(size = 1.2) +
  labs(title = "Evolution of log(qxt) for age 65 in males",
       x = "Period", y = "log(qxt)", color = "Country") +
  theme_minimal()

females %>%
  filter(Age == 30, Year >= 1975, Year < 2023, Country %in% c("ESP", "FRA", "ITA")) %>%
  ggplot(aes(x = Year, y = log(qxt), color = Country)) +
  geom_line(size = 1.2) +
  labs(title = "Evolution of log(qxt) for age 30 in males",
       x = "Period", y = "log(qxt)", color = "Country") +
  theme_minimal()
#To visualize the behaviour of one specific age 
females %>%
  filter(Age == 66, Year >= 1975, Year < 2023, Country %in% c("ESP", "FRA", "ITA")) %>%
  ggplot(aes(x = Year, y = log(qxt), color = Country)) +
  geom_line(size = 1.2) +
  labs(title = "Evolution of log(qxt) for age 30 in males",
       x = "Period", y = "log(qxt)", color = "Country") +
  theme_minimal()

#Some plots
plot1 <- males %>%
  filter(Year %in% 2020) %>%
  ggplot(aes(x = Age, y = log(qxt), color = as.factor(Country))) +
  geom_line(size = 1.2) +
  labs(x = "Age", y = "log(qxt)", color = "Period", title = "qxt Males") +
  theme_minimal()
plot1

plot2 <- females %>%
  filter(Year %in% 2020) %>%
  ggplot(aes(x = Age, y = log(qxt), color = as.factor(Country))) +
  geom_line(size = 1.2) +
  labs(x = "Age", y = "log(qxt)", color = "Period", title = "qxt Female") +
  theme_minimal()
plot2

#install.packages("CvmortalityMult")
library(CvmortalityMult)
##########################################################
#FITTING PROCESS
#help functions 
?CvmortalityMult::fitLCmulti
#There are five different multi-population mortality models 
#that can be used for fitting or forecasting mortality. 
#model = c("additive", "multiplicative", "CFM","ACFM", "joint-K")
#MALES
additive_males <- fitLCmulti(qxt = males_combined$qxt, 
                             periods = c(1975:2022), 
                             ages = c(0:99),
                             nPop = 4, 
                             model = "additive", 
                             lxt = NULL)
plot(additive_males)
#FEMALES
additive_females <- fitLCmulti(qxt = females_combined$qxt, 
                               periods = c(1975:2022),  
                               ages = c(0:99),  
                               nPop = 4,   
                               model = "additive", 
                               lxt = NULL)
additive_males
plot(additive_males)

additive_females
plot(additive_females)

#It should be remenber that for model = "CFM","ACFM", "joint-K"
#The first populations must corresponds to the mortality rates 
#of all populations considered (all as a whole).

##########################################################
#FORECASTIN PROCESS
?CvmortalityMult::forecast.fitLCmulti

#install.packages("forecas")
library(forecast)
#three options for forecasting mortality rates: 
#ktmethod = c("arimapdq", "arima010", "arimauser")

future_males <- forecast(object = additive_males, 
                         nahead = 10, ktmethod = 'arimapdq')
future_females <- forecast(object = additive_males, 
                         nahead = 10, ktmethod = 'arimapdq')

future_males
plot(future_males)

future_females
plot(future_females)

