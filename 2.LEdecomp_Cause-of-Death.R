#install.packages("devtools")
library(devtools)

#1. Using the GitHub account of one of the owner of the library.
#devtools::install_github("timriffe/LEdecomp")
library(LEdecomp)

#For cause-of-death analysis we provide in our package 
#the following dataset
??LEdecomp::US_data_CoD

#(also the US all causes data is provided in the package)
??LEdecomp::US_data

#which provides information from US population. 
#The dataset obtains the information from two sources:
#1. Human Mortality Dataset for Exposures.
#2. National Center for Health Statistics (NCHS) for Death register by causes

#With this two variables, we construct the mxt = Dxt/Ecxt 
#by period from 2000 to 2020 and for integer age from 0 to 100

#For Spain, INE provides some information about the number of deaths
#for different causes of deaths by group of ages (0, 1-4, 5-9, ..., 90-95, +95)
#check this link https://www.ine.es/jaxi/Tabla.htm?tpx=75296&L=0

#For the rest of the countries Human Mortality Database has its own 
#repository for Data
#https://www.mortality.org/Data/HCD
#Equally as in Spain the data are provided by group of ages 
US_data_CoD

df <- as.data.frame(US_data_CoD)
head(df)

unique(US_data_CoD$cause)
unique(US_data_CoD$cause_id)

#install.packages("dplyr")
library(dplyr)

#Select the data for males and females separately and 
#not select the all-causes because we are executing 
#a cause-of-death analysis
US_CoD_male_2015 <- US_data_CoD %>%
  filter(Period == 2015, cause != "All-causes", 
         Gender == "Male") %>%
  select(Age, cause_id, mxt)

#We have to order the data.frame to facilitate the visualization
US_CoD_male_2015 <- US_CoD_male_2015[order(US_CoD_male_2015$cause_id, 
                                           US_CoD_male_2015$Age), ]

US_CoD_female_2015 <- US_data_CoD %>%
  filter(Period == 2015, cause != "All-causes", 
         Gender == "Female") %>%
  select(Age, cause_id, mxt)

#We have to order the data.frame to facilitate the visualization
US_CoD_female_2015 <- US_CoD_female_2015[order(US_CoD_female_2015$cause_id, 
                                               US_CoD_female_2015$Age), ]


#As before, we use the same function LEdecomp
??LEdecomp::LEdecomp
LE_arri_US_2015 <- LEdecomp(mx1 = US_CoD_male_2015$mxt, 
                            mx2 = US_CoD_female_2015$mxt,
                            age = c(0:100), 
                            method = "arriaga_sym")
plot(LE_arri_US_2015)
LE_arri_US_2015

#install.packages("ggplot2")
library(ggplot2)

CoD_US_decomp_2015 <- data.frame(Age = rep(c(0:100), 18), 
                                 Cause = US_CoD_male_2015$cause_id, 
                                 Difference = LE_arri_US_2015$LEdecomp)
ggplot(CoD_US_decomp_2015, aes(x = Age, y = Difference, fill = Cause)) +
  geom_col() +
  ggplot2::scale_fill_discrete(
    labels = unique(US_CoD_male_2015$cause_id)) +
  labs(title = "Cause-of-death decomposition by age", 
       x = "Age", y = "Contribution") +
  theme_minimal()

#To see the contribution of every cause to the total difference
df_sum <- CoD_US_decomp_2015 %>%
  group_by(Cause) %>%
  summarise(total_diff = sum(Difference, na.rm = TRUE)) %>%
  arrange(desc(total_diff))
df_sum

####################################################################
#Repeat the process for 2020 
US_CoD_male_2020 <- US_data_CoD %>%
  filter(Period == 2020, cause != "All-causes", 
         Gender == "Male") %>%
  select(Age, cause_id, mxt)

#We have to order the data.frame to facilitate the visualization
US_CoD_male_2020 <- US_CoD_male_2020[order(US_CoD_male_2020$cause_id, 
                                           US_CoD_male_2020$Age), ]

US_CoD_female_2020 <- US_data_CoD %>%
  filter(Period == 2020, cause != "All-causes", 
         Gender == "Female") %>%
  select(Age, cause_id, mxt)

#We have to order the data.frame to facilitate the visualization
US_CoD_female_2020 <- US_CoD_female_2020[order(US_CoD_female_2020$cause_id, 
                                               US_CoD_female_2020$Age), ]


#As before, we use the same function LEdecomp
??LEdecomp::LEdecomp
LE_arri_US_2020 <- LEdecomp(mx1 = US_CoD_male_2020$mxt, 
                            mx2 = US_CoD_female_2020$mxt,
                            age = c(0:100), n_causes = 18,
                            method = "arriaga_sym")
LE_arri_US_2020
plot(LE_arri_US_2020)

#install.packages("ggplot2")
library(ggplot2)

CoD_US_decomp_2020 <- data.frame(Age = rep(c(0:100), 18), 
                                 Cause = US_CoD_male_2020$cause_id, 
                                 Difference = LE_arri_US_2020$LEdecomp)
ggplot(CoD_US_decomp_2020, aes(x = Age, y = Difference, fill = Cause)) +
  geom_col() +
  ggplot2::scale_fill_discrete(
    labels = unique(US_CoD_male_2020$cause_id)) +
  labs(title = "Cause-of-death decomposition by age", 
       x = "Age", y = "Age and Cause contribution") +
  theme_minimal()

#To see the contribution of every cause to the total difference
df_sum2 <- CoD_US_decomp_2020 %>%
  group_by(Cause) %>%
  summarise(total_diff = sum(Difference, na.rm = TRUE)) %>%
  arrange(desc(total_diff))
df_sum2

