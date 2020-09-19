#=============================================================================================================================================================================================================================
# PREREQUISITES, READING IN DATA SET, AND TIDYING DATA
#=============================================================================================================================================================================================================================

rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(Rmisc) # transform integers to factors, must be first or will mask other packages. We only need multiplot!
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values
library(dplyr)
library(wesanderson)
library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")

#https://www.kaggle.com/joniarroba/noshowappointments

#read the data set into RStudio and store into an object
data <- read.csv("C:/Users/Kevin/Desktop/Medical-Appointments/KaggleV2-May-2016.csv")

ma_data <- as.tibble(data)

# NEED TO REALLY TIDY DATA:
# change formatting of patient id
# ensure the date and time is in correct format for scheduled day and appointment day
# correct neighborhood names
# check what the zeroes and ones mean!

# POtential research and analysis opportunities:
# male vs female with no show rates
# scheduling far in advance cause more no shows?
# age and likelihood of no showing
# scholarship:SMS received, what do they mean and how do they relate to no shows?
# Can use linear regression model again since it is either a no show or a show (binomial)!
