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
# change formatting of patient id - won't fit, leave as is
# ensure the date and time is in correct format for scheduled day and appointment day - done!
# correct neighborhood names - not going to use column, so will filter out
# check what the zeroes and ones mean! - 0 means false and 1 means true
# What does Scholarship mean? in the data set

# POtential research and analysis opportunities:
# male vs female with no show rates
# scheduling far in advance cause more no shows?
# morning vs afternoon appointments. which have more no shows? - cannot do since no times for appointments.
# age and likelihood of no showing
# scholarship:SMS received, what do they mean and how do they relate to no shows?
# Can use linear regression model again since it is either a no show or a show (binomial)!

# TIDYING DATA SET: fix columns(renaming and class), fixing date-times within columns.
ma_data <- rename(ma_data, Hypertension = Hipertension) #fixed spelling for this column

ma_data$Scholarship <- as.factor(ma_data$Scholarship) #All columns that are factors have been updated as being so!
ma_data$Hypertension <- as.factor(ma_data$Hypertension)
ma_data$Diabetes <- as.factor(ma_data$Diabetes)
ma_data$Alcoholism <- as.factor(ma_data$Alcoholism)
ma_data$Handcap <- as.factor(ma_data$Handcap)
ma_data$SMS_received <- as.factor(ma_data$SMS_received)
ma_data$No.show <- as.factor(ma_data$No.show)

# Fixing the dates, it looks like it is organized as YYYY-MM-DD what is T and what is Z?
ma_data$ScheduledDay <- parse_date_time(ma_data$ScheduledDay, "Ymd HMS")
ma_data$AppointmentDay <- parse_date_time(ma_data$AppointmentDay, "Ymd HMS") #No times only the day of appointment




