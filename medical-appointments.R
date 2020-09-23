#=============================================================================================================================================================================================================================
# PREREQUISITES, READING IN DATA SET, AND TIDYING DATA
#=============================================================================================================================================================================================================================

# PREREQS:
rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(Rmisc) # transform integers to factors, must be first or will mask other packages. We only need multiplot!
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values
library(dplyr)
library(wesanderson)
library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")

#https://www.kaggle.com/joniarroba/noshowappointments

#READ DATA SET AND STORE INTO OBJECT:
data <- read.csv("C:/Users/Kevin/Desktop/Medical-Appointments/KaggleV2-May-2016.csv")

ma_data <- as.tibble(data)

# TIDY DATA: --DONE--
# change formatting of patient id - won't fit, leave as is
# ensure the date and time is in correct format for scheduled day and appointment day - done!
# correct neighborhood names - not going to use column, so will filter out
# check what the zeroes and ones mean! - 0 means false and 1 means true

# TIDYING DATA SET: fix columns(renaming and class), fixing date-times within columns.
ma_data <- rename(ma_data, Hypertension = Hipertension) #fixed spelling for this column

ma_data$Scholarship <- as.factor(ma_data$Scholarship) #All columns that are factors have been updated as being so!
ma_data$Hypertension <- as.factor(ma_data$Hypertension)
ma_data$Diabetes <- as.factor(ma_data$Diabetes)
ma_data$Alcoholism <- as.factor(ma_data$Alcoholism)
ma_data$Handcap <- as.factor(ma_data$Handcap)
ma_data$SMS_received <- as.factor(ma_data$SMS_received)
ma_data$No.show <- as.factor(ma_data$No.show)

# Fixing the dates, it looks like it is organized as YYYY-MM-DD HMS
ma_data$ScheduledDay <- parse_date_time(ma_data$ScheduledDay, "Ymd HMS")
ma_data$AppointmentDay <- parse_date_time(ma_data$AppointmentDay, "Ymd HMS") #No times only the day of appointment

ma_data <- ma_data %>%
  select(-Neighbourhood)

#=============================================================================================================================================================================================================================
# ANALYSIS & INITIAL BRAINSTORM
#=============================================================================================================================================================================================================================

# POtential research and analysis opportunities: (**) = green light analysis & (xx) = red light (cannot do it)
# ** male vs female with no show rates
# xx morning vs afternoon appointments. which have more no shows? - cannot do since no times for appointments.
# ** age and likelihood of no showing
# ** Can use linear regression model again since it is either a no show or a show (binomial)!
# What does Scholarship mean? - The scholarship is called Bolsa Familia and it is a social welfare program of the Brazilian Government. It provides financial aid to poor families as long as they have children that attend school and are vaccinated.
# If the student is dropped from the school program, by exceed total permitted school absences, funds are suspended. The scholarship whether True (they have it) or FALSE (they don't have it) could affect the ability to afford medical tx which could affect no show rates.
# ** Taking the differences between days and seeing the correlation of no shows based on the length of time between scheduled and appointment day!

#Start with glimpse() and see what you are working with! - start making it into a habit

glimpse(ma_data) # taking a glimpse of our data, the classes and columns are accurate

#Pearson's correlation would not be good for this data since the variables are not continuous integers and are factors.
# PCA would not be used either for the same reason.
# We can dive into analyzing it by variable and seeing how it relates to no.shows. No meaning they showed up and yes meaning they did not show
# see which variables are good predictors of no shows. (Regression model?)
# 1. Gender, 2. Appointment difference, 3. age, 4. scholarship:SMS received - create function to run analysis and plots and multiplot them together.

#=============================================================================================================================================================================================================================
# 1. GENDER
#=============================================================================================================================================================================================================================

# Let's run a bar plot and show the number of males vs females that made an appointment and within that a stack of who showed and did not show.
gender_stack <- ggplot(ma_data, aes(x = Gender, fill = No.show)) +
  geom_bar(position = "stack") +
  theme_dark() +
  labs(x = "Gender", 
       y = "Count",
       title = "More Female Appointments than Male Appointments",
       subtitle = "F = Female & M = Male",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

gender_fill <- ggplot(ma_data, aes(x = Gender, fill = No.show)) +
geom_bar(position = "fill") +
  theme_dark() +
  labs(x = "Gender", 
       y = "Rate",
       title = "No Show Rates are the Same Despite Gender",
       subtitle = "F = Female & M = Male",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

multiplot(gender_stack, gender_fill, cols = 2)
# We can see that the majority of appointments are made by females.

ma_data %>%
  group_by(Gender, No.show) %>%
  summarize(count = n())
# Within this data set there are 71,840 female appointments and 38,687 male appointments.
# Looking at the proportions of who does and does not show up, both female and males are even at 20%.
# Therefore, gender is not a good predictor of no show rates as there is no significant difference between the two.

#=============================================================================================================================================================================================================================
# 2. APPOINTMENT DIFFERENCES - NO SHOW RATE - HISTOGRAM - NUMBER OF DAYS PASSED VS NUMBER OF NO SHOWS
#=============================================================================================================================================================================================================================

# Let's take the difference between the dates and see if there is a correlation between the more days that pass the higher the rate of no shows.

date_data <- ma_data %>%
  select(ScheduledDay, AppointmentDay, No.show) %>%
  mutate(diffdays = abs(difftime(AppointmentDay, ScheduledDay, units = "days"))) # took the difference between the two dates to see how long it has been since they set the appointment and the actual appointment day. Absolute value was taken since you can't have an appointment before scheduling one.

ggplot(date_data, aes(x = diffdays, fill = No.show)) +
  geom_histogram() +
  theme_dark() +
  labs(x = "# of Days Appointment was Scheduled in Advance", 
       y = "Count",
       title = "Most Appointments Scheduled for the Same Day",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# This graph shows us that the majority of the appointments are set on the same day (roughly zero days in between)
# Let's zoom in and see where most of the no shows are!

ggplot(date_data, aes(x = diffdays, fill = No.show)) +
  geom_histogram() +
  theme_dark() +
  coord_cartesian(xlim = c(0, 180), ylim = c(0, 6000)) +
  labs(x = "# of Days Appointment was Scheduled in Advance", 
       y = "Count",
       title = "Same Day Appointments Have High No Show Counts",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# At first glance, it seems that in this zoomed graph, the majority of our no shows happen when a patient schedules on the same day or for the next 1-2 days.
# and as patients schedule for appointments further in advance, the no shows start to decline.

ggplot(date_data, aes(x = diffdays, fill = No.show)) +
  geom_histogram(position = "fill") +
  theme_dark() +
  labs(x = "# of Days Appointment was Scheduled in Advance", 
       y = "Rate",
       title = "No Show Rates are Lower for Same Day Appointments",
       subtitle = "Higher Likelihood of No Show after 4 Months of Advance Scheduling",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# However, after looking at the proportions, we see something entirely different.
# We see that the no show rates are relatively low if you schedule appointments and have your appointments within 120 days (4 months).
# Within 4 months the no show rates are less than 40%. 
# In contrast, those that have appointments set for far in the future (greater than 120 days), there is a huge spike in those who no show. Which is greater than 50%

# Therefore, when analyzing appointment and the actual appointment date in regards to no shows. There is a higher liklihood of individuals to no show if they set an appointment greater than 4 months in advance
# Also that most appointments set (within this data set) is scheduled for the same day.

#=============================================================================================================================================================================================================================
# 3. AGE AND NO SHOWS
#=============================================================================================================================================================================================================================

# Is there correlation between your age and the rate you show up to your appointments? Or is there a particular age group that is known not to show up to their scheduled appointments?
# Are younger patients less dependable than older patients?

# histogram for age and noshows - similar plotting as date_data
# The Age variable is measured in years, those that are less than 1 year old will fall within the zero bin

ggplot(ma_data, aes(x = Age, fill = No.show)) +
  geom_histogram() +
  theme_dark() +
  labs(x = "Age", 
       y = "Count",
       title = "Most Appointments are Scheduled for Children and Adults",
       subtitle = "Children Age Range: 0-5 years old & Adults Age Range: 30-60 years old",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# Within this portion of the analysis, most patients that are going for their appointment is less than 1 year old. Most likely, accompanied by their parent/gaurdian and between the age of 30-60 years old.

ggplot(ma_data, aes(x = Age, fill = No.show)) +
  geom_histogram(bins = 100) +
  theme_dark() +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 800)) +
  labs(x = "Age", 
       y = "Count",
       title = "No Shows are Most Apparent in Adults",
       subtitle = "Adults in the Age Range: 20-30 years old",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# After zooming into the plot and increasing the number of bins to represent 1 year per bin.
# We see that no shows begin to dip between the ages of zero years to 12 years old (most likely because parents are taking their children to their doctor's appointment)
# however, the no shows begin to spike and maintain in the range of 20-30 years of age. (of age to be responsible for your own doctor's appointments)
# However, beyond 30 years of age, there is a negative trend of no shows as age progresses.

ggplot(ma_data, aes(x = Age, fill = No.show)) +
  geom_histogram(bins = 100, position = "fill") +
  theme_dark() +
  labs(x = "Age", 
       y = "Rate",
       title = "No Shows are Most Apparent in Adults",
       subtitle = "Adults in the Age Range: 20-30 years old",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# We see a similar trend as above, but in regards to proportions.
# There is however a spike of noshows after the age of 90. There could be reasons for this as well (maybe appointments were set in advance for these older patient but passed before they could attend appointment)

age_date_data <- ma_data %>%
  select(ScheduledDay, AppointmentDay, Age, No.show) %>%
  mutate(diffdays = abs(difftime(AppointmentDay, ScheduledDay, units = "days")))

ggplot(age_date_data, aes(x = diffdays, y = Age, color = No.show)) +
  geom_point(position = "jitter") +
  theme_dark() +
  coord_cartesian(ylim = c(90, 120)) +
  labs(x = "# of Days Appointment was Scheduled in Advance", 
       y = "Age",
       title = "Patients Above the Age of 90",
       subtitle = "Mainly will Schedule Appointments 0-50 days in Advance",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# Can include, but this shows us that 90+ year old patients will schedule appointments between 0-50 days in advance.

ggplot(age_date_data, aes(x = diffdays, y = Age, color = No.show)) +
  geom_point(position = "jitter") +
  theme_dark() +
  coord_cartesian(xlim = c(0, 40), ylim = c(90, 120)) +
  labs(x = "# of Days Appointment was Scheduled in Advance", 
       y = "Age",
       title = "Patients Above the Age of 90",
       subtitle = "Mainly will Schedule Appointments 0-50 days in Advance",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# Here is a zoomed in version of the above scatter plot to visualize appointments set in 0-50 days in advance

#=============================================================================================================================================================================================================================
# 4. BINARY VARIABLES - BAR CHARTS AND MULTIPLOT - WRITE FUNCTION FOR ALL BAR PLOTS
#=============================================================================================================================================================================================================================

# use this section to practice writing a function so you're not copying and pasting the plots
# is this possible? writing a function for creating plots?


# COUNTS FUNTION

binary_plot <- function(df, vect){
  ggplot(data, aes(x = vect, fill = No.show)) +
    geom_bar() +
    theme_dark() +
    scale_x_discrete(limits = c("0", "1"),
                     labels = c("Doesn't\nHave", "Has"))
} # function to make plots for the desired vector within our data set

scholar_plot <- binary_plot(ma_data, ma_data$Scholarship) +
  labs(x = "Scholarship", 
       y = "Count",
       title = "Scholarship and No Shows",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

hypertension_plot <- binary_plot(ma_data, ma_data$Hypertension)+
  labs(x = "Hypertension", 
       y = "Count",
       title = "Hypertension and No Shows",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

diabetes_plot <- binary_plot(ma_data, ma_data$Diabetes)+
  labs(x = "Diabetes", 
       y = "Count",
       title = "Diabetes and No Shows",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

alcoholism_plot <- binary_plot(ma_data, ma_data$Alcoholism)+
  labs(x = "Alcoholism", 
       y = "Count",
       title = "Alcoholism and No Shows",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

handcap_plot <- binary_plot(ma_data, ma_data$Handcap)+
  labs(x = "Handicap Disability", 
       y = "Count",
       title = "Handicap and No Shows",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

sms_plot <- binary_plot(ma_data, ma_data$SMS_received)+
  labs(x = "SMS Message", 
       y = "Count",
       title = "SMS Message and No Shows",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

multiplot(scholar_plot, hypertension_plot, diabetes_plot, alcoholism_plot, handcap_plot, sms_plot, cols = 2)

# PROPORTIONS FUNCTION

binary_plot2 <- function(df, vect){
  ggplot(data, aes(x = vect, fill = No.show)) +
    geom_bar(position = "fill") +
    theme_dark() +
    scale_x_discrete(limits = c("0", "1"),
                     labels = c("Doesn't\nHave", "Has"))
}

scholar_plot2 <- binary_plot2(ma_data, ma_data$Scholarship)+
  labs(x = "Scholarship", 
       y = "Rate",
       title = "Scholarship Not Effective for No Shows",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

hypternsion_plot2 <- binary_plot2(ma_data, ma_data$Hypertension)+
  labs(x = "Hypertension", 
       y = "Rate",
       title = "Hypertensive Pts have Slightly Lower No Show",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

diabetes_plot2 <- binary_plot2(ma_data, ma_data$Diabetes) +
  labs(x = "Diabetes", 
       y = "Rate",
       title = "Diabetic Pts have Slightly Lower No Show",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

alcoholism_plot2 <- binary_plot2(ma_data, ma_data$Alcoholism) +
  labs(x = "Alcoholism", 
       y = "Rate",
       title = "Alcoholism Not a Good Indicator for No Show",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

handcap_plot2 <- binary_plot2(ma_data, ma_data$Handcap) +
  labs(x = "Handicap Disability", 
       y = "Rate",
       title = "Handicap Pts have Slightly Lower No Show",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

sms_plot2 <- binary_plot2(ma_data, ma_data$SMS_received)+
  labs(x = "SMS Message", 
       y = "Rate",
       title = "SMS Messages Do Not Improve No Shows",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

multiplot(scholar_plot2, hypternsion_plot2, diabetes_plot2, alcoholism_plot2, handcap_plot2, sms_plot2, cols = 2)

# Looking at the proportions of patients with or without the binary variables, we can see if there are trends that we can observe.
# As seen in Scholarships and SMS messages sent to the patient, it does not improve no show rates. Patients with children who have financial aid via scholarships have a higher rate of no shows than those without it. Also, patients who opted to receive sms message reminders of appointments had a higher no show rate.
# Patients with and without alcoholism show that there is an equal no show rate (so whether or not a person is an alcoholic does not indicate showing up or no showing to an appointment)
# Lastly, patients with preconceived health conditions such as hypertension and diabetes, and disabilities such as being handicapped, show to have lower no show rates.




























