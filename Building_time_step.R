library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)

library(ggplot2)
library(readr)

#install.packages("hms")
#library(hms)



#what is the min time? then how to merege this to get a date time value





#date_time_start_IS <- ISOdatetime(date_time_start)

#min_date_time_EF_group1_d1 <- ISOdatetime(paste0("2018-10-29 ", min_time_EF_group1_d1))
#min_date_time_EF_group1_d1 <- ISOdatetime(paste0("2018-10-29 ","09:15:02"))
#print(min_date_time_EF_group1_d1)

#create a new Df with start time of min_time_EF_group1_d1 then every 1min for 4 hours
#How many seconds in 4hours
#4*60*60
#max_time_EF_group1_d1 <- hms('09:15:02') + hms('4:00:00')
#max_time_EF_group1_d1 <- hms(min_time_EF_group1_d1) + hms('4:00:00')
#print(max_time_EF_group1_d1)

#Working out how to make df with regular time step
#test_df1 <- data.frame(time_step = dmy_hms ("29/10/18 09:15:02","29/10/18 09:15:03", "29/10/18 09:15:04"))
#test_df2 <- data.frame(time_step = dmy_hms (seq(from = "29/10/18 09:15:02", to = "29/10/18 09:15:04")))
#test_df3 <- seq(from = as.Date("29/10/18 09:15:02 ", "dmy_hms"), to = as.Date("29/10/18 10:15:02 ", "dmy_hms"), by = "hour")
#test_df4 <- data.frame(time_step = (seq(c(ISOdate(2018,10,29)), by = "min", length.out = 10)))
#test_df_min <- data.frame(time_step = (seq(c(ISOdate(2018,10,29, 09,15,02)), by = "min", length.out = 12)))
#test_df_sec <- data.frame(time_step = (seq(c(ISOdate(2018,10,29, 09,15,02)), by = "sec", length.out = 12)))
#test_df_sec1 <- data.frame(time_step =seq(from = (ISOdate(2018,10,29, 09,15,02)), to = (ISOdate(2018,10,29, 09,15,35)), by = "sec"))
#(ISOdate(2018,10,29, min_time_EF_group1_d1))
#test_df_sec1 <- data.frame(time_step =seq(from = (date_time_start), to = (ISOdate(2018,10,29, 09,15,35)), by = "sec"))



test <- read_excel("C:/Users/ouz001/pasture_utilisation/Take2/week1/start_end/EF_Group_1_d1_filter_start_end_collar_R_GDA.xls")



min_time <- (min(test$hms))
print(min_time)
date_time_start <- as_datetime(paste0("2018-10-29 ", min_time))
print(date_time_start)
date_time_end <- date_time_start + hms('4:00:00')
print(date_time_end)
df_sec1 <- data.frame(time_step =seq(from = (date_time_start), to = (date_time_end), by = "sec"))
head(df_sec1)

#from this make a clm that just has time

df_sec1 <- mutate(df_sec1,
                  hms = hms::as.hms(time_step, tz = "UTM"))


test <- mutate(test, time = as_datetime(time),
               hms = as.hms(time))
                  
##Now merege time step data and captured data
#fill(df, clm, .direction = c("down"))  
#fill(df, clm)
glimpse(df_sec1)
glimpse(test) # need to format as character





time_step_EF_Group_1_d1 <- (left_join(df_sec1,test, by = "hms"))

##This has sort of worked but I have multiple entries for the same second
#need to filter out data per sheep 
# check out naming of Sheep 24_collar_day1 and Sheep 21_collar_d1 should be closer in name
















###               1) Plot data to chcekThis is a very important step to make sure I am not including too much data          ######
ggplot(test, aes(time, hms))+
  geom_point()
#looks like some data was lefted on the loggers? - or something else?
#remove the 8th and find out what the max and min time is....

#####            2) filter out missing data    ###########

test_11_12nov <- filter(test, day != 8) 
#according to the notes EFGroup3d1.csv was captured on the 12th Nov with start time of 08:03

#######            min and max times of my data - not they are in wrong zone here #############
max_time_12nov <- as_datetime(max(test_12nov$time))
min_time_12nov <- as_datetime(min(test_12nov$time))
#######           create df with min and max time #######################

glimpse(max_time_12nov)
range_time12nov <- data.frame(file = "EF_Group_3_d1.csv",
                              min_date_time =(min_time_12nov),
                              max_date_time =(max_time_12nov),
                              min_time = hms::as.hms(min_time_12nov),
                              max_time = hms::as.hms(max_time_12nov))
glimpse(range_time12nov)


#### make a data frame for time sequence using min and max time####

seconds_in_range <- range_time12nov$max_time - range_time12nov$min_time

######## This is my time step ########
#I have used a start and stop time created from my data files - not sure i need this step
time_step <- data.frame(seconds = range_time12nov$min_time:range_time12nov$max_time)
time_step$seconds <- hms(seconds = time_step$seconds)
glimpse(time_step)


###Join my time step data to the sheep data


###JOBS to do ####

#use the correct data with just hh or whatever i am ment to do doing
#join time step and correct sheep data 
# should be ok to use left_join() remember x = time step and y = sheep
#Try these to fill in missing values - perhaps use mutate to chcek
#fill(df, clm, .direction = c("down"))  
#fill(df, clm)
