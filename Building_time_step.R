library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(lubridate)
library(ggplot2)
library(readr)
install.packages("hms")
library(hms)
library(readxl)
test <- read_excel("C:/Users/ouz001/pasture_utilisation/Take2/week1/start_end/EF_Group_1_d1_filter_start_end_collar_R_GDA.xls")


#what is the min time?


min_time_EF_group1_d1 <- (min(test$hms))
print(min_time_EF_group1_d1)

#create a new Df with start time of min_time_EF_group1_d1 then every 1min for 4 hours
#How many seconds in 4hours
4*60*60
max_time_EF_group1_d1 <- hms(min_time_EF_group1_d1) + hms('4:00:00')
print(max_time_EF_group1_d1)
as.hms
time_step <- data.frame(hms_1sec = as.hms(c("09:15:02"),("09:15:03")))

time_step1 <- as.data.frame(x, row.names = NULL, optional = FALSE, ...,
              nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " "))
https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/as.data.frame

df <- data.frame(hms_1sec=as.hms("01/01/2000", format="%m/%d/%Y"), 
                 File="", User="", stringsAsFactors=FALSE)

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
