library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(lubridate)
library(ggplot2)
library(readr)
install.packages("hms")
library(hms)

test<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/EF_Group_3_d1.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_3_d1.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

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
