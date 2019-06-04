library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)

library(ggplot2)
library(readr)

#install.packages("hms")
#library(hms)



#what is the min time? then how to merege this to get a date time value





glimpse(EF_Group_1_d1_filter_start_end_collar)

############################################################################################################################
####################                    Min time for data set                     #########################################
############################################################################################################################

min_time1 <- (min(EF_Group_1_d1_filter_start_end_collar$hms))
print(min_time1) 
print(hms::as.hms(min_time1) )
min_time <- hms::as.hms(min_time1)
print(min_time) 
date_time_start <- as_datetime(paste0("2018-10-29 ", min_time))
############################################################################################################################
####################                    Max time for data set                     #########################################
############################################################################################################################
print(date_time_start)
date_time_end <- date_time_start + hms('4:00:00')
print(date_time_end)

############################################################################################################################
####################                    time step starting at min and ending at max       ##################################
####################                    time step every second                            ##################################
############################################################################################################################

df_sec1 <- data.frame(time_step =seq(from = (date_time_start), to = (date_time_end), by = "sec"))
head(df_sec1)

############################################################################################################################
####################          make df into 2 clms one with date/time and one with time     ##################################
#############################################################################################################################


df_sec1 <- mutate(df_sec1,
                  hms = hms::as.hms(time_step, tz = "UTM"))



############################################################################################################################
####################           slip logged data into df for each sheep                     ##################################
#############################################################################################################################
glimpse(codes_EF_Group_1_d1)
glimpse(glimpse(EF_Group_1_d1_filter_start_end_collar)) # 1732 data pts for all sheep

#this works but gives me a list I want a df
split_sheep <- split(EF_Group_1_d1_filter_start_end_collar, EF_Group_1_d1_filter_start_end_collar$sheep, drop = FALSE)
glimpse(split_sheep)

library(tidyverse)
test_df_split <- map(split_sheep, paste0, collapse = " ") %>% bind_rows() %>% gather(POS, Word)
glimpse(test_df_split)

test_df_split2 <- map(split_sheep, paste0, collapse = " ") 
glimpse(test_df_split2)




############################################################################################################################
####################           merege time step data and logged data                     ##################################
#############################################################################################################################                  

#fill(df, clm, .direction = c("down"))  
#fill(df, clm)
glimpse(df_sec1) #14401 data pts
glimpse(glimpse(EF_Group_1_d1_filter_start_end_collar)) # 1732 data pts for all sheep
test <- full_join(df_sec1, EF_Group_1_d1_filter_start_end_collar, by = "hms")
glimpse(test)

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



### I want to convert long lat to easting and northing