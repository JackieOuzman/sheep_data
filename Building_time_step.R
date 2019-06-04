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
#####################                    Min time for data set                     #########################################
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
####################           split logged data into df for each sheep                     ##################################
#############################################################################################################################
glimpse(codes_EF_Group_1_d1)
glimpse(glimpse(EF_Group_1_d1_filter_start_end_collar)) # 1732 data pts for all sheep

#this splits my data frame into lists 
split_sheep <- split(EF_Group_1_d1_filter_start_end_collar, EF_Group_1_d1_filter_start_end_collar$sheep, drop = FALSE)
glimpse(split_sheep) #List of 6

#list of names I want my data frames to be 
sheep_names_EF_Group_1_d1 <- group_by(EF_Group_1_d1_filter_start_end_collar,  name) %>% 
  count()
print(sheep_names_EF_Group_1_d1)


#subet each list in split_sheep to make df

sheep21_collar_d1 <- split_sheep[[1]]
glimpse(Sheep_21_collar_d1)
Sheep22_collar_d1 <- split_sheep[[2]]  
Sheep23_collar_d1 <- split_sheep[[3]]  
Sheep24_collar_d1 <- split_sheep[[4]]
Sheep25_collar_d1 <- split_sheep[[5]]
Sheep26_collar_d1 <- split_sheep[[6]]

#glimpse(Sheep_21_collar_d1)
glimpse(Sheep22_collar_d1)
glimpse(Sheep23_collar_d1)
#glimpse(Sheep24_collar_d1)
#glimpse(Sheep25_collar_d1)
#glimpse(Sheep26_collar_d1)


############################################################################################################################
####################           merege time step data and logged data                     ##################################
#############################################################################################################################                  

#step 1 join
glimpse(df_sec1) #14401 data pts
glimpse(glimpse(Sheep_21_collar_d1)) # 343 data pts for Sheep_21_collar_d1
Sheep_21_collar_d1_time_step1 <- full_join(df_sec1, Sheep_21_collar_d1, by = "hms")
glimpse(Sheep_21_collar_d1_time_step1) #14401 data pts

#step 2 fill in missing values 
Sheep_21_collar_d1_time_step <- fill(Sheep_21_collar_d1_time_step1,  everything())
#data enetry has a problem d1 should be day1
Sheep_21_collar_d1_time_step <- mutate(Sheep_21_collar_d1_time_step, name = "Sheep 21_collar_day1")

#now for the rest of the sheep in day1 

Sheep_22_collar_d1_time_step1 <- full_join(df_sec1, Sheep22_collar_d1, by = "hms")
Sheep_23_collar_d1_time_step1 <- full_join(df_sec1, Sheep23_collar_d1, by = "hms")
Sheep_24_collar_d1_time_step1 <- full_join(df_sec1, Sheep24_collar_d1, by = "hms")
Sheep_25_collar_d1_time_step1 <- full_join(df_sec1, Sheep25_collar_d1, by = "hms")
Sheep_26_collar_d1_time_step1 <- full_join(df_sec1, Sheep26_collar_d1, by = "hms")


Sheep_22_collar_d1_time_step <- fill(Sheep_22_collar_d1_time_step1,  everything())
Sheep_23_collar_d1_time_step <- fill(Sheep_23_collar_d1_time_step1,  everything())
Sheep_24_collar_d1_time_step <- fill(Sheep_24_collar_d1_time_step1,  everything())
Sheep_25_collar_d1_time_step <- fill(Sheep_25_collar_d1_time_step1,  everything())
Sheep_26_collar_d1_time_step <- fill(Sheep_26_collar_d1_time_step1,  everything())

############################################################################################################################
####################          check my data I have created                                ##################################
############################################################################################################################# 

dim(Sheep_21_collar_d1_time_step)
dim(Sheep_22_collar_d1_time_step)
ggplot(Sheep_22_collar_d1_time_step, aes(hms, lon))+
  geom_point()







###               1) Plot data to chcekThis is a very important step to make sure I am not including too much data          ######
ggplot(test, aes(time, hms))+
  geom_point()
#looks like some data was lefted on the loggers? - or something else?
#remove the 8th and find out what the max and min time is....




###JOBS to do ####

#use the correct data with just hh or whatever i am ment to do doing
#join time step and correct sheep data 
# should be ok to use left_join() remember x = time step and y = sheep
#Try these to fill in missing values - perhaps use mutate to chcek
#fill(df, clm, .direction = c("down"))  
#fill(df, clm)



### I want to convert long lat to easting and northing