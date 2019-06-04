library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)

library(ggplot2)
library(readr)

#install.packages("hms")
#library(hms)
library(sp)
library(biogeo)
library(stringr)
library(rgdal)
library(sf)



############################################################################################################################
####################          Day 1   EF_Group_1_d1_filter_start_end_collar               ##################################
#############################################################################################################################

glimpse(EF_Group_1_d1_filter_start_end_collar)


############################################################################################################################
####################          convert lat and longs to x and Y                             ##################################
#############################################################################################################################


#https://spatialreference.org/ref/epsg/gda94-mga-zone-56/
#epsg projection 28356

mapCRS <- CRS("+init=epsg:28356")     # 28356 = GDA_1994_MGA_Zone_56
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

glimpse(EF_Group_1_d1_filter_start_end_collar) 

#start of file has some missing data that has not been filled
EF_Group_1_d1_filter_start_end_collar_1 <-drop_na(EF_Group_1_d1_filter_start_end_collar)
glimpse(EF_Group_1_d1_filter_start_end_collar_1)


coordinates(EF_Group_1_d1_filter_start_end_collar_1) <- ~ lon + lat

proj4string(EF_Group_1_d1_filter_start_end_collar_1) <- wgs84CRS   # assume input lat and longs are WGS84
glimpse(EF_Group_1_d1_filter_start_end_collar_1) 
EF_Group_1_d1_filter_start_end_collar_2 <- spTransform(EF_Group_1_d1_filter_start_end_collar_1, mapCRS)

glimpse(EF_Group_1_d1_filter_start_end_collar_2)

EF_Group_1_d1_filter_start_end_collar = as.data.frame(EF_Group_1_d1_filter_start_end_collar_2) #this has the new coordinates projected !YES!!
glimpse(EF_Group_1_d1_filter_start_end_collar)

EF_Group_1_d1_filter_start_end_collar <- mutate(EF_Group_1_d1_filter_start_end_collar,
                                        POINT_X = lon,  POINT_Y = lat )


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
glimpse(EF_Group_1_d1_filter_start_end_collar) # 1732 data pts for all sheep

#this splits my data frame into lists 
split_sheep <- split(EF_Group_1_d1_filter_start_end_collar, EF_Group_1_d1_filter_start_end_collar$sheep, drop = FALSE)
glimpse(split_sheep) #List of 6

#list of names I want my data frames to be 
sheep_names_EF_Group_1_d1 <- group_by(EF_Group_1_d1_filter_start_end_collar,  name) %>% 
  count()
print(sheep_names_EF_Group_1_d1)


#subet each list in split_sheep to make df

sheep21_collar_d1 <- split_sheep[[1]]
glimpse(sheep21_collar_d1)
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
glimpse(sheep21_collar_d1) # 343 data pts for Sheep_21_collar_d1
Sheep_21_collar_d1_time_step1 <- full_join(df_sec1, sheep21_collar_d1, by = "hms")
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
ggplot(Sheep_21_collar_d1_time_step, aes(hms, lon))+
  geom_point()

############################################################################################################################
####################          make some new variable                                      ##################################
#############################################################################################################################


df <-  mutate(Sheep_21_collar_d1_time_step,
    time_diff = lead(hms) - hms)
glimpse(df_1)
df_1 <- mutate(Sheep_21_collar_d1_time_step,
               x_diff = lead(POINT_X) - POINT_X,
               y_diff = lead(POINT_Y) - POINT_Y)

power(lead(Sheep_21_collar_d1_time_step$POINT_X) - Sheep_21_collar_d1_time_step$POINT_X, lambda =2)


df_2 <- mutate(df_1,
               x_diff_power2 = power(lead(POINT_X) - POINT_X, lambda =2),
               y_diff_power2 = power(lead(POINT_Y) - POINT_Y, lambda = 2))

#ts <- Sheep_21_collar_d1_time_step %>% arrange(hms) %>%
#  mutate(time_diff_2 = as.numeric(hms-lag(hms), units = 'mins'))
#glimpse(ts)

ts1 <- Sheep_21_collar_d1_time_step %>% arrange(hms) %>%
  mutate(distance = power(POINT_X-lag(POINT_X)))
glimpse(ts)


#Sheep_21_collar_d1_time_step = mutate(Sheep_21_collar_d1_time_step,
#                                      Distance = sqrt (Power(x2 - x1)2 + (Power(y2 - y1)2),
#                                       time_lapsed = hms2 - hms1
#                                      Speed_m_per_s = distance /time laspsed
#

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