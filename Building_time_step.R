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

#remove the missing data rows 
colSums(is.na(Sheep_21_collar_d1_time_step)) #4 missing records
Sheep_21_collar_d1_time_step <- filter(Sheep_21_collar_d1_time_step, POINT_X > 0)

############################################################################################################################
####################          make some new variable                                      ##################################
#############################################################################################################################

2^2
sqrt(-4)


Sheep_21_collar_d1_time_step <- mutate(Sheep_21_collar_d1_time_step,
                                       x_diff_power2 = ((lead(POINT_X) - POINT_X)^2),
                                       y_diff_power2 = ((lead(POINT_Y) - POINT_Y)^2))

Sheep_21_collar_d1_time_step <- mutate(Sheep_21_collar_d1_time_step,
                                       distance = sqrt((x_diff_power2+y_diff_power2)),
                                       time_lapased =  (lead(time_step) - time_step))

#Not sure I need this the animals arent moving much
Sheep_21_collar_d1_time_step1 <- mutate(Sheep_21_collar_d1_time_step,
                                        speed_class = case_when(
                                          distance < 0.09 ~"stationary" ,
                                          distance < 2.2 | distance > 0.09 ~"medium",
                                          distance > 2.2 ~"fast",
                                          TRUE ~ "no_class"))


############################################################################################################################
####################          check my data I have created                                ##################################
############################################################################################################################# 

dim(Sheep_21_collar_d1_time_step)
dim(Sheep_22_collar_d1_time_step)

glimpse(Sheep_21_collar_d1_time_step)

#distance covered
ggplot(Sheep_21_collar_d1_time_step, aes(hms, distance))+       
  geom_point()
#logged movement only and time of day
filter(Sheep_21_collar_d1_time_step,distance != 0) %>% 
  ggplot( aes(hms, distance))+       
    geom_point()


#distance covered
ggplot(Sheep_21_collar_d1_time_step1, aes(hms, speed_class))+       
  geom_point()


############################################################################################################################
####################          export my data I have created                                ##################################
############################################################################################################################# 


write_csv(Sheep_21_collar_d1_time_step, "C:/Users/ouz001/working_from_home/sheep_data/Sheep_21_collar_d1_time_step.csv")

