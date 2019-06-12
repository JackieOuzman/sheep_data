library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(lubridate)
library(ggplot2)
library(readr)
library(sp)
library(biogeo)
library(stringr)
library(rgdal)
library(sf)
####################################################################################################################
###################       week 2                                      ##############################################


#setwd("c:/Users/ouz001/pasture_utilisation/Take2")


#Bring in csv files with new format

#getting the codes out of the raw data file
#Danila has changed something and the files won't load now has white spaces sperated need to use read_table2!

####################################################################################################################
###################       Create a df with the sheep and tracking ID ##############################################
###################                   DAY1 EF sheep codes            ##############################################
####################################################################################################################

codes_EF_Group_2_d1<- read_csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/EF_Group_2_d1.csv",skip=34)
codes_EF_Group_2_d1 <- codes_EF_Group_2_d1[1:12, 1:2]
codes_EF_Group_2_d1 <- mutate(codes_EF_Group_2_d1, trksegID = as.integer(ID))
glimpse(codes_EF_Group_2_d1)

codes_EF_Group_2_d2 <- read.csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/EF_Group_2_d2.csv", skip=34)
codes_EF_Group_2_d2 <- codes_EF_Group_2_d2[1:12, 1:2]
codes_EF_Group_2_d2 <- mutate(codes_EF_Group_2_d2, trksegID1 = as.character(ID))
codes_EF_Group_2_d2 <- mutate(codes_EF_Group_2_d2, trksegID = as.integer(trksegID1))

codes_EF_Group_2_d3 <- read.csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/EF_Group_2_d3.csv", skip=34)
codes_EF_Group_2_d3 <- codes_EF_Group_2_d3[1:12,1:2]
codes_EF_Group_2_d3 <- mutate(codes_EF_Group_2_d3, trksegID = as.integer(ID))
codes_EF_Group_2_d3 <- mutate(codes_EF_Group_2_d3, trksegID1 = as.character(ID))
codes_EF_Group_2_d3 <- mutate(codes_EF_Group_2_d3, trksegID = as.integer(trksegID1))


codes_EF_Group_2_d4 <- read.csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/EF_Group_2_d4.csv", skip=34)
codes_EF_Group_2_d4 <- codes_EF_Group_2_d4[1:12,1:2]
codes_EF_Group_2_d4 <- mutate(codes_EF_Group_2_d4, trksegID1 = as.character(ID))
codes_EF_Group_2_d4 <- mutate(codes_EF_Group_2_d4, trksegID = as.integer(trksegID1))

####################################################################################################################
###################                   DAY1 VF sheep codes            ##############################################
####################################################################################################################

codes_VF_Group_2_d1<- read_csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/VF_Group_2_d1.csv", skip=34)
codes_VF_Group_2_d1 <- codes_VF_Group_2_d1[1:12, 1:2]
codes_VF_Group_2_d1 <- mutate(codes_VF_Group_2_d1, trksegID1 = as.character(ID))
codes_VF_Group_2_d1 <- mutate(codes_VF_Group_2_d1, trksegID = as.integer(trksegID1))

codes_VF_Group_2_d2 <- read.csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/VF_Group_2_d2.csv", skip=34)
codes_VF_Group_2_d2 <- codes_VF_Group_2_d2[1:12, 1:2]
codes_VF_Group_2_d2 <- mutate(codes_VF_Group_2_d2, trksegID1 = as.character(ID))
codes_VF_Group_2_d2 <- mutate(codes_VF_Group_2_d2, trksegID = as.integer(trksegID1))

codes_VF_Group_2_d3 <- read.csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/VF_Group_2_d3.csv", skip=34)
codes_VF_Group_2_d3 <- codes_VF_Group_2_d3[1:12,1:2]
codes_VF_Group_2_d3 <- mutate(codes_VF_Group_2_d3, trksegID1 = as.character(ID))
codes_VF_Group_2_d3 <- mutate(codes_VF_Group_2_d3, trksegID = as.integer(trksegID1))

codes_VF_Group_2_d4 <- read.csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/VF_Group_2_d4.csv", skip=34)
codes_VF_Group_2_d4 <- codes_VF_Group_2_d4[1:12,1:2]
codes_VF_Group_2_d4 <- mutate(codes_VF_Group_2_d4, trksegID1 = as.character(ID))
codes_VF_Group_2_d4 <- mutate(codes_VF_Group_2_d4, trksegID = as.integer(trksegID1))


####################################################################################################################
###################       Create a df with the logged data           ##############################################
###################################################################################################################
###################################################################################################################
###################                   DAY1 EF Group 2 - week 1          ###########################################
####################################################################################################################

##################             Step 1 bring in the raw logged data   ##############################################
EF_Group_2_d1<- read_csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/EF_Group_2_d1.csv", skip=64) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_2_d1.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))


##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################
glimpse(EF_Group_2_d1) #40,515
glimpse(codes_EF_Group_2_d1) #12


EF_Group_2_d1 <- left_join(EF_Group_2_d1, codes_EF_Group_2_d1, by= "trksegID") %>% 
  separate(name,into =  c("temp1","sheep", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -temp1)
glimpse(EF_Group_2_d1) #40,515



##################        Step 3 filter data based on time                          ###################################

EF_Group_2_d1_filter_start <- filter(EF_Group_2_d1, hms > hms('09:01:00'))
end_timeEF_Group_2_d1_filter_start <- hms('09:01:00') + hms('04:00:00')
EF_Group_2_d1_filter_start_end <- filter(EF_Group_2_d1_filter_start, hms < (end_timeEF_Group_2_d1_filter_start))
glimpse(EF_Group_2_d1_filter_start_end)

glimpse(EF_Group_2_d1_filter_start)

##################        Step 4 filter data based on collar (not handheld)        ###################################

EF_Group_2_d1_filter_start_end_collar <- filter(EF_Group_2_d1_filter_start_end, type == "collar")

glimpse(EF_Group_2_d1_filter_start_end_collar)


####################          step 5 convert lat and longs to x and Y               ##################################



#https://spatialreference.org/ref/epsg/gda94-mga-zone-56/
#epsg projection 28356

mapCRS <- CRS("+init=epsg:28356")     # 28356 = GDA_1994_MGA_Zone_56
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs


coordinates(EF_Group_2_d1_filter_start_end_collar) <- ~ lon + lat
proj4string(EF_Group_2_d1_filter_start_end_collar) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
EF_Group_2_d1_filter_start_end_collar_1 <- spTransform(EF_Group_2_d1_filter_start_end_collar, mapCRS)
#make new df_1
EF_Group_2_d1_filter_start_end_collar = as.data.frame(EF_Group_2_d1_filter_start_end_collar_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
EF_Group_2_d1_filter_start_end_collar <- mutate(EF_Group_2_d1_filter_start_end_collar,
                                                POINT_X = lon,  POINT_Y = lat )

####################          step 6 export data                                   ##################################
write_csv(EF_Group_2_d1_filter_start_end_collar, 
          "W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_2_d1_filter_start_end_collar.csv") 




###################################################################################################################
###################                   DAY2 EF Group 2 - week 1          ###########################################  
##################################################################################################################         

##################             Step 1 bring in the raw logged data   ##############################################

EF_Group_2_d2<- read_csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/EF_Group_2_d2.csv", skip=64) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_2_d2.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################

glimpse(EF_Group_2_d2) #
glimpse(codes_EF_Group_2_d2)
EF_Group_2_d2 <- left_join(EF_Group_2_d2, codes_EF_Group_2_d2, by= "trksegID") %>% 
  separate(name,into =  c("temp1", "sheep", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -temp1,
         -trksegID1)


##################        Step 3 filter data based on time                          ###################################
EF_Group_2_d2_filter_start <- filter(EF_Group_2_d2, hms > hms('08:35:00'))
end_timeEF_Group_2_d2 <- hms('08:35:00') + hms('04:00:00')
EF_Group_2_d2_filter_start_end <- filter(EF_Group_2_d2_filter_start, hms < end_timeEF_Group_2_d2)



glimpse(EF_Group_2_d2_filter_start_end) #28,484

##################        Step 4 filter data based on collar (not handheld)        ###################################

EF_Group_2_d2_filter_start_end_collar <- filter(EF_Group_2_d2_filter_start_end, type == "collar")



####################          step 5 convert lat and longs to x and Y               ##################################

glimpse(EF_Group_2_d2_filter_start_end_collar)#880

coordinates(EF_Group_2_d2_filter_start_end_collar) <- ~ lon + lat
proj4string(EF_Group_2_d2_filter_start_end_collar) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
EF_Group_2_d2_filter_start_end_collar_1 <- spTransform(EF_Group_2_d2_filter_start_end_collar, mapCRS)
#make new df_1
EF_Group_2_d2_filter_start_end_collar = as.data.frame(EF_Group_2_d2_filter_start_end_collar_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
EF_Group_2_d2_filter_start_end_collar <- mutate(EF_Group_2_d2_filter_start_end_collar,
                                                POINT_X = lon,  POINT_Y = lat )

####################          step 6 export data                                   ##################################
write_csv(EF_Group_2_d2_filter_start_end_collar, 
          "W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_2_d2_filter_start_end_collar.csv") 


###################################################################################################################
###################                   DAY3 EF Group 1 - week 1          ###########################################  
################################################################################################################## 
      

##################             Step 1 bring in the raw logged data   ##############################################

EF_Group_2_d3<- read_csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/EF_Group_2_d3.csv", skip=64) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_2_d3.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))
##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################


glimpse(EF_Group_2_d3)#39,702
glimpse(codes_EF_Group_2_d3)
EF_Group_2_d3 <- left_join(EF_Group_2_d3, codes_EF_Group_2_d3, by= "trksegID") %>% 
  separate(name,into =  c("temp1", "sheep", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -temp1,
         -trksegID1)  
##################        Step 3 filter data based on time                          ###################################

EF_Group_2_d3_filter_start <- filter(EF_Group_2_d3, hms > hms('09:04:00'))
end_timeEF_Group_2_d3 <- hms('09:04:00') + hms('04:00:00')
EF_Group_2_d3_filter_start_end <- filter(EF_Group_2_d3_filter_start, hms < end_timeEF_Group_2_d3)


glimpse(EF_Group_2_d3_filter_start_end)
##################        Step 4 filter data based on collar (not handheld)        ###################################
EF_Group_2_d3_filter_start_end_collar <- filter(EF_Group_2_d3_filter_start_end, type == "collar")

glimpse(EF_Group_2_d3_filter_start_end_collar)

####################          step 5 convert lat and longs to x and Y               ##################################

glimpse(EF_Group_2_d3_filter_start_end_collar)

coordinates(EF_Group_2_d3_filter_start_end_collar) <- ~ lon + lat
proj4string(EF_Group_2_d3_filter_start_end_collar) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
EF_Group_2_d3_filter_start_end_collar_1 <- spTransform(EF_Group_2_d3_filter_start_end_collar, mapCRS)
#make new df_1
EF_Group_2_d3_filter_start_end_collar = as.data.frame(EF_Group_2_d3_filter_start_end_collar_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
EF_Group_2_d3_filter_start_end_collar <- mutate(EF_Group_2_d3_filter_start_end_collar,
                                                POINT_X = lon,  POINT_Y = lat )

####################          step 6 export data                                   ##################################
write_csv(EF_Group_2_d3_filter_start_end_collar, 
          "W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_2_d3_filter_start_end_collar.csv") 









  
###################################################################################################################
###################                   DAY4 EF Group 2 - week 1          ###########################################  
################################################################################################################## 

##################             Step 1 bring in the raw logged data   ##############################################

EF_Group_2_d4<- read_csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/EF_Group_2_d4.csv", skip=64) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_2_d4.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################
glimpse(EF_Group_2_d4)

EF_Group_2_d4 <- left_join(EF_Group_2_d4, codes_EF_Group_2_d4, by= "trksegID") %>% 
  separate(name,into =  c("temp1","sheep", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -temp1,
         -trksegID1)   

##################        Step 3 filter data based on time                          ###################################
 
EF_Group_2_d4_filter_start <- filter(EF_Group_2_d4, hms > hms('08:55:00'))
end_timeEF_Group_2_d4 <- hms('08:55:00') + hms('04:00:00')
EF_Group_2_d4_filter_start_end <- filter(EF_Group_2_d4_filter_start, hms < end_timeEF_Group_2_d4)



#write.csv(EF_Group_1_d4_filter_start_end, "EF_Group_1_d4_filter_start_end.csv")
glimpse(EF_Group_2_d4_filter_start_end)#32,309

##################        Step 4 filter data based on collar (not handheld)        ###################################

EF_Group_2_d4_filter_start_end_collar <- filter(EF_Group_2_d4_filter_start_end, type == "collar")

glimpse(EF_Group_2_d4_filter_start_end_collar)

####################          step 5 convert lat and longs to x and Y               ##################################

glimpse(EF_Group_2_d4_filter_start_end_collar)

coordinates(EF_Group_2_d4_filter_start_end_collar) <- ~ lon + lat
proj4string(EF_Group_2_d4_filter_start_end_collar) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
EF_Group_2_d4_filter_start_end_collar_1 <- spTransform(EF_Group_2_d4_filter_start_end_collar, mapCRS)
#make new df_1
EF_Group_2_d4_filter_start_end_collar = as.data.frame(EF_Group_2_d4_filter_start_end_collar_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
EF_Group_2_d4_filter_start_end_collar <- mutate(EF_Group_2_d4_filter_start_end_collar,
                                                POINT_X = lon,  POINT_Y = lat )

####################          step 6 export data                                   ##################################
write_csv(EF_Group_2_d4_filter_start_end_collar, 
          "W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_2_d4_filter_start_end_collar.csv") 









###################################################################################################################
###################                   DAY1 VF Group 2 - week 2          ###########################################  
################################################################################################################## 

##################             Step 1 bring in the raw logged data   ##############################################



VF_Group_2_d1<- read_csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/VF_Group_2_d1.csv", skip=64) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_2_d1.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_2_d1)#33,573


##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################
VF_Group_2_d1 <- left_join(VF_Group_2_d1, codes_VF_Group_2_d1, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1) 



##################        Step 3 filter data based on time                          ###################################
glimpse(VF_Group_2_d1)

VF_Group_2_d1_filter_start <- filter(VF_Group_2_d1, hms > hms('09:08:00'))
end_timeVF_Group_2_d1 <- hms('09:08:00') + hms('04:00:00')
VF_Group_2_d1_filter_start_end <- filter(VF_Group_2_d1_filter_start, hms < end_timeVF_Group_2_d1)

glimpse(VF_Group_2_d1_filter_start_end)

VF_Group_2_d1_filter_start_end_collar <- filter(VF_Group_2_d1_filter_start_end, type == "collar")

glimpse(VF_Group_2_d1_filter_start_end_collar)


##################        Step 4 filter data based on collar (not handheld)        ###################################

VF_Group_2_d1_filter_start_end_collar <- filter(VF_Group_2_d1_filter_start_end_collar, type == "collar")

glimpse(VF_Group_2_d1_filter_start_end_collar)

####################          step 5 convert lat and longs to x and Y               ##################################

glimpse(VF_Group_2_d1_filter_start_end_collar)

coordinates(VF_Group_2_d1_filter_start_end_collar) <- ~ lon + lat
proj4string(VF_Group_2_d1_filter_start_end_collar) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF_Group_2_d1_filter_start_end_collar_1 <- spTransform(VF_Group_2_d1_filter_start_end_collar, mapCRS)
#make new df_1
VF_Group_2_d1_filter_start_end_collar = as.data.frame(VF_Group_2_d1_filter_start_end_collar_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF_Group_2_d1_filter_start_end_collar <- mutate(VF_Group_2_d1_filter_start_end_collar,
                                             POINT_X = lon,  POINT_Y = lat )


####################          step 6 export data                                   ##################################
write_csv(VF_Group_2_d1_filter_start_end_collar, 
          "W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_2_d1_filter_start_end_collar.csv") 




###################################################################################################################
###################                   DAY2 VF Group 2 - week 1          ###########################################  
################################################################################################################## 

##################             Step 1 bring in the raw logged data   ##############################################


VF_Group_2_d2<- read_csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/VF_Group_2_d2.csv", skip=64) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_2_d2.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_2_d2)
#glimpse(codes_VF_Group_1_d2)

##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################

VF_Group_2_d2 <- left_join(VF_Group_2_d2, codes_VF_Group_2_d2, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1)   

##################        Step 3 filter data based on time                          ###################################
VF_Group_2_d2_filter_start <- filter(VF_Group_2_d2, hms > hms('08:43:00'))

end_timeVF_Group_2_d2 <- hms('08:43:00') + hms('04:00:00')
VF_Group_2_d2_filter_start_end <- filter(VF_Group_2_d2_filter_start, hms < end_timeVF_Group_2_d2)

glimpse(VF_Group_2_d2_filter_start_end)
##################        Step 4 filter data based on collar (not handheld)        ###################################

VF_Group_2_d2_filter_start_end_collar <- filter(VF_Group_2_d2_filter_start_end, type == "collar")
glimpse(VF_Group_2_d2_filter_start_end_collar)

####################          step 5 convert lat and longs to x and Y               ##################################

glimpse(VF_Group_2_d2_filter_start_end_collar)

coordinates(VF_Group_2_d2_filter_start_end_collar) <- ~ lon + lat
proj4string(VF_Group_2_d2_filter_start_end_collar) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF_Group_2_d2_filter_start_end_collar_1 <- spTransform(VF_Group_2_d2_filter_start_end_collar, mapCRS)
#make new df_1
VF_Group_2_d2_filter_start_end_collar = as.data.frame(VF_Group_2_d2_filter_start_end_collar_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF_Group_2_d2_filter_start_end_collar <- mutate(VF_Group_2_d2_filter_start_end_collar,
                                                POINT_X = lon,  POINT_Y = lat )


####################          step 6 export data                                   ##################################
write_csv(VF_Group_2_d2_filter_start_end_collar, 
          "W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_2_d2_filter_start_end_collar.csv") 













###################################################################################################################
###################                   DAY3 VF Group 2 - week 2          ###########################################  
################################################################################################################## 

##################             Step 1 bring in the raw logged data   ##############################################


VF_Group_2_d3<- read_csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/VF_Group_2_d3.csv", skip=64) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_2_d3.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_2_d3)


##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################

VF_Group_2_d3 <- left_join(VF_Group_2_d3, codes_VF_Group_2_d3, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1)     

##################        Step 3 filter data based on time                          ###################################
VF_Group_2_d3_filter_start <- filter(VF_Group_2_d3, hms > hms('09:02:00'))
end_timeVF_Group_2_d3 <- hms('09:02:00') + hms('04:00:00')
VF_Group_2_d3_filter_start_end <- filter(VF_Group_2_d3_filter_start, hms < end_timeVF_Group_2_d3)


glimpse(VF_Group_2_d3_filter_start_end)
##################        Step 4 filter data based on collar (not handheld)        ###################################

VF_Group_2_d3_filter_start_end_collar <- filter(VF_Group_2_d3_filter_start_end, type == "collar")

glimpse(VF_Group_2_d3_filter_start_end_collar)

####################          step 5 convert lat and longs to x and Y               ##################################

glimpse(VF_Group_2_d3_filter_start_end_collar)

coordinates(VF_Group_2_d3_filter_start_end_collar) <- ~ lon + lat
proj4string(VF_Group_2_d3_filter_start_end_collar) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF_Group_2_d3_filter_start_end_collar_1 <- spTransform(VF_Group_2_d3_filter_start_end_collar, mapCRS)
#make new df_1
VF_Group_2_d3_filter_start_end_collar = as.data.frame(VF_Group_2_d3_filter_start_end_collar_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF_Group_2_d3_filter_start_end_collar <- mutate(VF_Group_2_d3_filter_start_end_collar,
                                                POINT_X = lon,  POINT_Y = lat )


####################          step 6 export data                                   ##################################
write_csv(VF_Group_2_d3_filter_start_end_collar, 
          "W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_2_d3_filter_start_end_collar.csv") 













###################################################################################################################
###################                   DAY4 VF Group 2 - week 2          ###########################################  
################################################################################################################## 

##################             Step 1 bring in the raw logged data   ##############################################


VF_Group_2_d4<- read_csv("W:/VF/pasture_utilisation/Take2/raw_data/Week 2/VF_Group_2_d4.csv", skip=64) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_2_d4.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_2_d4)


##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################

VF_Group_2_d4 <- left_join(VF_Group_2_d4, codes_VF_Group_2_d4, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1)        


##################        Step 3 filter data based on time                          ###################################
VF_Group_2_d4_filter_start <- filter(VF_Group_2_d4, hms > hms('09:00:00'))
end_timeVF_Group_2_d4 <- hms('09:00:00') + hms('04:00:00')
VF_Group_2_d4_filter_start_end <- filter(VF_Group_2_d4_filter_start, hms < end_timeVF_Group_2_d4)


glimpse(VF_Group_2_d4_filter_start_end)
##################        Step 4 filter data based on collar (not handheld)        ###################################

VF_Group_2_d4_filter_start_end_collar <- filter(VF_Group_2_d4_filter_start_end, type == "collar")

glimpse(VF_Group_2_d4_filter_start_end_collar)
  
####################          step 5 convert lat and longs to x and Y               ##################################



coordinates(VF_Group_2_d4_filter_start_end_collar) <- ~ lon + lat
proj4string(VF_Group_2_d4_filter_start_end_collar) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF_Group_2_d4_filter_start_end_collar_1 <- spTransform(VF_Group_2_d4_filter_start_end_collar, mapCRS)
#make new df_1
VF_Group_2_d4_filter_start_end_collar = as.data.frame(VF_Group_2_d4_filter_start_end_collar_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF_Group_2_d4_filter_start_end_collar <- mutate(VF_Group_2_d4_filter_start_end_collar,
                                                POINT_X = lon,  POINT_Y = lat )


####################          step 6 export data                                   ##################################
write_csv(VF_Group_2_d4_filter_start_end_collar, 
          "W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_2_d4_filter_start_end_collar.csv") 

  
  
  
  
#################################################################################################################################  
############################               summary of files that have been clipped                ##############################  
#####     Week1    EF    #######
#this first one might need another clm added to it


EF_Group_2_d1_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_2_d1_clipped.txt")
EF_Group_2_d2_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_2_d2_clipped.txt")
EF_Group_2_d3_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_2_d3_clipped.txt")
EF_Group_2_d4_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_2_d4_clipped.txt")

glimpse(EF_Group_2_d1_clipped)
glimpse(EF_Group_2_d2_clipped)

EF_Group_2_clipped <- rbind(EF_Group_2_d1_clipped,
                            EF_Group_2_d2_clipped,
                            EF_Group_2_d3_clipped,
                            EF_Group_2_d4_clipped)
glimpse(EF_Group_2_clipped)
ggplot(EF_Group_2_clipped, aes(hms, NEAR_DIST, colour = day_of_exp))+
  geom_point()+
  facet_grid(day_of_exp~sheep)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "EF week 2",
       x= "Time of day",
       y = "Distance (m)")

#####     Week1    VF    #######

VF_Group_2_d1_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_2_d1_clipped.txt")
VF_Group_2_d2_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_2_d2_clipped.txt")
VF_Group_2_d3_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_2_d3_clipped.txt")
VF_Group_2_d4_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_2_d4_clipped.txt")

vF_Group_2_clipped <- rbind(VF_Group_2_d1_clipped,
                            VF_Group_2_d2_clipped,
                            VF_Group_2_d3_clipped,
                            VF_Group_2_d4_clipped)

glimpse(vF_Group_2_clipped)
ggplot(vF_Group_2_clipped, aes(hms, NEAR_DIST, colour = day_of_exp))+
  geom_point()+
  facet_grid(day_of_exp~sheep)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "VF week 2",
       x= "Time of day",
       y = "Distance (m)")


write_csv(EF_Group_1_clipped, 
          "W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_1_clipped.csv") 

write_csv(vF_Group_1_clipped, 
          "W:/VF/pasture_utilisation/Take2/data_for_clipping/vF_Group_1_clipped.csv") 


####stop here###
  
  
  
  
  
  




#
