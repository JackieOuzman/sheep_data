library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(lubridate)
library(ggplot2)
library(readr)


##### week 1 ######
#now connected to GIT hub
setwd("c:/Users/ouz001/pasture_utilisation/Take2")


#Bring in csv files with new format

#getting the codes out of the raw data file
#Danila has changed something and the files won't load now has white spaces sperated need to use read_table2!

####################################################################################################################
###################       Create a df with the sheep and tracking ID ##############################################
###################                   DAY1 EF sheep codes            ##############################################
####################################################################################################################

codes_EF_Group_1_d1<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/EF_Group_1_d1.csv",skip=33)
codes_EF_Group_1_d1 <- codes_EF_Group_1_d1[1:12, 1:2]
codes_EF_Group_1_d1 <- mutate(codes_EF_Group_1_d1, trksegID = as.integer(ID))
glimpse(codes_EF_Group_1_d1)

codes_EF_Group_1_d2 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/EF_Group_1_d2.csv", skip=33)
codes_EF_Group_1_d2 <- codes_EF_Group_1_d2[1:12, 1:2]
codes_EF_Group_1_d2 <- mutate(codes_EF_Group_1_d2, trksegID1 = as.character(ID))
codes_EF_Group_1_d2 <- mutate(codes_EF_Group_1_d2, trksegID = as.integer(trksegID1))

codes_EF_Group_1_d3 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/EF_Group_1_d3.csv", skip=33)
codes_EF_Group_1_d3 <- codes_EF_Group_1_d3[1:12,1:2]
codes_EF_Group_1_d3 <- mutate(codes_EF_Group_1_d3, trksegID = as.integer(ID))
codes_EF_Group_1_d3 <- mutate(codes_EF_Group_1_d3, trksegID1 = as.character(ID))
codes_EF_Group_1_d3 <- mutate(codes_EF_Group_1_d3, trksegID = as.integer(trksegID1))


codes_EF_Group_1_d4 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/EF_Group_1_d4.csv", skip=33)
codes_EF_Group_1_d4 <- codes_EF_Group_1_d4[1:12,1:2]
codes_EF_Group_1_d4 <- mutate(codes_EF_Group_1_d4, trksegID1 = as.character(ID))
codes_EF_Group_1_d4 <- mutate(codes_EF_Group_1_d4, trksegID = as.integer(trksegID1))

####################################################################################################################
###################                   DAY1 VF sheep codes            ##############################################
####################################################################################################################

codes_VF_Group_1_d1<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/VF_Group_1_d1.csv", skip=33)
codes_VF_Group_1_d1 <- codes_VF_Group_1_d1[1:12, 1:2]
codes_VF_Group_1_d1 <- mutate(codes_VF_Group_1_d1, trksegID1 = as.character(ID))
codes_VF_Group_1_d1 <- mutate(codes_VF_Group_1_d1, trksegID = as.integer(trksegID1))

codes_VF_Group_1_d2 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/VF_Group_1_d2.csv", skip=33)
codes_VF_Group_1_d2 <- codes_VF_Group_1_d2[1:12, 1:2]
codes_VF_Group_1_d2 <- mutate(codes_VF_Group_1_d2, trksegID1 = as.character(ID))
codes_VF_Group_1_d2 <- mutate(codes_VF_Group_1_d2, trksegID = as.integer(trksegID1))

codes_VF_Group_1_d3 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/VF_Group_1_d3.csv", skip=33)
codes_VF_Group_1_d3 <- codes_VF_Group_1_d3[1:12,1:2]
codes_VF_Group_1_d3 <- mutate(codes_VF_Group_1_d3, trksegID1 = as.character(ID))
codes_VF_Group_1_d3 <- mutate(codes_VF_Group_1_d3, trksegID = as.integer(trksegID1))

codes_VF_Group_1_d4 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/VF_Group_1_d4.csv", skip=33)
codes_VF_Group_1_d4 <- codes_VF_Group_1_d4[1:12,1:2]
codes_VF_Group_1_d4 <- mutate(codes_VF_Group_1_d4, trksegID1 = as.character(ID))
codes_VF_Group_1_d4 <- mutate(codes_VF_Group_1_d4, trksegID = as.integer(trksegID1))


####################################################################################################################
###################       Create a df with the logged data           ##############################################
###################################################################################################################
###################################################################################################################
###################                   DAY1 EF Group 1 - week 1          ###########################################
####################################################################################################################

##################             Step 1 bring in the raw logged data   ##############################################
EF_Group_1_d1<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/EF_Group_1_d1.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_1_d1.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))


##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################
glimpse(EF_Group_1_d1) #44588
glimpse(codes_EF_Group_1_d1) #12


EF_Group_1_d1 <- left_join(EF_Group_1_d1, codes_EF_Group_1_d1, by= "trksegID") %>% 
  separate(name,into =  c("sheep", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp)
glimpse(EF_Group_1_d1) #44588

#write.csv(EF_Group_1_d1, "EF_Group_1_d1.csv") 

##################        Step 3 filter data based on time                          ###################################

EF_Group_1_d1_filter_start <- filter(EF_Group_1_d1, hms > hms('09:15:00'))
end_timeEF_Group_1_d1_filter_start <- hms('09:15:00') + hms('04:00:00')
EF_Group_1_d1_filter_start_end <- filter(EF_Group_1_d1_filter_start, hms < (end_timeEF_Group_1_d1_filter_start))
glimpse(EF_Group_1_d1_filter_start_end)

#write.csv(EF_Group_1_d1_filter_start_end, "EF_Group_1_d1_filter_start_end.csv")
glimpse(EF_Group_1_d1_filter_start)

##################        Step 4 filter data based on collar (not handheld)        ###################################
#EF_Group_1_d1_filter_start_end_hh <- filter(EF_Group_1_d1_filter_start_end, type != "collar")
EF_Group_1_d1_filter_start_end_collar <- filter(EF_Group_1_d1_filter_start_end, type == "collar")
#write.csv(EF_Group_1_d1_filter_start_end_hh, "EF_Group_1_d1_filter_start_end_hh_R.csv")
#write.csv(EF_Group_1_d1_filter_start_end_collar, "EF_Group_1_d1_filter_start_end_collar_R.csv") 
glimpse(EF_Group_1_d1_filter_start_end_hh)


###################################################################################################################
###################                   DAY2 EF Group 1 - week 1          ###########################################  
##################################################################################################################         

##################             Step 1 bring in the raw logged data   ##############################################

EF_Group_1_d2<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/EF_Group_1_d2.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_1_d2.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################

glimpse(EF_Group_1_d2)
glimpse(codes_EF_Group_1_d2)
EF_Group_1_d2 <- left_join(EF_Group_1_d2, codes_EF_Group_1_d2, by= "trksegID") %>% 
  separate(name,into =  c("sheep", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1)
#write.csv(EF_Group_1_d2, "EF_Group_1_d2.csv") 

##################        Step 3 filter data based on time                          ###################################
EF_Group_1_d2_filter_start <- filter(EF_Group_1_d2, hms > hms('09:00:00'))
end_timeEF_Group_1_d2 <- hms('09:00:00') + hms('04:00:00')
EF_Group_1_d2_filter_start_end <- filter(EF_Group_1_d2_filter_start, hms < end_timeEF_Group_1_d2)


#write.csv(EF_Group_1_d2_filter_start_end, "EF_Group_1_d2_filter_start_end.csv")
glimpse(EF_Group_1_d2_filter_start_end)

##################        Step 4 filter data based on collar (not handheld)        ###################################
#EF_Group_1_d2_filter_start_end_hh <- filter(EF_Group_1_d2_filter_start_end, type != "collar")
EF_Group_1_d2_filter_start_end_collar <- filter(EF_Group_1_d2_filter_start_end, type == "collar")
#write.csv(EF_Group_1_d2_filter_start_end_hh, "EF_Group_1_d2_filter_start_end_hh_R.csv")
#write.csv(EF_Group_1_d2_filter_start_end_collar, "EF_Group_1_d2_filter_start_end_collar_R.csv") 
glimpse(EF_Group_1_d2_filter_start_end_hh)


###################################################################################################################
###################                   DAY3 EF Group 1 - week 1          ###########################################  
################################################################################################################## 
      

##################             Step 1 bring in the raw logged data   ##############################################

EF_Group_1_d3<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/EF_Group_1_d3.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_1_d3.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))
##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################


glimpse(EF_Group_1_d3)
glimpse(codes_EF_Group_1_d3)
EF_Group_1_d3 <- left_join(EF_Group_1_d3, codes_EF_Group_1_d3, by= "trksegID") %>% 
  separate(name,into =  c("sheep", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1)  
##################        Step 3 filter data based on time                          ###################################
#write.csv(EF_Group_1_d3, "EF_Group_1_d3.csv")
EF_Group_1_d3_filter_start <- filter(EF_Group_1_d3, hms > hms('08:50:00'))
end_timeEF_Group_1_d3 <- hms('08:50:00') + hms('04:00:00')
EF_Group_1_d3_filter_start_end <- filter(EF_Group_1_d3_filter_start, hms < end_timeEF_Group_1_d3)

#write.csv(EF_Group_1_d3_filter_start_end, "EF_Group_1_d3_filter_start_end.csv")
glimpse(EF_Group_1_d3_filter_start_end)
##################        Step 4 filter data based on collar (not handheld)        ###################################
EF_Group_1_d3_filter_start_end_hh <- filter(EF_Group_1_d3_filter_start_end, type != "collar")
EF_Group_1_d3_filter_start_end_collar <- filter(EF_Group_1_d3_filter_start_end, type == "collar")
#write.csv(EF_Group_1_d3_filter_start_end_hh, "EF_Group_1_d3_filter_start_end_hh_R.csv")
#write.csv(EF_Group_1_d3_filter_start_end_collar, "EF_Group_1_d3_filter_start_end_collar_R.csv") 
glimpse(EF_Group_1_d3_filter_start_end_hh)

  
###################################################################################################################
###################                   DAY4 EF Group 1 - week 1          ###########################################  
################################################################################################################## 

##################             Step 1 bring in the raw logged data   ##############################################

EF_Group_1_d4<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/EF_Group_1_d4.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_1_d4.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################
glimpse(EF_Group_1_d4)
#glimpse(codes_EF_Group_1_d4)
EF_Group_1_d4 <- left_join(EF_Group_1_d4, codes_EF_Group_1_d4, by= "trksegID") %>% 
  separate(name,into =  c("sheep", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1)   

##################        Step 3 filter data based on time                          ###################################
#write.csv(EF_Group_1_d4, "EF_Group_1_d4.csv")  
EF_Group_1_d4_filter_start <- filter(EF_Group_1_d4, hms > hms('08:33:00'))
end_timeEF_Group_1_d4 <- hms('08:33:00') + hms('04:00:00')
EF_Group_1_d4_filter_start_end <- filter(EF_Group_1_d4_filter_start, hms < end_timeEF_Group_1_d4)



#write.csv(EF_Group_1_d4_filter_start_end, "EF_Group_1_d4_filter_start_end.csv")
glimpse(EF_Group_1_d4_filter_start_end)

##################        Step 4 filter data based on collar (not handheld)        ###################################
EF_Group_1_d4_filter_start_end_hh <- filter(EF_Group_1_d4_filter_start_end, type != "collar")
EF_Group_1_d4_filter_start_end_collar <- filter(EF_Group_1_d4_filter_start_end, type == "collar")
#write.csv(EF_Group_1_d4_filter_start_end_hh, "EF_Group_1_d4_filter_start_end_hh_R.csv")
#write.csv(EF_Group_1_d4_filter_start_end_collar, "EF_Group_1_d4_filter_start_end_collar_R.csv") 
glimpse(EF_Group_1_d4_filter_start_end_hh)







###################################################################################################################
###################                   DAY1 VF Group 1 - week 1          ###########################################  
################################################################################################################## 

##################             Step 1 bring in the raw logged data   ##############################################



VF_Group_1_d1<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/VF_Group_1_d1.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_1_d1.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_1_d1)
#glimpse(codes_VF_Group_1_d1)

##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################
VF_Group_1_d1 <- left_join(VF_Group_1_d1, codes_VF_Group_1_d1, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1) 

#VF_Group_1_d1 <- mutate(VF_Group_1_d1, sheep =paste0(VF_Group_1_d1$sheep_temp, " ", VF_Group_1_d1$sheep1))

##################        Step 3 filter data based on time                          ###################################
glimpse(VF_Group_1_d1)
#write.csv(VF_Group_1_d1, "VF_Group_1_d1.csv")
VF_Group_1_d1_filter_start <- filter(VF_Group_1_d1, hms > hms('09:25:00'))
end_timeVF_Group_1_d1 <- hms('09:25:00') + hms('04:00:00')
VF_Group_1_d1_filter_start_end <- filter(VF_Group_1_d1_filter_start, hms < end_timeVF_Group_1_d1)


#write.csv(VF_Group_1_d1_filter_start_end, "VF_Group_1_d1_filter_start_end.csv")
glimpse(VF_Group_1_d1_filter_start_end)

#VF_Group_1_d1_filter_start_end_hh <- filter(VF_Group_1_d1_filter_start_end, type != "collar")
VF_Group_1_d1_filter_start_end_collar <- filter(VF_Group_1_d1_filter_start_end, type == "collar")
#write.csv(VF_Group_1_d1_filter_start_end_hh, "VF_Group_1_d1_filter_start_end_hh_R.csv")
#write.csv(VF_Group_1_d1_filter_start_end_collar, "VF_Group_1_d1_filter_start_end_collar_R.csv") 
glimpse(VF_Group_1_d1_filter_start_end_hh)


###################################################################################################################
###################                   DAY2 VF Group 1 - week 1          ###########################################  
################################################################################################################## 

##################             Step 1 bring in the raw logged data   ##############################################


VF_Group_1_d2<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/VF_Group_1_d2.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_1_d2.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_1_d2)
#glimpse(codes_VF_Group_1_d2)

##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################

VF_Group_1_d2 <- left_join(VF_Group_1_d2, codes_VF_Group_1_d2, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1)   
#write.csv(VF_Group_1_d2, "VF_Group_1_d2.csv")
##################        Step 3 filter data based on time                          ###################################
VF_Group_1_d2_filter_start <- filter(VF_Group_1_d2, hms > hms('09:15:00'))
#write.csv(VF_Group_1_d2_filter_start, "VF_Group_1_d2_filter_start.csv")
end_timeVF_Group_1_d2 <- hms('09:15:00') + hms('04:00:00')
VF_Group_1_d2_filter_start_end <- filter(VF_Group_1_d2_filter_start, hms < end_timeVF_Group_1_d2)

glimpse(VF_Group_1_d2_filter_start_end)
##################        Step 4 filter data based on collar (not handheld)        ###################################
#VF_Group_1_d2_filter_start_end_hh <- filter(VF_Group_1_d2_filter_start_end, type != "collar")
VF_Group_1_d2_filter_start_end_collar <- filter(VF_Group_1_d2_filter_start_end, type == "collar")
#write.csv(VF_Group_1_d2_filter_start_end_hh, "VF_Group_1_d2_filter_start_end_hh_R.csv")
#write.csv(VF_Group_1_d2_filter_start_end_collar, "VF_Group_1_d2_filter_start_end_collar_R.csv") 
glimpse(VF_Group_1_d2_filter_start_end_hh)
###################################################################################################################
###################                   DAY3 VF Group 1 - week 1          ###########################################  
################################################################################################################## 

##################             Step 1 bring in the raw logged data   ##############################################


VF_Group_1_d3<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/VF_Group_1_d3.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_1_d3.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_1_d3)
#glimpse(codes_VF_Group_1_d3)

##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################

VF_Group_1_d3 <- left_join(VF_Group_1_d3, codes_VF_Group_1_d3, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1)     
#write.csv(VF_Group_1_d3, "VF_Group_1_d3.csv") 
##################        Step 3 filter data based on time                          ###################################
VF_Group_1_d3_filter_start <- filter(VF_Group_1_d3, hms > hms('08:53:00'))
end_timeVF_Group_1_d3 <- hms('08:53:00') + hms('04:00:00')
VF_Group_1_d3_filter_start_end <- filter(VF_Group_1_d3_filter_start, hms < end_timeVF_Group_1_d3)

#write.csv(VF_Group_1_d3_filter_start_end, "VF_Group_1_d3_filter_start_end.csv")
glimpse(VF_Group_1_d3_filter_start_end)
##################        Step 4 filter data based on collar (not handheld)        ###################################
#VF_Group_1_d3_filter_start_end_hh <- filter(VF_Group_1_d3_filter_start_end, type != "collar")
VF_Group_1_d3_filter_start_end_collar <- filter(VF_Group_1_d3_filter_start_end, type == "collar")
#write.csv(VF_Group_1_d3_filter_start_end_hh, "VF_Group_1_d3_filter_start_end_hh_R.csv")
#write.csv(VF_Group_1_d3_filter_start_end_collar, "VF_Group_1_d3_filter_start_end_collar_R.csv") 
glimpse(VF_Group_1_d3_filter_start_end_hh)
###################################################################################################################
###################                   DAY4 VF Group 1 - week 1          ###########################################  
################################################################################################################## 

##################             Step 1 bring in the raw logged data   ##############################################


VF_Group_1_d4<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 1/VF_Group_1_d4.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_1_d4.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_1_d4)
#glimpse(codes_VF_Group_1_d3)

##################             Step 2 JOIN The sheep code data based on   trksegID   ###################################

VF_Group_1_d4 <- left_join(VF_Group_1_d4, codes_VF_Group_1_d4, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1)        

#write.csv(VF_Group_1_d4, "VF_Group_1_d4.csv")   
##################        Step 3 filter data based on time                          ###################################
VF_Group_1_d4_filter_start <- filter(VF_Group_1_d4, hms > hms('08:43:00'))
end_timeVF_Group_1_d4 <- hms('08:43:00') + hms('04:00:00')
VF_Group_1_d4_filter_start_end <- filter(VF_Group_1_d4_filter_start, hms < end_timeVF_Group_1_d4)

#write.csv(VF_Group_1_d4_filter_start_end, "VF_Group_1_d4_filter_start_end.csv")
glimpse(VF_Group_1_d4_filter_start_end)
##################        Step 4 filter data based on collar (not handheld)        ###################################
#VF_Group_1_d4_filter_start_end_hh <- filter(VF_Group_1_d4_filter_start_end, type != "collar")
VF_Group_1_d4_filter_start_end_collar <- filter(VF_Group_1_d4_filter_start_end, type == "collar")
#write.csv(VF_Group_1_d4_filter_start_end_hh, "VF_Group_1_d4_filter_start_end_hh_R.csv")
#write.csv(VF_Group_1_d4_filter_start_end_collar, "VF_Group_1_d4_filter_start_end_collar_R.csv") 
glimpse(VF_Group_1_d4_filter_start_end_hh)
  
  
  
  
  
  
  
  






####stop here###
  
  
  
  
  
  




#
