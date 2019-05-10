library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(lubridate)
library(ggplot2)
library(readr)


##### week 3 ######

setwd("c:/Users/ouz001/pasture_utilisation/Take2")


#Bring in csv files with new format

#getting the codes out of the raw data file
#Danila has changed something and the files won't load now has white spaces sperated need to use read_table2!
####DAY1 EF sheep codes####


codes_EF_Group_3_d1<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/EF_Group_3_d1.csv",skip=33)
codes_EF_Group_3_d1 <- codes_EF_Group_3_d1[1:12, 1:2]
codes_EF_Group_3_d1 <- mutate(codes_EF_Group_3_d1, trksegID = as.integer(ID))

codes_EF_Group_3_d2 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/EF_Group_3_d2.csv", skip=33)
codes_EF_Group_3_d2 <- codes_EF_Group_3_d2[1:12, 1:2]
codes_EF_Group_3_d2 <- mutate(codes_EF_Group_3_d2, trksegID1 = as.character(ID))
codes_EF_Group_3_d2 <- mutate(codes_EF_Group_3_d2, trksegID = as.integer(trksegID1))

codes_EF_Group_3_d3 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/EF_Group_3_d3.csv", skip=33)
codes_EF_Group_3_d3 <- codes_EF_Group_3_d3[1:12,1:2]
codes_EF_Group_3_d3 <- mutate(codes_EF_Group_3_d3, trksegID = as.integer(ID))
codes_EF_Group_3_d3 <- mutate(codes_EF_Group_3_d3, trksegID1 = as.character(ID))
codes_EF_Group_3_d3 <- mutate(codes_EF_Group_3_d3, trksegID = as.integer(trksegID1))


codes_EF_Group_3_d4 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/EF_Group_3_d4.csv", skip=33)
codes_EF_Group_3_d4 <- codes_EF_Group_3_d4[1:12,1:2]
codes_EF_Group_3_d4 <- mutate(codes_EF_Group_3_d4, trksegID1 = as.character(ID))
codes_EF_Group_3_d4 <- mutate(codes_EF_Group_3_d4, trksegID = as.integer(trksegID1))

####DAY1 VF sheep codes####
codes_VF_Group_3_d1<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/VF_Group_3_d1.csv", skip=33)
codes_VF_Group_3_d1 <- codes_VF_Group_3_d1[1:12, 1:2]
codes_VF_Group_3_d1 <- mutate(codes_VF_Group_3_d1, trksegID1 = as.character(ID))
codes_VF_Group_3_d1 <- mutate(codes_VF_Group_3_d1, trksegID = as.integer(trksegID1))

codes_VF_Group_3_d2 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/VF_Group_3_d2.csv", skip=33)
codes_VF_Group_3_d2 <- codes_VF_Group_3_d3[1:12, 1:2]
glimpse(codes_VF_Group_3_d2)
codes_VF_Group_3_d2 <- mutate(codes_VF_Group_3_d2, trksegID1 = as.character(ID))
codes_VF_Group_3_d2 <- mutate(codes_VF_Group_3_d2, trksegID = as.integer(trksegID1))

codes_VF_Group_3_d3 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/VF_Group_3_d3.csv", skip=33)
codes_VF_Group_3_d3 <- codes_VF_Group_2_d3[1:12,1:2]
codes_VF_Group_3_d3 <- mutate(codes_VF_Group_3_d3, trksegID1 = as.character(ID))
codes_VF_Group_3_d3 <- mutate(codes_VF_Group_3_d3, trksegID = as.integer(trksegID1))

codes_VF_Group_3_d4 <- read.csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/VF_Group_3_d4.csv", skip=33)
codes_VF_Group_3_d4 <- codes_VF_Group_2_d4[1:12,1:2]
codes_VF_Group_3_d4 <- mutate(codes_VF_Group_3_d4, trksegID1 = as.character(ID))
codes_VF_Group_3_d4 <- mutate(codes_VF_Group_3_d4, trksegID = as.integer(trksegID1))


###EF_Group_3_d1###

EF_Group_3_d1<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/EF_Group_3_d1.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_3_d1.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(EF_Group_3_d1)
glimpse(codes_EF_Group_3_d1)
###JOIN The sheep code data based on   trksegID
  
EF_Group_3_d1 <- left_join(EF_Group_3_d1, codes_EF_Group_3_d1, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         #-trksegID1,
         -sheep_temp,
         -sheep1) 
glimpse(EF_Group_3_d1)
write.csv(EF_Group_3_d1, "EF_Group_3_d1.csv") 

####filtering data with time
EF_Group_3_d1_filter_start <- filter(EF_Group_3_d1, hms > hms('08:03:00'))
write.csv(EF_Group_3_d1_filter_start, "EF_Group_3_d1_filter_start.csv")
glimpse(EF_Group_3_d1_filter_start)

EF_Group_3_d1_filter_start_hh <- filter(EF_Group_3_d1_filter_start, type != "collar")
EF_Group_3_d1_filter_start_collar <- filter(EF_Group_3_d1_filter_start, type == "collar")


write.csv(EF_Group_3_d1_filter_start_hh, "EF_Group_3_d1_filter_start_hh_R.csv")
write.csv(EF_Group_3_d1_filter_start_collar, "EF_Group_3_d1_filter_start_collar_R.csv") 
glimpse(EF_Group_3_d1_filter_start_hh)      
         
###EF_Group_3_d2###
EF_Group_3_d2<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/EF_Group_3_d2.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_3_d2.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))
###JOIN The sheep code data based on   trksegID
glimpse(EF_Group_3_d2)
#glimpse(codes_EF_Group_3_d2)
EF_Group_3_d2 <- left_join(EF_Group_3_d2, codes_EF_Group_3_d2, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         #-trksegID1,
         -sheep_temp,
         -sheep1) 
write.csv(EF_Group_3_d2, "EF_Group_3_d2.csv") 
EF_Group_3_d2_filter_start <- filter(EF_Group_3_d2, hms > hms('08:56:00'))
EF_Group_3_d2_filter_start_hh <- filter(EF_Group_3_d2_filter_start, type != "collar")
EF_Group_3_d2_filter_start_collar <- filter(EF_Group_3_d2_filter_start, type == "collar")
write.csv(EF_Group_3_d2_filter_start_hh, "EF_Group_3_d2_filter_start_hh_R.csv")
write.csv(EF_Group_3_d2_filter_start_collar, "EF_Group_3_d2_filter_start_collar_R.csv") 

###EF_Group_3_d3###
EF_Group_3_d3<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/EF_Group_3_d3.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_3_d3.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))
###JOIN The sheep code data based on   trksegID
glimpse(EF_Group_3_d3)
#glimpse(codes_EF_Group_3_d3)
EF_Group_3_d3 <- left_join(EF_Group_3_d3, codes_EF_Group_3_d3, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         #-trksegID1,
         -sheep_temp,
         -sheep1)   
write.csv(EF_Group_3_d3, "EF_Group_3_d3.csv")
EF_Group_3_d3_filter_start <- filter(EF_Group_3_d3, hms > hms('08:43:00'))
EF_Group_3_d3_filter_start_hh <- filter(EF_Group_3_d3_filter_start, type != "collar")
EF_Group_3_d3_filter_start_collar <- filter(EF_Group_3_d3_filter_start, type == "collar")
write.csv(EF_Group_3_d3_filter_start_hh, "EF_Group_3_d3_filter_start_hh_R.csv")
write.csv(EF_Group_3_d3_filter_start_collar, "EF_Group_3_d3_filter_start_collar_R.csv") 
  
###EF_Group_3_d4###


EF_Group_3_d4<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/EF_Group_3_d4.csv", skip=61) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "EF_Group_3_d4.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))
###JOIN The sheep code data based on   trksegID
glimpse(EF_Group_3_d4)
#glimpse(codes_EF_Group_3_d4)
EF_Group_3_d4 <- left_join(EF_Group_3_d4, codes_EF_Group_3_d4, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         #-trksegID1,
         -sheep_temp,
         -sheep1)
glimpse(EF_Group_3_d4)

write.csv(EF_Group_3_d4, "EF_Group_3_d4.csv")  
EF_Group_3_d4_filter_start <- filter(EF_Group_3_d4, hms > hms('08:40:00'))
EF_Group_3_d4_filter_start_hh <- filter(EF_Group_3_d4_filter_start, type != "collar")
EF_Group_3_d4_filter_start_collar <- filter(EF_Group_3_d4_filter_start, type == "collar")
write.csv(EF_Group_3_d4_filter_start_hh, "EF_Group_3_d4_filter_start_hh_R.csv")
write.csv(EF_Group_3_d4_filter_start_collar, "EF_Group_3_d4_filter_start_collar_R.csv")  
  
###VF_Group_3_d1###

VF_Group_3_d1<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/VF_Group_3_d1.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_3_d1.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_3_d1)
#glimpse(codes_VF_Group_3_d1)
###JOIN The sheep code data based on   trksegID

VF_Group_3_d1 <- left_join(VF_Group_3_d1, codes_VF_Group_3_d1, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1) 



glimpse(VF_Group_3_d1)
write.csv(VF_Group_3_d1, "VF_Group_3_d1.csv")

VF_Group_3_d1_filter_start <- filter(VF_Group_3_d1, hms > hms('09:08:00'))
VF_Group_3_d1_filter_start_hh <- filter(VF_Group_3_d1_filter_start, type != "collar")
VF_Group_3_d1_filter_start_collar <- filter(VF_Group_3_d1_filter_start, type == "collar")
write.csv(VF_Group_3_d1_filter_start_hh, "VF_Group_3_d1_filter_start_hh_R.csv")
write.csv(VF_Group_3_d1_filter_start_collar, "VF_Group_3_d1_filter_start_collar_R.csv")
###VF_Group_3_d2###

VF_Group_3_d2<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/VF_Group_3_d2.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_3_d2.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_3_d2)
glimpse(codes_VF_Group_3_d2)
###JOIN The sheep code data based on   trksegID

VF_Group_3_d2 <- left_join(VF_Group_3_d2, codes_VF_Group_3_d2, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1)  
glimpse(VF_Group_3_d2)

write.csv(VF_Group_3_d2, "VF_Group_3_d2.csv")
VF_Group_3_d2_filter_start <- filter(VF_Group_3_d2, hms > hms('09:01:00'))
VF_Group_3_d2_filter_start_hh <- filter(VF_Group_3_d2_filter_start, type != "collar")
VF_Group_3_d2_filter_start_collar <- filter(VF_Group_3_d2_filter_start, type == "collar")
write.csv(VF_Group_3_d2_filter_start_hh, "VF_Group_3_d2_filter_start_hh_R.csv")
write.csv(VF_Group_3_d2_filter_start_collar, "VF_Group_3_d2_filter_start_collar_R.csv")

###VF_Group_2_d3###

VF_Group_3_d3<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/VF_Group_3_d3.csv", skip=63) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_3_d3.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_3_d3)
#glimpse(codes_VF_Group_3_d3)
###JOIN The sheep code data based on   trksegID

VF_Group_3_d3 <- left_join(VF_Group_3_d3, codes_VF_Group_3_d3, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1)     
write.csv(VF_Group_3_d3, "VF_Group_3_d3.csv")

VF_Group_3_d3_filter_start <- filter(VF_Group_3_d3, hms > hms('08:47:00'))
VF_Group_3_d3_filter_start_hh <- filter(VF_Group_3_d3_filter_start, type != "collar")
VF_Group_3_d3_filter_start_collar <- filter(VF_Group_3_d3_filter_start, type == "collar")
write.csv(VF_Group_3_d3_filter_start_hh, "VF_Group_3_d3_filter_start_hh_R.csv")
write.csv(VF_Group_3_d3_filter_start_collar, "VF_Group_3_d3_filter_start_collar_R.csv")
  
###VF_Group_3_d4###

VF_Group_3_d4<- read_csv("C:/Users/ouz001/pasture_utilisation/Take2/raw_data/Week 3/VF_Group_3_d4.csv", skip=61) %>% 
  select(ID, trksegID, lat, lon,  ele, time) %>% 
  mutate(file_name = "VF_Group_3_d4.csv") %>% 
  separate(file_name,into =  c("fence", "group", "week", "day_of_exp"),  sep = "_", remove = FALSE ) %>% 
  select(-group)%>% 
  separate(day_of_exp,into =  c("day_of_exp"),  sep = ".csv", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time),
         date= date(time),
         month = month(time),
         day = day(time))

glimpse(VF_Group_3_d4)
#glimpse(codes_VF_Group_3_d3)
###JOIN The sheep code data based on   trksegID

VF_Group_3_d4 <- left_join(VF_Group_3_d4, codes_VF_Group_3_d4, by= "trksegID") %>% 
  separate(name,into =  c("sheep_temp", "sheep1", "type", "temp"),  sep = "_", remove = FALSE ) %>% 
  mutate(sheep =paste0(sheep_temp, " ", sheep1)) %>% 
  select(-ID.y,
         ID = ID.x,
         -temp,
         -trksegID1,
         -sheep_temp,
         -sheep1)        

write.csv(VF_Group_3_d4, "VF_Group_3_d4.csv") 
VF_Group_3_d4_filter_start <- filter(VF_Group_3_d4, hms > hms('08:44:00'))
VF_Group_3_d4_filter_start_hh <- filter(VF_Group_3_d4_filter_start, type != "collar")
VF_Group_3_d4_filter_start_collar <- filter(VF_Group_3_d4_filter_start, type == "collar")
write.csv(VF_Group_3_d4_filter_start_hh, "VF_Group_3_d4_filter_start_hh_R.csv")
write.csv(VF_Group_3_d4_filter_start_collar, "VF_Group_3_d4_filter_start_collar_R.csv")
  
  
  
  
  
  
  
  






####stop here###
  
  
  
  
  
  



#divide data up into type collar and hh

group1_week1_ef_hh <- filter(group1_week1_ef, type != "collar")
group1_week1_ef_collar <- filter(group1_week1_ef, type == "collar")
glimpse(group1_week1_ef_hh)
glimpse(group1_week1_ef_collar)
write.csv(group1_week1_ef_collar, "group1_week1_ef_collar_postR.csv")
write.csv(group1_week1_ef_hh, "group1_week1_ef_hh_postR.csv")

#