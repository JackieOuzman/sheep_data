library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)

library(ggplot2)
library(readr)

#install.packages("hms")
#library(hms)
#library(sp)
#library(biogeo)
#library(stringr)
#library(rgdal)
#library(sf)



############################################################################################################################
####################          week1  day 1-4  EF_Group_1_clipped               ##################################
#############################################################################################################################


EF_Group_1_d1_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_1_d1_clipped.txt")
EF_Group_1_d1_clipped <- mutate(EF_Group_1_d1_clipped, NEAR_FC = 'arm_hill_x1_nth')

EF_Group_1_d2_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_1_d2_clipped.txt")
EF_Group_1_d3_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_1_d3_clipped.txt")
EF_Group_1_d4_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/EF_Group_1_d4_clipped.txt")


EF_Group_1_clipped <- rbind(EF_Group_1_d1_clipped,
                            EF_Group_1_d2_clipped,
                            EF_Group_1_d3_clipped,
                            EF_Group_1_d4_clipped)


############################################################################################################################
#####################                    Min time for data set  EF_Group_1_clipped day 1 -4#########################################
############################################################################################################################

min_time1 <- (min(EF_Group_1_clipped$hms))
print(min_time1) 
print(hms::as.hms(min_time1) )
min_time <- hms::as.hms(min_time1)
print(min_time) 
date_time_start <- as_datetime(paste0("2018-10-29 ", min_time))

############################################################################################################################
####################                    Max time for data set  EF_Group_1_clipped   day 1 -4#########################################
############################################################################################################################
max_time1 <- (max(EF_Group_1_clipped$hms))
print(max_time1) 
max_time <- hms::as.hms(max_time1)
print(max_time) 
date_time_end <- as_datetime(paste0("2018-10-29 ", max_time))


############################################################################################################################
####################                    time step starting at min and ending at max       ##################################
####################                    time step every second     day 1 -4              ##################################
############################################################################################################################

df_sec1 <- data.frame(time_step =seq(from = (date_time_start), to = (date_time_end), by = "sec"))
head(df_sec1)

############################################################################################################################
####################          make df into 2 clms one with date/time and one with time  day 1 -4 ############################
#############################################################################################################################


df_sec1 <- mutate(df_sec1,
                  hms = hms::as.hms(time_step, tz = "UTM"))



############################################################################################################################
####################           split logged data into df for each sheep       Day 1       ##################################
#############################################################################################################################

glimpse(EF_Group_1_d1_clipped) # 1168 data pts for all sheep

#this splits my data frame into lists 
split_sheep <- split(EF_Group_1_d1_clipped, EF_Group_1_d1_clipped$sheep, drop = FALSE)
glimpse(split_sheep) #List of 6

#list of names I want my data frames to be 
sheep_names_EF_Group_1_d1 <- group_by(EF_Group_1_d1_clipped,  name) %>% 
  count()
print(sheep_names_EF_Group_1_d1)


#subet each list in split_sheep to make df

sheep21_collar_d1 <- split_sheep[[1]]
Sheep22_collar_d1 <- split_sheep[[2]]  
Sheep23_collar_d1 <- split_sheep[[3]]  
Sheep24_collar_d1 <- split_sheep[[4]]
Sheep25_collar_d1 <- split_sheep[[5]]
Sheep26_collar_d1 <- split_sheep[[6]]

dim(sheep21_collar_d1)
dim(Sheep22_collar_d1)
dim(Sheep23_collar_d1)
dim(Sheep24_collar_d1)
dim(Sheep25_collar_d1)
dim(Sheep26_collar_d1)


############################################################################################################################
####################           merege time step data and logged data      day 1           ##################################
#############################################################################################################################                  


glimpse(df_sec1) #16,645 data pts df is reg time step
glimpse(sheep21_collar_d1) # 241 data pts for Sheep_21_collar_d1


#step 1 join
sheep_21_collar_d1_time_step1 <- full_join(df_sec1, sheep21_collar_d1, by = "hms")
Sheep_22_collar_d1_time_step1 <- full_join(df_sec1, Sheep22_collar_d1, by = "hms")
Sheep_23_collar_d1_time_step1 <- full_join(df_sec1, Sheep23_collar_d1, by = "hms")
Sheep_24_collar_d1_time_step1 <- full_join(df_sec1, Sheep24_collar_d1, by = "hms")
Sheep_25_collar_d1_time_step1 <- full_join(df_sec1, Sheep25_collar_d1, by = "hms")
Sheep_26_collar_d1_time_step1 <- full_join(df_sec1, Sheep26_collar_d1, by = "hms")

#dim(sheep_21_collar_d1_time_step1)
#colSums(is.na(sheep_21_collar_d1_time_step1))

#step 2 fill in missing values 
Sheep_21_collar_d1_time_step <- fill(sheep_21_collar_d1_time_step1,  .direction = "down", everything())
Sheep_22_collar_d1_time_step <- fill(Sheep_22_collar_d1_time_step1,  .direction = "down",everything())
Sheep_23_collar_d1_time_step <- fill(Sheep_23_collar_d1_time_step1,  .direction = "down",everything())
Sheep_24_collar_d1_time_step <- fill(Sheep_24_collar_d1_time_step1,  .direction = "down",everything())
Sheep_25_collar_d1_time_step <- fill(Sheep_25_collar_d1_time_step1,  .direction = "down",everything())
Sheep_26_collar_d1_time_step <- fill(Sheep_26_collar_d1_time_step1,  .direction = "down",everything())

#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#step 2 fill in missing values that are up 
Sheep_21_collar_d1_time_step <- fill(Sheep_21_collar_d1_time_step,  .direction = "up", everything())
Sheep_22_collar_d1_time_step <- fill(Sheep_22_collar_d1_time_step,  .direction = "up", everything())
Sheep_23_collar_d1_time_step <- fill(Sheep_23_collar_d1_time_step,  .direction = "up", everything())
Sheep_24_collar_d1_time_step <- fill(Sheep_24_collar_d1_time_step,  .direction = "up", everything())
Sheep_25_collar_d1_time_step <- fill(Sheep_25_collar_d1_time_step,  .direction = "up", everything())
Sheep_26_collar_d1_time_step <- fill(Sheep_26_collar_d1_time_step,  .direction = "up", everything())


#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#data enetry has a problem d1 should be day1
Sheep_21_collar_d1_time_step <- mutate(Sheep_21_collar_d1_time_step, name = "Sheep 21_collar_day1")


#remove the missing data rows 
colSums(is.na(Sheep_21_collar_d1_time_step)) #0 missing records
colSums(is.na(Sheep_22_collar_d1_time_step))
colSums(is.na(Sheep_23_collar_d1_time_step))
colSums(is.na(Sheep_24_collar_d1_time_step))
colSums(is.na(Sheep_25_collar_d1_time_step))
colSums(is.na(Sheep_26_collar_d1_time_step))


############################################################################################################################
####################          export my data I have created         day 1                  ##################################
############################################################################################################################# 

#EF_week1

dim(Sheep_21_collar_d1_time_step) #16,645
dim(Sheep_22_collar_d1_time_step) #16,645
dim(Sheep_23_collar_d1_time_step)
dim(Sheep_24_collar_d1_time_step)
dim(Sheep_25_collar_d1_time_step)
dim(Sheep_26_collar_d1_time_step)


write_csv(Sheep_21_collar_d1_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/Sheep_21_collar_d1_time_step.csv")
write_csv(Sheep_22_collar_d1_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/Sheep_22_collar_d1_time_step.csv")
write_csv(Sheep_23_collar_d1_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/Sheep_23_collar_d1_time_step.csv")
write_csv(Sheep_24_collar_d1_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/Sheep_24_collar_d1_time_step.csv")
write_csv(Sheep_25_collar_d1_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/Sheep_25_collar_d1_time_step.csv")
write_csv(Sheep_26_collar_d1_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/Sheep_26_collar_d1_time_step.csv")


EF_week1_d1_time_step <- rbind(Sheep_21_collar_d1_time_step,
                               Sheep_22_collar_d1_time_step,
                               Sheep_23_collar_d1_time_step,
                               Sheep_24_collar_d1_time_step,
                               Sheep_25_collar_d1_time_step,
                               Sheep_26_collar_d1_time_step)


write_csv(EF_week1_d1_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/EF_week1_d1_time_step.csv")



############################################################################################################################
####################           split logged data into df for each sheep    day 2           ##################################
#############################################################################################################################

glimpse(EF_Group_1_d2_clipped) # 1168 data pts for all sheep

#this splits my data frame into lists 
split_sheep_EF_wk1_day2 <- split(EF_Group_1_d2_clipped, EF_Group_1_d2_clipped$sheep, drop = FALSE)
glimpse(split_sheep_EF_wk1_day2) #List of 6

#list of names I want my data frames to be 
names_sheep_EF_wk1_day2 <- group_by(EF_Group_1_d2_clipped,  name) %>% 
  count()
print(names_sheep_EF_wk1_day2)


#subet each list in split_sheep to make df

sheep21_collar_d2 <- split_sheep_EF_wk1_day2[[1]]
Sheep22_collar_d2 <- split_sheep_EF_wk1_day2[[2]]  
Sheep23_collar_d2 <- split_sheep_EF_wk1_day2[[3]]  
Sheep24_collar_d2 <- split_sheep_EF_wk1_day2[[4]]
Sheep25_collar_d2 <- split_sheep_EF_wk1_day2[[5]]
Sheep26_collar_d2 <- split_sheep_EF_wk1_day2[[6]]

dim(sheep21_collar_d2)
dim(Sheep22_collar_d2)
dim(Sheep23_collar_d2)
dim(Sheep24_collar_d2)
dim(Sheep25_collar_d2)
dim(Sheep26_collar_d2)


############################################################################################################################
####################           merege time step data and logged data                     ##################################
#############################################################################################################################                  


glimpse(df_sec1) #16,645 data pts df is reg time step
glimpse(sheep21_collar_d2) # 164 data pts for Sheep_21_collar_d1


#step 1 join
sheep_21_collar_d2_time_step1 <- full_join(df_sec1, sheep21_collar_d2, by = "hms")
Sheep_22_collar_d2_time_step1 <- full_join(df_sec1, Sheep22_collar_d2, by = "hms")
Sheep_23_collar_d2_time_step1 <- full_join(df_sec1, Sheep23_collar_d2, by = "hms")
Sheep_24_collar_d2_time_step1 <- full_join(df_sec1, Sheep24_collar_d2, by = "hms")
Sheep_25_collar_d2_time_step1 <- full_join(df_sec1, Sheep25_collar_d2, by = "hms")
Sheep_26_collar_d2_time_step1 <- full_join(df_sec1, Sheep26_collar_d2, by = "hms")

#dim(sheep_21_collar_d1_time_step1)
#colSums(is.na(sheep_21_collar_d1_time_step1))

#step 2 fill in missing values 
Sheep_21_collar_d2_time_step <- fill(sheep_21_collar_d2_time_step1,  .direction = "down", everything())
Sheep_22_collar_d2_time_step <- fill(Sheep_22_collar_d2_time_step1,  .direction = "down",everything())
Sheep_23_collar_d2_time_step <- fill(Sheep_23_collar_d2_time_step1,  .direction = "down",everything())
Sheep_24_collar_d2_time_step <- fill(Sheep_24_collar_d2_time_step1,  .direction = "down",everything())
Sheep_25_collar_d2_time_step <- fill(Sheep_25_collar_d2_time_step1,  .direction = "down",everything())
Sheep_26_collar_d2_time_step <- fill(Sheep_26_collar_d2_time_step1,  .direction = "down",everything())

#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#step 2 fill in missing values that are up 
Sheep_21_collar_d2_time_step <- fill(Sheep_21_collar_d2_time_step,  .direction = "up", everything())
Sheep_22_collar_d2_time_step <- fill(Sheep_22_collar_d2_time_step,  .direction = "up", everything())
Sheep_23_collar_d2_time_step <- fill(Sheep_23_collar_d2_time_step,  .direction = "up", everything())
Sheep_24_collar_d2_time_step <- fill(Sheep_24_collar_d2_time_step,  .direction = "up", everything())
Sheep_25_collar_d2_time_step <- fill(Sheep_25_collar_d2_time_step,  .direction = "up", everything())
Sheep_26_collar_d2_time_step <- fill(Sheep_26_collar_d2_time_step,  .direction = "up", everything())


#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#data enetry has a problem d1 should be day1
Sheep_21_collar_d1_time_step <- mutate(Sheep_21_collar_d1_time_step, name = "Sheep 21_collar_day1")


#remove the missing data rows 
colSums(is.na(Sheep_21_collar_d2_time_step)) #0 missing records
colSums(is.na(Sheep_22_collar_d2_time_step))
colSums(is.na(Sheep_23_collar_d2_time_step))
colSums(is.na(Sheep_24_collar_d1_time_step))
colSums(is.na(Sheep_25_collar_d2_time_step))
colSums(is.na(Sheep_26_collar_d2_time_step))


############################################################################################################################
####################          export my data I have created                                ##################################
############################################################################################################################# 

#EF_week1

dim(Sheep_21_collar_d2_time_step) #16,645
dim(Sheep_22_collar_d2_time_step) #16,645
dim(Sheep_23_collar_d2_time_step)
dim(Sheep_24_collar_d2_time_step)
dim(Sheep_25_collar_d2_time_step)
dim(Sheep_26_collar_d2_time_step)


EF_week1_d2_time_step <- rbind(Sheep_21_collar_d2_time_step,
                               Sheep_22_collar_d2_time_step,
                               Sheep_23_collar_d2_time_step,
                               Sheep_24_collar_d2_time_step,
                               Sheep_25_collar_d2_time_step,
                               Sheep_26_collar_d2_time_step)


write_csv(EF_week1_d2_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/EF_week1_d2_time_step.csv")






############################################################################################################################
####################           split logged data into df for each sheep   day 3           ##################################
#############################################################################################################################

glimpse(EF_Group_1_d3_clipped) # 1,310 data pts for all sheep

#this splits my data frame into lists 
split_sheep_EF_wk1_day3 <- split(EF_Group_1_d3_clipped, EF_Group_1_d3_clipped$sheep, drop = FALSE)
glimpse(split_sheep_EF_wk1_day3) #List of 6

#list of names I want my data frames to be 
names_sheep_EF_wk1_day3 <- group_by(EF_Group_1_d3_clipped,  name) %>% 
  count()
print(names_sheep_EF_wk1_day3)


#subet each list in split_sheep to make df

sheep21_collar_d3 <- split_sheep_EF_wk1_day3[[1]]
Sheep22_collar_d3 <- split_sheep_EF_wk1_day3[[2]]  
Sheep23_collar_d3 <- split_sheep_EF_wk1_day3[[3]]  
Sheep24_collar_d3 <- split_sheep_EF_wk1_day3[[4]]
Sheep25_collar_d3 <- split_sheep_EF_wk1_day3[[5]]
Sheep26_collar_d3 <- split_sheep_EF_wk1_day3[[6]]

dim(sheep21_collar_d3)
dim(Sheep22_collar_d3)
dim(Sheep23_collar_d3)
dim(Sheep24_collar_d3)
dim(Sheep25_collar_d3)
dim(Sheep26_collar_d3)


############################################################################################################################
####################           merege time step data and logged data                     ##################################
#############################################################################################################################                  


glimpse(df_sec1) #16,645 data pts df is reg time step


#step 1 join
sheep_21_collar_d3_time_step1 <- full_join(df_sec1, sheep21_collar_d3, by = "hms")
Sheep_22_collar_d3_time_step1 <- full_join(df_sec1, Sheep22_collar_d3, by = "hms")
Sheep_23_collar_d3_time_step1 <- full_join(df_sec1, Sheep23_collar_d3, by = "hms")
Sheep_24_collar_d3_time_step1 <- full_join(df_sec1, Sheep24_collar_d3, by = "hms")
Sheep_25_collar_d3_time_step1 <- full_join(df_sec1, Sheep25_collar_d3, by = "hms")
Sheep_26_collar_d3_time_step1 <- full_join(df_sec1, Sheep26_collar_d3, by = "hms")

dim(sheep_21_collar_d3_time_step1)
#colSums(is.na(sheep_21_collar_d1_time_step1))

#step 2 fill in missing values 
Sheep_21_collar_d3_time_step <- fill(sheep_21_collar_d3_time_step1,  .direction = "down", everything())
Sheep_22_collar_d3_time_step <- fill(Sheep_22_collar_d3_time_step1,  .direction = "down",everything())
Sheep_23_collar_d3_time_step <- fill(Sheep_23_collar_d3_time_step1,  .direction = "down",everything())
Sheep_24_collar_d3_time_step <- fill(Sheep_24_collar_d3_time_step1,  .direction = "down",everything())
Sheep_25_collar_d3_time_step <- fill(Sheep_25_collar_d3_time_step1,  .direction = "down",everything())
Sheep_26_collar_d3_time_step <- fill(Sheep_26_collar_d3_time_step1,  .direction = "down",everything())

#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#step 2 fill in missing values that are up 
Sheep_21_collar_d3_time_step <- fill(Sheep_21_collar_d3_time_step,  .direction = "up", everything())
Sheep_22_collar_d3_time_step <- fill(Sheep_22_collar_d3_time_step,  .direction = "up", everything())
Sheep_23_collar_d3_time_step <- fill(Sheep_23_collar_d3_time_step,  .direction = "up", everything())
Sheep_24_collar_d3_time_step <- fill(Sheep_24_collar_d3_time_step,  .direction = "up", everything())
Sheep_25_collar_d3_time_step <- fill(Sheep_25_collar_d3_time_step,  .direction = "up", everything())
Sheep_26_collar_d3_time_step <- fill(Sheep_26_collar_d3_time_step,  .direction = "up", everything())


#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#data enetry has a problem d1 should be day1
#Sheep_21_collar_d3_time_step <- mutate(Sheep_21_collar_d1_time_step, name = "Sheep 21_collar_day3")


#remove the missing data rows 
colSums(is.na(Sheep_21_collar_d3_time_step)) #0 missing records
colSums(is.na(Sheep_22_collar_d3_time_step))
colSums(is.na(Sheep_23_collar_d3_time_step))
colSums(is.na(Sheep_24_collar_d3_time_step))
colSums(is.na(Sheep_25_collar_d3_time_step))
colSums(is.na(Sheep_26_collar_d3_time_step))


############################################################################################################################
####################          export my data I have created                                ##################################
############################################################################################################################# 

#EF_week1

dim(Sheep_21_collar_d3_time_step) #16,645
dim(Sheep_22_collar_d3_time_step) #16,645
dim(Sheep_23_collar_d3_time_step)
dim(Sheep_24_collar_d3_time_step)
dim(Sheep_25_collar_d3_time_step)
dim(Sheep_26_collar_d3_time_step)

EF_week1_d3_time_step <- rbind(Sheep_21_collar_d3_time_step,
                               Sheep_22_collar_d3_time_step,
                               Sheep_23_collar_d3_time_step,
                               Sheep_24_collar_d3_time_step,
                               Sheep_25_collar_d3_time_step,
                               Sheep_26_collar_d3_time_step)


write_csv(EF_week1_d3_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/EF_week1_d3_time_step.csv")


write_csv(Sheep_21_collar_d3_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/Sheep_21_collar_d3_time_step.csv")


dim(EF_week1_d3_time_step)
6*16645



############################################################################################################################
####################           split logged data into df for each sheep   day 4           ##################################
#############################################################################################################################

glimpse(EF_Group_1_d4_clipped) # 1,473 data pts for all sheep

#this splits my data frame into lists 
split_sheep_EF_wk1_day4 <- split(EF_Group_1_d4_clipped, EF_Group_1_d4_clipped$sheep, drop = FALSE)
glimpse(split_sheep_EF_wk1_day4) #List of 6

#list of names I want my data frames to be 
names_sheep_EF_wk1_day4 <- group_by(EF_Group_1_d4_clipped,  name) %>% 
  count()
print(names_sheep_EF_wk1_day4)


#subet each list in split_sheep to make df

sheep21_collar_d4 <- split_sheep_EF_wk1_day4[[1]]
Sheep22_collar_d4 <- split_sheep_EF_wk1_day4[[2]]  
Sheep23_collar_d4 <- split_sheep_EF_wk1_day4[[3]]  
Sheep24_collar_d4 <- split_sheep_EF_wk1_day4[[4]]
Sheep25_collar_d4 <- split_sheep_EF_wk1_day4[[5]]
Sheep26_collar_d4 <- split_sheep_EF_wk1_day4[[6]]

dim(sheep21_collar_d4)
dim(Sheep22_collar_d4)
dim(Sheep23_collar_d4)
dim(Sheep24_collar_d4)
dim(Sheep25_collar_d4)
dim(Sheep26_collar_d4)


############################################################################################################################
####################           merege time step data and logged data                     ##################################
#############################################################################################################################                  


glimpse(df_sec1) #16,645 data pts df is reg time step


#step 1 join
sheep_21_collar_d4_time_step1 <- full_join(df_sec1, sheep21_collar_d4, by = "hms")
Sheep_22_collar_d4_time_step1 <- full_join(df_sec1, Sheep22_collar_d4, by = "hms")
Sheep_23_collar_d4_time_step1 <- full_join(df_sec1, Sheep23_collar_d4, by = "hms")
Sheep_24_collar_d4_time_step1 <- full_join(df_sec1, Sheep24_collar_d4, by = "hms")
Sheep_25_collar_d4_time_step1 <- full_join(df_sec1, Sheep25_collar_d4, by = "hms")
Sheep_26_collar_d4_time_step1 <- full_join(df_sec1, Sheep26_collar_d4, by = "hms")

dim(sheep_21_collar_d4_time_step1)
#colSums(is.na(sheep_21_collar_d1_time_step1))

#step 2 fill in missing values 
Sheep_21_collar_d4_time_step <- fill(sheep_21_collar_d4_time_step1,  .direction = "down", everything())
Sheep_22_collar_d4_time_step <- fill(Sheep_22_collar_d4_time_step1,  .direction = "down",everything())
Sheep_23_collar_d4_time_step <- fill(Sheep_23_collar_d4_time_step1,  .direction = "down",everything())
Sheep_24_collar_d4_time_step <- fill(Sheep_24_collar_d4_time_step1,  .direction = "down",everything())
Sheep_25_collar_d4_time_step <- fill(Sheep_25_collar_d4_time_step1,  .direction = "down",everything())
Sheep_26_collar_d4_time_step <- fill(Sheep_26_collar_d4_time_step1,  .direction = "down",everything())

#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#step 2 fill in missing values that are up 
Sheep_21_collar_d4_time_step <- fill(Sheep_21_collar_d4_time_step,  .direction = "up", everything())
Sheep_22_collar_d4_time_step <- fill(Sheep_22_collar_d4_time_step,  .direction = "up", everything())
Sheep_23_collar_d4_time_step <- fill(Sheep_23_collar_d4_time_step,  .direction = "up", everything())
Sheep_24_collar_d4_time_step <- fill(Sheep_24_collar_d4_time_step,  .direction = "up", everything())
Sheep_25_collar_d4_time_step <- fill(Sheep_25_collar_d4_time_step,  .direction = "up", everything())
Sheep_26_collar_d4_time_step <- fill(Sheep_26_collar_d4_time_step,  .direction = "up", everything())


#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#data enetry has a problem d1 should be day1
#Sheep_21_collar_d3_time_step <- mutate(Sheep_21_collar_d1_time_step, name = "Sheep 21_collar_day3")


#remove the missing data rows 
colSums(is.na(Sheep_21_collar_d4_time_step)) #0 missing records
colSums(is.na(Sheep_22_collar_d4_time_step))
colSums(is.na(Sheep_23_collar_d4_time_step))
colSums(is.na(Sheep_24_collar_d4_time_step))
colSums(is.na(Sheep_25_collar_d4_time_step))
colSums(is.na(Sheep_26_collar_d4_time_step))


############################################################################################################################
####################          export my data I have created                                ##################################
############################################################################################################################# 

#EF_week1

dim(Sheep_21_collar_d4_time_step) #16,645
dim(Sheep_22_collar_d4_time_step) #16,645
dim(Sheep_23_collar_d4_time_step)
dim(Sheep_24_collar_d4_time_step)
dim(Sheep_25_collar_d4_time_step)
dim(Sheep_26_collar_d4_time_step)

EF_week1_d4_time_step <- rbind(Sheep_21_collar_d4_time_step,
                               Sheep_22_collar_d4_time_step,
                               Sheep_23_collar_d4_time_step,
                               Sheep_24_collar_d4_time_step,
                               Sheep_25_collar_d4_time_step,
                               Sheep_26_collar_d4_time_step)


write_csv(EF_week1_d4_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/EF_week1/EF_week1_d4_time_step.csv")

