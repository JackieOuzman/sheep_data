library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)

library(ggplot2)
library(readr)




############################################################################################################################
####################          week1  day 1-4  VF_Group_1_clipped               ##################################
#############################################################################################################################


VF_Group_3_d1_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_3_d1_clipped.txt")


VF_Group_3_d2_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_3_d2_clipped.txt")
VF_Group_3_d3_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_3_d3_clipped.txt")
VF_Group_3_d4_clipped <- read_csv("W:/VF/pasture_utilisation/Take2/data_for_clipping/VF_Group_3_d4_clipped.txt")


VF_Group_3_clipped <- rbind(VF_Group_3_d1_clipped,
                            VF_Group_3_d2_clipped,
                            VF_Group_3_d3_clipped,
                            VF_Group_3_d4_clipped)



############################################################################################################################
#####################                    Min time for data set  EF_Group_1_clipped day 1 -4#########################################
############################################################################################################################

min_time1 <- (min(VF_Group_3_clipped$hms))
print(min_time1) 
print(hms::as.hms(min_time1) )
min_time <- hms::as.hms(min_time1)
print(min_time) 
date_time_start <- as_datetime(paste0("2018-10-29 ", min_time))

############################################################################################################################
####################                    Max time for data set  EF_Group_1_clipped   day 1 -4#########################################
############################################################################################################################
max_time1 <- (max(VF_Group_3_clipped$hms))
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

glimpse(VF_Group_3_d1_clipped) #   787 data pts for all sheep

#this splits my data frame into lists 
split_sheep_VF_wk3_day1 <- split(VF_Group_3_d1_clipped, 
                                 VF_Group_3_d1_clipped$sheep, drop = FALSE)
glimpse(split_sheep_VF_wk3_day1) #List of 6

#list of names I want my data frames to be 
sheep_names_VF_Group_3_d1 <- group_by(VF_Group_3_d1_clipped,  name) %>% 
  count()
print(sheep_names_VF_Group_3_d1)


#subet each list in split_sheep to make df

sheep10_collar_d1 <- split_sheep_VF_wk3_day1[[1]]
Sheep18_collar_d1 <- split_sheep_VF_wk3_day1[[2]]  
Sheep19_collar_d1 <- split_sheep_VF_wk3_day1[[3]]  
Sheep03_collar_d1 <- split_sheep_VF_wk3_day1[[4]]
Sheep05_collar_d1 <- split_sheep_VF_wk3_day1[[5]]
Sheepst_collar_d1 <- split_sheep_VF_wk3_day1[[6]]

dim(sheep10_collar_d1)
dim(Sheep18_collar_d1)
dim(Sheep19_collar_d1)
dim(Sheep03_collar_d1)
dim(Sheep05_collar_d1)
dim(Sheepst_collar_d1)


############################################################################################################################
####################           merege time step data and logged data      day 1           ##################################
#############################################################################################################################                  


glimpse(df_sec1) #16,645 data pts df is reg time step



#step 1 join
sheep_10_collar_d1_time_step1 <- full_join(df_sec1, sheep10_collar_d1, by = "hms")
Sheep_18_collar_d1_time_step1 <- full_join(df_sec1, Sheep18_collar_d1, by = "hms")
Sheep_19_collar_d1_time_step1 <- full_join(df_sec1, Sheep19_collar_d1, by = "hms")
Sheep_03_collar_d1_time_step1 <- full_join(df_sec1, Sheep03_collar_d1, by = "hms")
Sheep_05_collar_d1_time_step1 <- full_join(df_sec1, Sheep05_collar_d1, by = "hms")
Sheep_st_collar_d1_time_step1 <- full_join(df_sec1, Sheepst_collar_d1, by = "hms")

#dim(sheep_21_collar_d1_time_step1)
#colSums(is.na(sheep_21_collar_d1_time_step1))

#step 2 fill in missing values 
Sheep_10_collar_d1_time_step <- fill(sheep_10_collar_d1_time_step1,  .direction = "down", everything())
Sheep_18_collar_d1_time_step <- fill(Sheep_18_collar_d1_time_step1,  .direction = "down",everything())
Sheep_19_collar_d1_time_step <- fill(Sheep_19_collar_d1_time_step1,  .direction = "down",everything())
Sheep_03_collar_d1_time_step <- fill(Sheep_03_collar_d1_time_step1,  .direction = "down",everything())
Sheep_05_collar_d1_time_step <- fill(Sheep_05_collar_d1_time_step1,  .direction = "down",everything())
Sheep_st_collar_d1_time_step <- fill(Sheep_st_collar_d1_time_step1,  .direction = "down",everything())

#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#step 2 fill in missing values that are up 
Sheep_10_collar_d1_time_step <- fill(Sheep_10_collar_d1_time_step,  .direction = "up", everything())
Sheep_18_collar_d1_time_step <- fill(Sheep_18_collar_d1_time_step,  .direction = "up", everything())
Sheep_19_collar_d1_time_step <- fill(Sheep_19_collar_d1_time_step,  .direction = "up", everything())
Sheep_03_collar_d1_time_step <- fill(Sheep_03_collar_d1_time_step,  .direction = "up", everything())
Sheep_05_collar_d1_time_step <- fill(Sheep_05_collar_d1_time_step,  .direction = "up", everything())
Sheep_st_collar_d1_time_step <- fill(Sheep_st_collar_d1_time_step,  .direction = "up", everything())


#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#data enetry has a problem d1 should be day1
#Sheep_21_collar_d1_time_step <- mutate(Sheep_21_collar_d1_time_step, name = "Sheep 21_collar_day1")


#remove the missing data rows 
colSums(is.na(Sheep_10_collar_d1_time_step)) #0 missing records
colSums(is.na(Sheep_18_collar_d1_time_step))
colSums(is.na(Sheep_19_collar_d1_time_step))
colSums(is.na(Sheep_03_collar_d1_time_step))
colSums(is.na(Sheep_05_collar_d1_time_step))
colSums(is.na(Sheep_st_collar_d1_time_step))


############################################################################################################################
####################          export my data I have created         day 1                  ##################################
############################################################################################################################# 


VF_week3_d1_time_step <- rbind(Sheep_10_collar_d1_time_step,
                               Sheep_18_collar_d1_time_step,
                               Sheep_19_collar_d1_time_step,
                               Sheep_03_collar_d1_time_step,
                               Sheep_05_collar_d1_time_step,
                               Sheep_st_collar_d1_time_step)


write_csv(VF_week3_d1_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/VF_week3/VF_week3_d1_time_step.csv")



############################################################################################################################
####################           split logged data into df for each sheep       Day 2       ##################################
#############################################################################################################################

glimpse(VF_Group_3_d2_clipped) #   1,910 data pts for all sheep

#this splits my data frame into lists 
split_sheep_VF_wk3_day2 <- split(VF_Group_3_d2_clipped, 
                                 VF_Group_3_d2_clipped$sheep, drop = FALSE)
glimpse(split_sheep_VF_wk3_day2) #List of 6

#list of names I want my data frames to be 
sheep_names_VF_Group_3_d2 <- group_by(VF_Group_3_d2_clipped,  name) %>% 
  count()
print(sheep_names_VF_Group_3_d2)


#subet each list in split_sheep to make df

sheep10_collar_d2 <- split_sheep_VF_wk3_day2[[1]]
Sheep18_collar_d2 <- split_sheep_VF_wk3_day2[[2]]  
Sheep19_collar_d2 <- split_sheep_VF_wk3_day2[[3]]  
Sheep03_collar_d2 <- split_sheep_VF_wk3_day2[[4]]
Sheep05_collar_d2 <- split_sheep_VF_wk3_day2[[5]]
Sheepst_collar_d2 <- split_sheep_VF_wk3_day2[[6]]

dim(sheep10_collar_d2)
dim(Sheep18_collar_d2)
dim(Sheep19_collar_d2)
dim(Sheep03_collar_d2)
dim(Sheep05_collar_d2)
dim(Sheepst_collar_d2)


############################################################################################################################
####################           merege time step data and logged data      day 2           ##################################
#############################################################################################################################                  


glimpse(df_sec1) #16,645 data pts df is reg time step



#step 1 join
sheep_10_collar_d2_time_step1 <- full_join(df_sec1, sheep10_collar_d2, by = "hms")
Sheep_18_collar_d2_time_step1 <- full_join(df_sec1, Sheep18_collar_d2, by = "hms")
Sheep_19_collar_d2_time_step1 <- full_join(df_sec1, Sheep19_collar_d2, by = "hms")
Sheep_03_collar_d2_time_step1 <- full_join(df_sec1, Sheep03_collar_d2, by = "hms")
Sheep_05_collar_d2_time_step1 <- full_join(df_sec1, Sheep05_collar_d2, by = "hms")
Sheep_st_collar_d2_time_step1 <- full_join(df_sec1, Sheepst_collar_d2, by = "hms")

#dim(sheep_21_collar_d1_time_step1)
#colSums(is.na(sheep_21_collar_d1_time_step1))

#step 2 fill in missing values 
Sheep_10_collar_d2_time_step <- fill(sheep_10_collar_d2_time_step1,  .direction = "down", everything())
Sheep_18_collar_d2_time_step <- fill(Sheep_18_collar_d2_time_step1,  .direction = "down",everything())
Sheep_19_collar_d2_time_step <- fill(Sheep_19_collar_d2_time_step1,  .direction = "down",everything())
Sheep_03_collar_d2_time_step <- fill(Sheep_03_collar_d2_time_step1,  .direction = "down",everything())
Sheep_05_collar_d2_time_step <- fill(Sheep_05_collar_d2_time_step1,  .direction = "down",everything())
Sheep_st_collar_d2_time_step <- fill(Sheep_st_collar_d2_time_step1,  .direction = "down",everything())

#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#step 2 fill in missing values that are up 
Sheep_10_collar_d2_time_step <- fill(Sheep_10_collar_d2_time_step,  .direction = "up", everything())
Sheep_18_collar_d2_time_step <- fill(Sheep_18_collar_d2_time_step,  .direction = "up", everything())
Sheep_19_collar_d2_time_step <- fill(Sheep_19_collar_d2_time_step,  .direction = "up", everything())
Sheep_03_collar_d2_time_step <- fill(Sheep_03_collar_d2_time_step,  .direction = "up", everything())
Sheep_05_collar_d2_time_step <- fill(Sheep_05_collar_d2_time_step,  .direction = "up", everything())
Sheep_st_collar_d2_time_step <- fill(Sheep_st_collar_d2_time_step,  .direction = "up", everything())


#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#data enetry has a problem d1 should be day1
#Sheep_21_collar_d1_time_step <- mutate(Sheep_21_collar_d1_time_step, name = "Sheep 21_collar_day1")


#remove the missing data rows 
colSums(is.na(Sheep_10_collar_d2_time_step)) #0 missing records
colSums(is.na(Sheep_18_collar_d2_time_step))
colSums(is.na(Sheep_19_collar_d2_time_step))
colSums(is.na(Sheep_03_collar_d2_time_step))
colSums(is.na(Sheep_05_collar_d2_time_step))
colSums(is.na(Sheep_st_collar_d2_time_step))


############################################################################################################################
####################          export my data I have created         day 2                  ##################################
############################################################################################################################# 


VF_week3_d2_time_step <- rbind(Sheep_10_collar_d2_time_step,
                               Sheep_18_collar_d2_time_step,
                               Sheep_19_collar_d2_time_step,
                               Sheep_03_collar_d2_time_step,
                               Sheep_05_collar_d2_time_step,
                               Sheep_st_collar_d2_time_step)


write_csv(VF_week3_d2_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/VF_week3/VF_week3_d2_time_step.csv")








############################################################################################################################
####################           split logged data into df for each sheep       Day 3       ##################################
#############################################################################################################################

glimpse(VF_Group_3_d3_clipped) #   1,143 data pts for all sheep

#this splits my data frame into lists 
split_sheep_VF_wk3_day3 <- split(VF_Group_3_d3_clipped, 
                                 VF_Group_3_d3_clipped$sheep, drop = FALSE)
glimpse(split_sheep_VF_wk3_day3) #List of 6

#list of names I want my data frames to be 
sheep_names_VF_Group_3_d3 <- group_by(VF_Group_3_d3_clipped,  name) %>% 
  count()
print(sheep_names_VF_Group_3_d3)


#subet each list in split_sheep to make df

sheep10_collar_d3 <- split_sheep_VF_wk3_day3[[1]]
Sheep18_collar_d3 <- split_sheep_VF_wk3_day3[[2]]  
Sheep19_collar_d3 <- split_sheep_VF_wk3_day3[[3]]  
Sheep03_collar_d3 <- split_sheep_VF_wk3_day3[[4]]
Sheep05_collar_d3 <- split_sheep_VF_wk3_day3[[5]]
Sheepst_collar_d3 <- split_sheep_VF_wk3_day3[[6]]

dim(sheep10_collar_d3)
dim(Sheep18_collar_d3)
dim(Sheep19_collar_d3)
dim(Sheep03_collar_d3)
dim(Sheep05_collar_d3)
dim(Sheepst_collar_d3)


############################################################################################################################
####################           merege time step data and logged data      day 3           ##################################
#############################################################################################################################                  


glimpse(df_sec1) #16,645 data pts df is reg time step



#step 1 join
sheep_10_collar_d3_time_step1 <- full_join(df_sec1, sheep10_collar_d3, by = "hms")
Sheep_18_collar_d3_time_step1 <- full_join(df_sec1, Sheep18_collar_d3, by = "hms")
Sheep_19_collar_d3_time_step1 <- full_join(df_sec1, Sheep19_collar_d3, by = "hms")
Sheep_03_collar_d3_time_step1 <- full_join(df_sec1, Sheep03_collar_d3, by = "hms")
Sheep_05_collar_d3_time_step1 <- full_join(df_sec1, Sheep05_collar_d3, by = "hms")
Sheep_st_collar_d3_time_step1 <- full_join(df_sec1, Sheepst_collar_d3, by = "hms")

#dim(sheep_21_collar_d1_time_step1)
#colSums(is.na(sheep_21_collar_d1_time_step1))

#step 2 fill in missing values 
Sheep_10_collar_d3_time_step <- fill(sheep_10_collar_d3_time_step1,  .direction = "down", everything())
Sheep_18_collar_d3_time_step <- fill(Sheep_18_collar_d3_time_step1,  .direction = "down",everything())
Sheep_19_collar_d3_time_step <- fill(Sheep_19_collar_d3_time_step1,  .direction = "down",everything())
Sheep_03_collar_d3_time_step <- fill(Sheep_03_collar_d3_time_step1,  .direction = "down",everything())
Sheep_05_collar_d3_time_step <- fill(Sheep_05_collar_d3_time_step1,  .direction = "down",everything())
Sheep_st_collar_d3_time_step <- fill(Sheep_st_collar_d3_time_step1,  .direction = "down",everything())

#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#step 2 fill in missing values that are up 
Sheep_10_collar_d3_time_step <- fill(Sheep_10_collar_d3_time_step,  .direction = "up", everything())
Sheep_18_collar_d3_time_step <- fill(Sheep_18_collar_d3_time_step,  .direction = "up", everything())
Sheep_19_collar_d3_time_step <- fill(Sheep_19_collar_d3_time_step,  .direction = "up", everything())
Sheep_03_collar_d3_time_step <- fill(Sheep_03_collar_d3_time_step,  .direction = "up", everything())
Sheep_05_collar_d3_time_step <- fill(Sheep_05_collar_d3_time_step,  .direction = "up", everything())
Sheep_st_collar_d3_time_step <- fill(Sheep_st_collar_d3_time_step,  .direction = "up", everything())


#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#data enetry has a problem d1 should be day1
#Sheep_21_collar_d1_time_step <- mutate(Sheep_21_collar_d1_time_step, name = "Sheep 21_collar_day1")


#remove the missing data rows 
colSums(is.na(Sheep_10_collar_d3_time_step)) #0 missing records
colSums(is.na(Sheep_18_collar_d3_time_step))
colSums(is.na(Sheep_19_collar_d3_time_step))
colSums(is.na(Sheep_03_collar_d3_time_step))
colSums(is.na(Sheep_05_collar_d3_time_step))
colSums(is.na(Sheep_st_collar_d3_time_step))


############################################################################################################################
####################          export my data I have created         day 3                  ##################################
############################################################################################################################# 


VF_week3_d3_time_step <- rbind(Sheep_10_collar_d3_time_step,
                               Sheep_18_collar_d3_time_step,
                               Sheep_19_collar_d3_time_step,
                               Sheep_03_collar_d3_time_step,
                               Sheep_05_collar_d3_time_step,
                               Sheep_st_collar_d3_time_step)


write_csv(VF_week3_d3_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/VF_week3/VF_week3_d3_time_step.csv")






############################################################################################################################
####################           split logged data into df for each sheep       Day 4       ##################################
#############################################################################################################################

glimpse(VF_Group_3_d4_clipped) #   1,080 data pts for all sheep

#this splits my data frame into lists 
split_sheep_VF_wk3_day4 <- split(VF_Group_3_d4_clipped, 
                                 VF_Group_3_d4_clipped$sheep, drop = FALSE)
glimpse(split_sheep_VF_wk3_day4) #List of 6

#list of names I want my data frames to be 
sheep_names_VF_Group_3_d4 <- group_by(VF_Group_3_d4_clipped,  name) %>% 
  count()
print(sheep_names_VF_Group_3_d4)


#subet each list in split_sheep to make df

sheep10_collar_d4 <- split_sheep_VF_wk3_day4[[1]]
Sheep18_collar_d4 <- split_sheep_VF_wk3_day4[[2]]  
Sheep19_collar_d4 <- split_sheep_VF_wk3_day4[[3]]  
Sheep03_collar_d4 <- split_sheep_VF_wk3_day4[[4]]
Sheep05_collar_d4 <- split_sheep_VF_wk3_day4[[5]]
Sheepst_collar_d4 <- split_sheep_VF_wk3_day4[[6]]

dim(sheep10_collar_d4)
dim(Sheep18_collar_d4)
dim(Sheep19_collar_d4)
dim(Sheep03_collar_d4)
dim(Sheep05_collar_d4)
dim(Sheepst_collar_d4)


############################################################################################################################
####################           merege time step data and logged data      day 3           ##################################
#############################################################################################################################                  


glimpse(df_sec1) #16,645 data pts df is reg time step



#step 1 join
sheep_10_collar_d4_time_step1 <- full_join(df_sec1, sheep10_collar_d4, by = "hms")
Sheep_18_collar_d4_time_step1 <- full_join(df_sec1, Sheep18_collar_d4, by = "hms")
Sheep_19_collar_d4_time_step1 <- full_join(df_sec1, Sheep19_collar_d4, by = "hms")
Sheep_03_collar_d4_time_step1 <- full_join(df_sec1, Sheep03_collar_d4, by = "hms")
Sheep_05_collar_d4_time_step1 <- full_join(df_sec1, Sheep05_collar_d4, by = "hms")
Sheep_st_collar_d4_time_step1 <- full_join(df_sec1, Sheepst_collar_d4, by = "hms")

#dim(sheep_21_collar_d1_time_step1)
#colSums(is.na(sheep_21_collar_d1_time_step1))

#step 2 fill in missing values 
Sheep_10_collar_d4_time_step <- fill(sheep_10_collar_d4_time_step1,  .direction = "down", everything())
Sheep_18_collar_d4_time_step <- fill(Sheep_18_collar_d4_time_step1,  .direction = "down",everything())
Sheep_19_collar_d4_time_step <- fill(Sheep_19_collar_d4_time_step1,  .direction = "down",everything())
Sheep_03_collar_d4_time_step <- fill(Sheep_03_collar_d4_time_step1,  .direction = "down",everything())
Sheep_05_collar_d4_time_step <- fill(Sheep_05_collar_d4_time_step1,  .direction = "down",everything())
Sheep_st_collar_d4_time_step <- fill(Sheep_st_collar_d4_time_step1,  .direction = "down",everything())

#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#step 2 fill in missing values that are up 
Sheep_10_collar_d4_time_step <- fill(Sheep_10_collar_d4_time_step,  .direction = "up", everything())
Sheep_18_collar_d4_time_step <- fill(Sheep_18_collar_d4_time_step,  .direction = "up", everything())
Sheep_19_collar_d4_time_step <- fill(Sheep_19_collar_d4_time_step,  .direction = "up", everything())
Sheep_03_collar_d4_time_step <- fill(Sheep_03_collar_d4_time_step,  .direction = "up", everything())
Sheep_05_collar_d4_time_step <- fill(Sheep_05_collar_d4_time_step,  .direction = "up", everything())
Sheep_st_collar_d4_time_step <- fill(Sheep_st_collar_d4_time_step,  .direction = "up", everything())


#dim(Sheep_21_collar_d1_time_step)
#colSums(is.na(Sheep_21_collar_d1_time_step))

#data enetry has a problem d1 should be day1
#Sheep_21_collar_d1_time_step <- mutate(Sheep_21_collar_d1_time_step, name = "Sheep 21_collar_day1")


#remove the missing data rows 
colSums(is.na(Sheep_10_collar_d4_time_step)) #0 missing records
colSums(is.na(Sheep_18_collar_d4_time_step))
colSums(is.na(Sheep_19_collar_d4_time_step))
colSums(is.na(Sheep_03_collar_d4_time_step))
colSums(is.na(Sheep_05_collar_d4_time_step))
colSums(is.na(Sheep_st_collar_d4_time_step))


############################################################################################################################
####################          export my data I have created         day 4                  ##################################
############################################################################################################################# 


VF_week3_d4_time_step <- rbind(Sheep_10_collar_d4_time_step,
                               Sheep_18_collar_d4_time_step,
                               Sheep_19_collar_d4_time_step,
                               Sheep_03_collar_d4_time_step,
                               Sheep_05_collar_d4_time_step,
                               Sheep_st_collar_d4_time_step)


write_csv(VF_week3_d4_time_step, "W:/VF/pasture_utilisation/Take2/reg_time_step/VF_week3/VF_week3_d4_time_step.csv")
