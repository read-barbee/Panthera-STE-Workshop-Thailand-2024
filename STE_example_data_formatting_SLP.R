#### Format example data for STE workshop ####

# Author: Read Barbee

# Date:2024-08-16 

# Purpose:


################################ Libraries #################################
library(tidyverse)
library(janitor)

###############################################################################

#1. Import Data

###############################################################################

raw <- read_csv("SCL/SLP/2022_SLP_Master_All_records_IDS_S3018_20221108_20230219.csv") %>% 
  clean_names()

cam_dat <- read_csv("SCL/SLP/SLP_2022_CameraDetails.csv") %>% 
  janitor::clean_names() %>% 
  select(camera_station, deployment_date, last_active_day_station) %>% 
  mutate(start_date = mdy(deployment_date),
         end_date = mdy(last_active_day_station)) %>% 
  select(-c(deployment_date, last_active_day_station)) %>% 
  rename(station = camera_station)


#remove duplicate rows with the same timestamp, species, and station
no_dupes <- raw %>% 
  select(station, species, date_time_original, group) %>% 
  distinct(station, species, date_time_original, .keep_all = T)

###############################################################################

#2. Convert to timelapse format

###############################################################################

#filter to boar only for example because it has the most detections
boar <- no_dupes %>% filter(species == "Boar_Wild")

#round all timestamps to the nearest hour
round <- boar %>% 
  mutate(date_time = ymd_hms(date_time_original)) %>% 
  mutate(date_time = round_date(date_time, unit = "hour")) %>% 
  select(station, species, date_time, group)

#remove duplicates of the same species, station, and timestamp
no_dupes2 <- round %>% 
  distinct(station, species, date_time, .keep_all = T)

#add column for camera area. set it as a default of 0.0000154 km2 from the example csv
area <- no_dupes2 %>% 
  mutate(area_m2 = 0.0000154 * 1e6)

#split the dataframe by station for looping
split <- area %>% split(area$station)

#initialize empty list
new_list <- list()

#for each camera station, create the full sequence of timestamps and join boar detections to them. fill the rest with 0s.
for(i in 1:length(split)){
  
  station_fill <- split[[i]]$station[1]
  start_date <- cam_dat %>% filter(station == station_fill) %>% pull(start_date)
  end_date <- cam_dat %>% filter(station == station_fill) %>% pull(end_date)
  start_dt <- ymd_hms(paste(as.character(start_date), "00:00:00"))
  end_dt <- ymd_hms(paste(as.character(end_date), "00:00:00")) + days(1)
  species_fill <- "Boar_Wild"
  area_fill <- 0.0000154 * 1e6
  group_size_fill <- 0
  
  ts_complete <- tibble(
      date_time = seq(from = start_dt, to = end_dt, by = "hour")
      )
  
  new_list[[i]] <- ts_complete %>% 
    left_join(split[[i]], by = "date_time") %>% 
    mutate(station = replace_na(station, station_fill),
           species = replace_na(species, species_fill),
           area_m2 = replace_na(area_m2, area_fill),
           group = replace_na(group, group_size_fill)) %>% 
    select(station, species, date_time, area_m2, group)
    
  
}

new_list <- bind_rows(new_list)


split2 <- new_list %>% split(new_list$group)


split2$`0` <- split2$`0` %>% 
  mutate(tf = rbinom(nrow(split2$`0`), 1, 0.95)) %>% 
  mutate(area_m2 = case_when(tf == 1 ~ area_m2,
                              tf == 0 ~ 0)) %>% 
  select(-tf)

split2$`1` <- split2$`1` %>% 
  mutate(tf = rbinom(nrow(split2$`1`), 1, 0.9)) %>% 
  mutate(group = case_when(tf == 1 ~ group,
                              tf == 0 ~ 2)) %>% 
  select(-tf)

split2$`1` <- split2$`1` %>% 
  mutate(tf = rbinom(nrow(split2$`1`), 1, 0.95)) %>% 
  mutate(group = case_when(tf == 1 ~ group,
                              tf == 0 ~ 3)) %>% 
  select(-tf)

tl_data <- bind_rows(split2)


#write_csv(tl_data, "boar_timelapse_example_data_SLP.csv")

