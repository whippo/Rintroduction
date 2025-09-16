#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Sasquatch Exploratory                                                       ##
# Data are current as of 2024-10-17                                           ##
# Data source: https://www.sasquatchdataproject.com/the-dataset               ##
# R code prepared by Ross Whippo                                              ##  
# Last updated 2025-09-16                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# sasquatch.csv

# Associated Files:
# .R

# TO DO 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sasquatch <- read_csv("WorkshopData/sasquatch.csv")

sasquatch

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# how many sightings by state? 
sasquatch |>
  ggplot(aes(x = State)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# table
sas_table <- sasquatch |>
  count(State)

# how many sightings by year?
sasquatch |>
  ggplot(aes(x = Year)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) 

# line graph
year_counts <- sasquatch |>
  count(Year) 
  
year_counts |>
  ggplot(aes(x = Year, y = n)) +
  geom_point() +
  geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) 

# by time

sas_time <- sasquatch |>
  mutate(time_obj = hms(Time),
         time_seconds = as.numeric(time_obj)) |>
  group_by(State) |>
  summarise(average_seconds = mean(time_seconds, na.rm = TRUE)) |>
  mutate(average_time = seconds_to_period(average_seconds))

state_count <- sasquatch |>
  count(State)

time_count <- sas_time |>
  left_join(state_count)

time_count |>
  ggplot(aes(x = State, y = n, fill = average_seconds)) +
  geom_col() +
  scale_fill_viridis(name = "Avg. Time of Day",
                     breaks = c(21000, 70200),
                     labels = c("6am", "7pm")) +
  theme_bw() +
  ylab("Number of Sightings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####