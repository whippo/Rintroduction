#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# SBB Seagrass Data Exploration                                               ##
# Data source: Kasitsna Bay Lab - NCCOS - NOAA                                ##
# R code prepared by Ross Whippo                                              ##
# Script Created 2025-09-15                                                   ##
# Last updated 2025-09-15                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# R exploration of seagrass density data in Mud Bay, Kachemak Bay, AK for 
# Semester By The Bay Students Fall 2025.

# Required Files (check that script is loading latest version):
# seagrass_data_SBB2025.csv

# Associated Files:
# .proj

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES & PREPARE WORKSPACE                                         ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# install.packages("tidyverse")
# install.packages("viridis")
# clear any existing data in the environment
# rm(list = ls())

library(tidyverse) # tidyverse is a collection of packages for data manipulation
library(viridis) # viridis is a color-blind friendly palette for graphs

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

seagrass_data_SBB2025 <- read_csv("git/Rintroduction/WorkshopData/seagrass_data_SBB2025.csv", 
                                  col_types = cols(Date = col_date(format = "%m/%d/%Y")))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

seagrass_data_SBB2025
view(seagrass_data_SBB2025)
head(seagrass_data_SBB2025)
tail(seagrass_data_SBB2025)

str(seagrass_data_SBB2025)
glimpse(seagrass_data_SBB2025)

seagrass_data_SBB2025$Taxon_ID
seagrass_data_SBB2025[10,]
seagrass_data_SBB2025[,10]
seagrass_data_SBB2025[10,10]

zmarina <- seagrass_data_SBB2025 |>
  filter(Taxon_ID == "Z. marina")

write_csv(zmarina, "WorkshopData/zmarina.csv")


mean(zmarina$Length_cm, na.rm = TRUE)

zmarina |>
  drop_na(Length_cm) |>
  group_by(Quadrat_Num) |>
  summarise(mean(Length_cm))

zmarina |>  
  ggplot(aes(x = Quadrat_Num,
             y = Length_cm, 
             group = Quadrat_Num)) +
  geom_boxplot() +
  theme_bw()

zmarina_small <- zmarina |>
  select(Quadrat_Num, Length_cm) |>
  mutate(Quadrat_Num = as.character(Quadrat_Num)) |>
  filter(Quadrat_Num == c(1, 4))


datax <- rnorm(n = 25,
               mean = 5,
               sd = 3)

datay <- rnorm(n = 25,
               mean = 9,
               sd = 4)

t.test(datay, datax)

zmarina |>
  ggplot(aes(x = Quadrat_Num, 
             y = Width_cm, 
             group = Quadrat_Num,
             color = Length_cm)) +
  geom_jitter(width = 0.3,
              height = 0.02,
              size = 3) +
  scale_color_viridis(option = "F") +
  ylab("Quadrat Number") +
  xlab("Width (cm)") +
  theme_bw() +
  labs(color = "Length (cm)")


#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####
# some analysis

df <- read_csv(thisdata)


newthing <- mutate(df, this, that, theother)
newnewthing <- sort(newthing, this, that, theother)
n4thing <- filter(newnewthing, this, that, theother)
plot(df, this, that, theother)


newdf <- df |>
  mutate(this,that,theother) |>
  sort(this,that,theother) |>
  filter(thisthat) |>
  plot()

df <- exp(5*4+44) 

newdf <- df |>
  sqrt() |>
  log()

install.packages("tidyverse")

library(tidyverse)

install.packages("viridis")


