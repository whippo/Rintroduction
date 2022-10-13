#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# TITLE                                                                          ##
# Data are current as of yyyy-mm-dd                                              ##
# Data source: NAME/ORG                                                          ##
# R code prepared by NAME                                                        ##
# Last updated yyyy-mm-dd                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# FILE.csv

# Associated Scripts:
# FILE.R

# TO DO 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MANIPULATE DATA                                                                 +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# script created 2022-10-12

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(vegan)
library(lme4)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SP_CSWG_2022_urchin_measurements <- read_csv("SP CSWG_2022_urchin-measurements.csv", 
                                             col_types = cols(date = col_character(), 
                                            measurer = col_factor(levels = c("Galloway", "Taradash", "Persad")), 
                                            `5m_area_bin` = col_factor(levels = c("first", "second", "third", "fourth")),
                                             method = col_factor(levels = c("analog", "laser", "digital"))))

HG_count_variability <- read_csv("HG.count.variability.csv", col_types = cols(expertise = col_factor(levels = c("novice", "advanced", "expert"))))

Count_summary <- HG_count_variability %>%
  select(image.name, analyst, expertise, red, purple, green) %>%
  pivot_longer(red:green, names_to = "species", values_to = "count")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Figures                                                                      ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SIZE DATA

ggplot(SP_CSWG_2022_urchin_measurements, aes(x = test_diam_mm, y = method)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Test Diameter (mm)", y = "Method")

ggplot(SP_CSWG_2022_urchin_measurements, aes(x = test_diam_mm, y = measurer)) +
  geom_boxplot() +
  facet_grid(side~.) +
  labs(x = "Test Diameter (mm)", y = "Observer")

ggplot(SP_CSWG_2022_urchin_measurements, aes(x = test_diam_mm)) +
  geom_histogram() +
  facet_grid(method~`5m_area_bin`) +
  labs(x = "Test Diameter (mm)", y = "Count")

ggplot(SP_CSWG_2022_urchin_measurements, aes(x = method, y = test_diam_mm)) +
  geom_violin() + 
  geom_jitter(width = 0.15) +
  labs(y = "Test Diameter (mm)", x = "Method")

# COUNT DATA

ggplot(Count_summary, aes(x = expertise, y = count)) +
  geom_col() +
  facet_grid(.~species) +
  theme_minimal()

count_test <- Count_summary %>%
  group_by(expertise, species) %>%
  summarise(sum(count, na.rm = TRUE))

count_test <- Count_summary %>%
  group_by(species) %>%
  filter(expertise == "novice") %>%
  summarise(sum(count))

  unique(Count_summary$expertise)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analyses                                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# summarise count data for lm
  count_lm_data <- Count_summary %>%
    separate(image.name, into = c("transect", NA)) %>%
    select(transect, analyst, species, count)

# LM
  count_lm <- lm(count ~ transect + analyst, data = count_lm_data)
  summary(count_lm)
  
# visualize 
  count_observer_group <- count_lm_data %>%
    group_by(transect, analyst) %>%
    summarise(total = sum(count, na.rm = TRUE))
  
  ggplot(count_observer_group, aes(x = analyst, y = total)) +
    geom_jitter(col = "grey", size = 3, width = 0.15) +
    scale_color_viridis(discrete = TRUE,
                        begin = 0.3,
                        end = 0.7,
                        option = "magma") +
    stat_summary(
      geom = "point",
      fun = "mean",
      size = 6,
      shape = 19
    ) +
    geom_errorbar(stat="summary", fun.data="mean_se", size = 1)
  
  

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####