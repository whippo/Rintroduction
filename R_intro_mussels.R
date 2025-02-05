#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# R INTRODUCTION                                                                 ##
# Data source: Kasitsna Bay Lab - NCCOS - NOAA                                   ##
# R code prepared by NAME                                                        ##
# Last updated 2024-07-18                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Here is a narrative summary of the script.

# Required Files (check that script is loading latest version):
# mussel_data.csv
# tidalbands_shore_information_translatedPolygon.shp
# Jakolof_Bay_poly.kml

# Associated Scripts:
# FILE.R

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
 
# 2024/07/17 Script created

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES & PREPARE WORKSPACE                                            ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# install.packages("tidyverse")
# install.packages("viridis")
# clear any existing data in the environment
rm(list = ls())

library(tidyverse) # tidyverse is a collection of packages for data manipulation
library(viridis) # viridis is a color-blind friendly palette for graphs

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

anem_abund <- read_csv("WorkshopData/anem_abund.csv")

view(anem_abund) # brings up dataset
str(anem_abund)
head(anem_abund)
tail(anem_abund)

# command to read in data to R
mussel_data <- read_csv("WorkshopData/mussel_data.csv", 
                        col_types = cols(polygon_ID = col_character(), 
                                         quadrat = col_character(), 
                                         site = col_character())) 
# look at summary of data structure
str(mussel_data)
glimpse(mussel_data)
head(mussel_data)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# EXPLORATORY SUMMARY FIGURES                                                  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# the simplest ggplot boxplot
ggplot(mussel_data, aes(x = site, 
                        y = length_mm)) +
  geom_boxplot()

# adding themes
ggplot(mussel_data, aes(x = site, 
                        y = length_mm)) +
  geom_boxplot() +
  theme_bw()

# adding color
ggplot(mussel_data, aes(x = site, 
                        y = length_mm,
                        fill = site)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "C") 

# separate by site and quadrat
ggplot(mussel_data, aes(x = quadrat, 
                        y = length_mm,
                        fill = site)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "C") 

# reorder quadrat factor
mussel_data <- mussel_data %>%
  mutate(quadrat = factor(quadrat, levels = c("1", "2", "3", "4", "5",
                                              "6", "7", "8", "9", "10",
                                              "11", "12", "13", "14", "15",
                                              "16", "17", "18", "19", "20")))

# site and quadrat with proper ordering
ggplot(mussel_data, aes(x = quadrat, 
                        y = length_mm,
                        fill = site)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "C")

# total number of mussels per quadrat
ggplot(mussel_data, aes(x = quadrat,
                        fill = site)) +
  stat_count() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "C") 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DATA MANIPULATION & VISUALIZATION                                            ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# How much water can mussels filter per hour calculated from their length?

# equation: Rc = 0.0002L^2.19 from Jones et al. 1992

mussel_data <- mussel_data %>%
  mutate(filtration_l_hr = 0.0002 * length_mm^2.19)

# how many liters per hour do the measured mussels filter combined?
sum(mussel_data$filtration_l_hr)

# liters filtered per hour per site
ggplot(mussel_data, aes(x = site,
                        y = filtration_l_hr,
                        fill = site)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "C")

# liters filtered per hour per quadrat
ggplot(mussel_data, aes(x = quadrat,
                        y = filtration_l_hr,
                        fill = site)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis("Site", 
                     labels = c("One",
                                "Two",
                                "Three",
                                "Four"),
                     discrete = TRUE,
                     option = "C") +
  labs(x = "Quadrat", y = "Filtration Rate L/hr") 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SPATIAL ANALYSIS                                                             ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# terra package works with spatial data
library(terra)

# import the shoreline spatial data
tidelands <- terra::vect("~/git/kbay_filterfeeders/data/tidalbands_shore_information_translated/tidalbands_shore_information_translatedPolygon.shp")

# turn the spatial layer into a data.frame (spreadsheet) and summarize its contents
tidelands_df <- data.frame(values(tidelands))

# plot all the tidelands data by the 'subclass' (substrate type) factor
plot(tidelands, "subclass", col = viridis(nrow(tidelands),
                                    begin = 0,
                                    end = 1,
                                    option = "turbo"))

# import a bounding shape for Jakolof Bay
jako <- terra::vect("C:/Users/Ross.Whippo/Desktop/Jakolof_Bay_poly.kml")

# check if the coordinate reference system (crs) matches the tidelands data
crs(jako)
crs(tidelands)

# extract the tidelands crs
newcrs <- crs(tidelands)

# reproject the Jakolof layer in the new crs
jako_new <- terra:: project(jako, newcrs)

# crop the tidelands layer to just Jakolof
tidelands_jako <- terra::crop(tidelands, jako_new)

# plot tidelands for Jakolof by 'subclass'
plot(tidelands_jako, "subclass", col = viridis(nrow(tidelands_jako),
                                    begin = 0,
                                    end = 1,
                                    option = "turbo"))

# add centroids of polygons
plot(centroids(tidelands_jako, 
               inside = TRUE), add = TRUE)

# extract lat long of centroids
habitat_centroids <- centroids(tidelands_jako, inside = TRUE)
habitat_centroids <- project(habitat_centroids, "+proj=longlat")
coords <- project(habitat_centroids, "+proj=longlat")
plot(coords)
latlong <- extract(habitat_centroids, coords)

points <- crds(centroids(tidelands_jako, inside = TRUE))
jako_df <- data.frame(tidelands_jako)
all_substrate <- data.frame(points, jako_df$subclass)

# calculate total area of the polygons in square meters
sum(terra::expanse(tidelands_jako, unit = "m")) 

#+++++++++++++++++++
# SLOPE CORRECTION #
#+++++++++++++++++++

# FUNCTIONS
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

# tide measurements from https://tidesandcurrents.noaa.gov/datums.html?id=9455517
# highest tide: 7.05 m
# mean low: 0.514 m
# total height of polygons = 6.536

# calculate multiplier to find estimated area of slope plane
# 1/cos(slope) = Multiplier
# Multiplier * Footprint Area = estimated slope area

# averaging high_slope, highmediu0, lowmedium0, low_slope and calculating estimated areas
jako_df <- jako_df %>%
  mutate(temp = str_extract(high_slope, "[0-9]-[0-9]*")) %>%
  mutate(temp = replace_na(temp, "60")) %>%
  separate_wider_delim(temp, "-", names = c("low", "high"), too_few = "align_start") %>%
  mutate(high = replace_na(high, "60")) %>%
  mutate(across(c(high, low), as.numeric)) %>%
  rowwise() %>%
  mutate(high_slope_mean = mean(c(low, high))) %>% # high mean slope
  mutate(temp1 = str_extract(highmediu0, "[0-9]-[0-9]*")) %>%
  mutate(temp1 = replace_na(temp1, "60")) %>%
  separate_wider_delim(temp1, "-", names = c("low1", "high1"), too_few = "align_start") %>%
  mutate(high1 = replace_na(high1, "60")) %>%
  mutate(across(c(high1, low1), as.numeric)) %>%
  rowwise() %>%
  mutate(highmed_slope_mean = mean(c(low1, high1))) %>% # high-med mean slope
  mutate(temp2 = str_extract(lowmedium0, "[0-9]-[0-9]*")) %>%
  mutate(temp2 = replace_na(temp2, "60")) %>%
  separate_wider_delim(temp2, "-", names = c("low2", "high2"), too_few = "align_start") %>%
  mutate(high2 = replace_na(high2, "60")) %>%
  mutate(across(c(high2, low2), as.numeric)) %>%
  rowwise() %>%
  mutate(lowmed_slope_mean = mean(c(low2, high2))) %>% # low-med mean slope
  mutate(temp3 = str_extract(low_slope, "[0-9]-[0-9]*")) %>%
  mutate(temp3 = replace_na(temp3, "60")) %>%
  separate_wider_delim(temp3, "-", names = c("low3", "high3"), too_few = "align_start") %>%
  mutate(high3 = replace_na(high3, "60")) %>%
  mutate(across(c(high3, low3), as.numeric)) %>%
  rowwise() %>%
  mutate(low_slope_mean = mean(c(low3, high3))) %>% # low mean slope
  mutate(average_slope = (high_slope_mean +
                            highmed_slope_mean +
                            lowmed_slope_mean +
                            low_slope_mean)/4) %>% # grand mean slope
  mutate(multiplier = 1/cos(deg2rad(average_slope)))

# add multiplier as value to layer  
tidelands_jako[["multiplier"]] <- jako_df$multiplier

# extract raw polygon areas
jako_polygon_area <- expanse(tidelands_jako)  

# create data frame of corrected polygon areas
jako_areas <- data.frame(jako_df$key, jako_polygon_area, jako_df$multiplier) %>%
  mutate(corrected_area = jako_polygon_area * jako_df.multiplier)

# write data as a csv
write_csv(jako_areas, "WorkshopData/jakolof_polygon_area.csv")

#+++++++++++++++++++++++++++++++++++++++++++
# JOIN SPATIAL DATA WITH MUSSEL FILTRATION #
#+++++++++++++++++++++++++++++++++++++++++++

# Determine mean tidal height for quads within substrates, mean mussel size, 
# mussel count, and filtration rates
mussel_height_summary <- mussel_data %>%
  group_by(substrate, position_percent) %>%
  summarise(across(c(tidal_height_ft,
                     length_mm,
                     filtration_l_hr), mean)) 

mussel_counts <- mussel_data %>%
        group_by(site, substrate, position_percent) %>%
        count() %>%
        ungroup() %>%
        group_by(substrate, position_percent) %>%
        mutate(mean_count = mean(n)) %>%
        ungroup() %>%
        select(substrate, position_percent, mean_count) %>%
        distinct()

mussel_height_summary <- mussel_height_summary %>%
  left_join(mussel_counts, by = c("substrate",
                                  "position_percent")) %>%
  mutate(mean_filtration = filtration_l_hr * mean_count)

# plot mean filtration by position
ggplot(mussel_height_summary, aes(x = tidal_height_ft,
                                  y = mean_filtration,
                                  fill = substrate)) +
  geom_col(position = "dodge",
           width = 0.5) +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE,
                     option = "E")
  


####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####