#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# New R Workshop Script                                                       ##
# Script created 2019-07-25                                                   ##
# Data source: Ross Whippo/Edd Hamilton                                       ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2023-09-21                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Script used for demonstration purposes at the R Introduction Workshop adapted 
# from REU Workshop 2019.
# A few take-homes that aren't in the script below:

# KEEP YOUR DATA TIDY! use https://style.tidyverse.org/ as a guide on how to make
# and maintain clean code.

# Use version control! Read up on it and avail yourselves of
# version control tools. I use the version control software 'Git' which is utilized
# by the online platform 'Github' (think of it as Dropbox for coders.) R Studio
# is compatible with Git and can be accessed through the 'Project' drop down
# at the far top right of the R Studio screen. 

# If you want to see the Github repository I created for this workshop, go to:
# https://github.com/whippo/Rintroduction


# Required Files (check that script is loading latest version):
# scorpion_lengths.csv
# daphnia.csv
# heights.csv
# trout.csv

# Associated Scripts:
# None

# TO DO:
# NA

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# RECENT CHANGES TO SCRIPT                                                     +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE AND ANALYZE DATA                                                  +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2019-07-25 Script created for R Workshop
# 2019-07-26 Further annotation and formatting for distribution to students.
# 2022-02-15 adapted to new workshop
# 2023-09-21 Script tidied for use w/ Semester by the Sea Intern workshop

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load the packages that you will need to do the analyses.
library(dplyr) # manipulates data
library(tidyr) # tidys data
library(readr) # better import of datasets
library(ggplot2) # data visualization 
library(viridis) # color palette for color blind
library(ggpubr) # layout scheme for multiple plots

# set your working directory 
# make sure this script is saved in the folder that contains the data you want to
# use. Go to the menu above, select Session > Set Working Directory > To Source 
# File Location. Copy and past the command that appears in the console to the space
# below. It should begin with: setwd(etc..........)
# setwd()

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# create data in the R environment

# create a vector of numbers and save as an object called 'x_values'
x_values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
# you can save the same sequence of numbers using a colon as a short hand
x_values <- c(1:9)
# square values of all x_values and save as an object called 'y_values'
y_values <- x_values^2

# make plot of x and y values
plot1 <- plot(x_values, y_values)
# add more customization to the plot. Use the command: ?plot to find
# more commands that can be included for customization. 
plot(x_values, y_values, col = "red", type = "l", lty = 3, lwd = 3)

# make table of x and y (looks like a spreadsheet). This kind of layout, columns
# and rows, is called a 'data frame'. A tibble is a certain kind of data frame. 
x_and_y <- tibble(x_values, y_values)
# view the tibble you just created. You can also click on it in the environment box.
x_and_y

# simulate data for later t-test

# set.seed creates a standard starting place for the creation of random numbers.
# You can change the number to whatever you like, it will only set the seed for the
# next command you run. After that, you'd have to set the seed again. Only use 
# set.seed if you are trying to standardize a draw of random numbers. Otherwise ignore.
set.seed(227)
# Create a draw of random numbers from the normal distribution and save as vector 
sim_norm_1 <- rnorm(n = 10, mean = 8, sd = 3)
# set the seed again, only if you are standardizing random number draws. 
set.seed(911)
# Create a second, different draw of random numbers and save as a vector
sim_norm_2 <- rnorm(n = 20, mean = 14, sd = 2)



# import data from external sources
# if you are copying and pasting your code for reading datasets into your session
# after loading the data through the graphical interface to the right,
# be sure that your file path begins with: "~/your folder name here/etc....". You 
# need the tilde in front of a slash for R to read the location properly ~/

# import scorpion data set
scorpion_lengths <- read_csv("~/Git/Rintroduction/WorkshopData/scorpion_lengths.csv")
# compare to the code generated by importing through the folder pane to the right:
# scorpion_lengths <- scorpion_lengths <- read_csv("WorkshopData/scorpion_lengths.csv")
# gulfwatch analog of scorpions
anem_abundance <- read_csv("~/git/Rintroduction/WorkshopData/anem_abund.csv")
# number of Urticina crassicornis per 100 m^2 for each survey taken at 
# Elephant Island and Bluff Point

# import daphnia data
daphnia <- read_csv("WorkshopData/daphnia.csv")
anemyears <- read.csv("WorkshopData/anemyears.csv")
# number of all anemones seen across all sites in 2022 and 2023

# import heights data
heights <- read_csv("WorkshopData/heights.csv")
anem_stratum <- read_csv("WorkshopData/anem_stratum.csv")
# density of anemones at all years and sites in high and low strata

# import bromeliad data
bromeliads <- read_csv("WorkshopData/bromeliads.csv")
anem_sites <- read_csv("WorkshopData/anem_sites.csv")
# number of anemones across four sites

# import trout data
trout <- read_csv("WorkshopData/trout.csv")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE AND ANALYZE DATA                                               ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# t-test for differences between simulated data. Do the vectors have a statistically
# significant difference in their mean?
t.test(sim_norm_1, sim_norm_2)

# find the mean of scorpion lengths found in the bedroom
scorpion_lengths %>%
  summarise(mean(bedroom))

# find the mean of anemone density at each site
anem_abundance %>%
  summarise(mean(`Elephant Island`))
anem_abundance %>%
  summarise(mean(`Bluff Point`, na.rm = TRUE))

# use a t.test to determine if bedroom and kitchen had different sizes of scorpions
attach(scorpion_lengths)
t.test(kitchen, bedroom)
# always detach!
detach(scorpion_lengths)
# you can also run it like this and not have to use the attach() command:
t.test(scorpion_lengths$kitchen, scorpion_lengths$bedroom)
# use a t.test to determine if bedroom and kitchen had different sizes of scorpions
# you can also run it like this and not have to use the attach() command:

t.test(anem_abundance$`Bluff Point`, anem_abundance$`Elephant Island`)


# is there a difference in daphnia abundance between summer and winter in 10 ponds?
attach(daphnia)
t.test(summer, winter)
# but wait! You're resampling the same ponds through time! You must run a 
# PAIRED t.test!
t.test(summer, winter, paired = TRUE)
# always detach!
detach(daphnia)

t.test(anemyears$year2022, anemyears$year2023)
t.test(anemyears$year2022, anemyears$year2023, paired = TRUE)

# use a t.test to find out if there are differences in height among males
# and females. 

# sex = descriptive variable
# height = response variable
# formula for this sort of t.test (no attaching required!):
# t.test(response variable ~ descriptive variable, data = your data)
t.test(height ~ sex, data = heights)

t.test(mean ~ Stratum, data = anem_stratum)


# what does the data called bromeliads look like?
str(bromeliads)

str(anem_sites)

# anova - used to compare the means of more than two sets of samples!
# are there different numbers of mosquitoes among sites (3 sites)?
# run an anova and save it as an object
bromeliad_aov <- aov(mosquitoes ~ location, data = bromeliads)
# see a summary of the ouput of your anova
summary(bromeliad_aov)

anem_site_aov <- aov(`Abundance (# ind/100 m2)` ~ Site, data = anem_sites)
summary(anem_site_aov)

# Anova tells you if the means are different, but they don't tell you which
# sets of data are different from which. Only that there is a difference
# somewhere. To find pairwise differences among sets of data in an anova,
# use a post-hoc Tukey test.
TukeyHSD(bromeliad_aov)

TukeyHSD(anem_site_aov)
# this produces a list of pairwise comparisons among your sites with 
# associated p-value. p > 0.05 is no difference, p < 0.05 are different.

# make a boxplot to visualize the mosquito abundance data by location
boxplot(mosquitoes ~ location, data = bromeliads)

boxplot(`Abundance (# ind/100 m2)` ~ Site, data = anem_sites)

# a more flexible and attractive way to visualize your data is with ggplot.

# here is a bare-bones ggplot:
ggplot(bromeliads, aes(x = location, y = mosquitoes)) +
  geom_boxplot() 

ggplot(anem_sites, aes(x = Site, y = `Abundance (# ind/100 m2)`)) +
  geom_boxplot() 

# now add a bunch of options to it!
ggplot(bromeliads, aes(x = location, y = mosquitoes, fill = location)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 1, option = "D") +
  labs(x = "Site", y = "Mosquito Abundance")

# now add a bunch of options to it!
ggplot(anem_sites, aes(x = Site, y = `Abundance (# ind/100 m2)`, fill = Site)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 1, option = "D") +
  labs(x = "Site", y = "Anemone Density per 100 m^2")

# Manipulating data with common dplyr and tidyr commands

# check structure of the trout dataset
str(trout)

# make a new dataset called trout_rainbow that keeps only the rainbow
# trout data
trout_rainbow <- trout %>%
  filter(species == "rainbow")

# make a new trout dataset that only includes the length and species columns
trout_len_spp <- trout %>%
  select(length, species)

# add a new column to the dataset that include the log length of each trout
trout_log_len <- trout %>%
  mutate(loglen = log10(length), sqlen = length^2)

# export your rainbow trout data to a csv file in your working directory
write_csv(trout_rainbow, "trout_rainbow.csv")

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# I often include and area called Scratchpad at the end of my code while
# it is still in progress. It's an out of the way place to try quick 
# analyses or manipulations in my session. I would then cut and paste
# useful code from the scratch pad into the appropriate place in
# the above script. I would delete this section before sharing or posting.

# SCRATCH PAD ####

# Scratchpad scripts go here.




























