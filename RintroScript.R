###################################################################################
#                                                                                ##
# R Workshop Script                                                              ##
# Data are current as of 2019-07-25                                              ##
# Data source: OIMB                                                              ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2019-07-26                                                        ##
#                                                                                ##
###################################################################################

# SUMMARY:

# Script used for demonstration purposes at the R Introduction Workshop for REUs.
# A few take-homes that aren't in the script below:

# KEEP YOUR DATA TIDY! use https://style.tidyverse.org/ as a guide on how to make
# and maintain clean code.

# Use version control! We didn't have a chance to get into version control (which
# would be a workshop all on its own), but read up on it and avail yourselves of
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

###################################################################################
# TABLE OF CONTENTS                                                               #
#                                                                                 #
# RECENT CHANGES TO SCRIPT                                                        #
# LOAD PACKAGES                                                                   #
# READ IN AND PREPARE DATA                                                        #
# MANIPULATE AND ANALYZE DATA                                                     #   
#                                                                                 #
###################################################################################

###################################################################################
# RECENT CHANGES TO SCRIPT                                                        #
###################################################################################

# 2019-07-25 Script created for R Workshop
# 2019-07-26 Further annotation and formatting for distribution to students.

###################################################################################
# LOAD PACKAGES                                                                   #
###################################################################################

# load the packages that you will need to do the analyses.
library(dplyr) # manipulates data
library(tidyr) # tidys data
library(readr) # better import of datasets
library(ggplot2) # data visualization 
library(viridis) # color palette for color blind
library(ggpubr) # layout scheme for multiple plots

# set your working directory (I forgot to mention this in the workshop!)
# make sure this script is saved in the folder that contains the data you want to
# use. Go to the menu above, select Session > Set Working Directory > To Source 
# File Location. Copy and past the command that appears in the console to the space
# below. It should begin with: setwd(etc..........)
# setwd()

###################################################################################
# READ IN AND PREPARE DATA                                                        #
###################################################################################

# create data in the R environment

# create a vector of numbers and save as an object called 'x_values'
x_values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
# you can save the same sequence of numbers using a colon as a short hand
x_values <- c(1:9)

# square values of all x_values and save as an object called 'y_values'
y_values <- x_values^2

# make plot of x and y values
plot(x_values, y_values)
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
# scorpion_lengths <- read_csv("Git/Rintroduction/WorkshopData/scorpion_lengths.csv")

# import daphnia data
daphnia <- read_csv("~/Git/Rintroduction/WorkshopData/daphnia.csv")

# import heights data
heights <- read_csv("~/Git/Rintroduction/WorkshopData/heights.csv")

# import bromeliad data
bromeliads <- read_csv("~/Git/Rintroduction/WorkshopData/bromeliads.csv")

# import trout data
trout <- read_csv("~/Git/Rintroduction/WorkshopData/trout.csv")

###################################################################################
# MANIPULATE AND ANALYZE DATA                                                     #
###################################################################################

# t-test for differences between simulated data. Do the vectors have a statistically
# significant difference in their mean?
t.test(sim_norm_1, sim_norm_2)

# find the mean of scorpion lengths found in the bedroom
scorpion_lengths %>%
  summarise(mean(bedroom))

# use a t.test to determine if bedroom and kitchen had different sizes of scorpions
attach(scorpion_lengths)
t.test(kitchen, bedroom)
# always detach!
detach(scorpion_lengths)
# you can also run it like this and not have to use the attach() command:
t.test(scorpion_lengths$kitchen, scorpion_lengths$bedroom)

# is there a difference in daphnia abundance between summer and winter in 10 ponds?
attach(daphnia)
t.test(summer, winter)
# but wait! You're resampling the same ponds through time! You must run a 
# PAIRED t.test!
t.test(summer, winter, paired = TRUE)
# always detach!
detach(daphnia)


# use a t.test to find out if there are differences in height among males
# and females. 

# sex = descriptive variable
# height = response variable
# formula for this sort of t.test (no attaching required!):
# t.test(response variable ~ descriptive variable, data = your data)
t.test(height ~ sex, data = heights)


# what does the data called bromeliads look like?
str(bromeliads)

# anova - used to compare the means of more than two sets of samples!
# are there different numbers of mosquitoes among sites (3 sites)?
# run an anova and save it as an object
bromeliad_aov <- aov(mosquitoes ~ location, data = bromeliads)
# see a summary of the ouput of your anova
summary(bromeliad_aov)

# Anova tells you if the means are different, but they don't tell you which
# sets of data are different from which. Only that there is a difference
# somewhere. To find pairwise differences among sets of data in an anova,
# use a post-hoc Tukey test.
TukeyHSD(bromeliad_aov)
# this produces a list of pairwise comparisons among your sites with 
# associated p-value. p > 0.05 is no difference, p < 0.05 are different.

# make a boxplot to visualize the mosquito abundance data by location
boxplot(mosquitoes ~ location, data = bromeliads)

# a more flexible and attractive way to visualize your data is with ggplot.

# here is a bare-bones ggplot:
ggplot(bromeliads, aes(x = location, y = mosquitoes)) +
  geom_boxplot() 

# now add a bunch of options to it!
ggplot(bromeliads, aes(x = location, y = mosquitoes, fill = location)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 1, option = "D") +
  labs(x = "Site", y = "Mosquito Abundance")


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
  mutate(log10(length))

# export your rainbow trout data to a csv file in your working directory
write_csv(trout_rainbow, "trout_rainbow.csv")


#####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# I often include and area called Scratchpad at the end of my code while
# it is still in progress. It's an out of the way place to try quick 
# analyses or manipulations in my session. I would then cut and paste
# useful code from the scratch pad into the appropriate place in
# the above script. I would delete this section before sharing or posting.

############### SCRATCHPAD

# scratchpad scripts go here.