###################################################################################
#                                                                                ##
# R Shorcuts                                                                     ##
# My most often used code                                                        ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 20180928                                                          ##
#                                                                                ##
###################################################################################

# TO DO

# List of actions to take

###################################################################################
# TABLE OF CONTENTS                                                               #
#                                                                                 #
# RECENT CHANGES TO SCRIPT                                                        #
# LOAD PACKAGES                                                                   #   
#                                                                                 #     
###################################################################################

###################################################################################
# RECENT CHANGES TO SCRIPT                                                        #
###################################################################################

# 20170315  Script created

###################################################################################
# LOAD PACKAGES                                                                   #
###################################################################################

# Load packages here

library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(lubridate)

###################################################################################
# ENVIRONMENT                                                                     #
###################################################################################

# detach package
detach(package:packagename)

###################################################################################
# DATA MANIPULATION                                                               #
###################################################################################

# filter dataset by multiple column values
x %>%
  filter(colname1 != "value") %>%
  filter(colname2 == "value") %>%
  filter(colname3 == 123) %>%
  filter(colname4 > 123)

# separate single column into multiple
x %>%
  separate(colname, c("newcolname1", "newcolname2"), sep = "_")

# create new column from multiple columns (unique IDs)
x %>%
  unite_("newcolname", c("colname1", "colname2"), sep = "_", remove = FALSE)

# replace NAs with 0 
x[is.na(x)] <- 0

# constrain factors to currently used factors in list
x$y <- factor(x$y)

# rename factors
recode(x$y, `a` = "b", `c` = "d", 'e' = "f")

# change multiple columns to factor at once
x[] <- lapply(x[], factor)

# identify difference between vector and dataframe column
x[!(x %in% y$z)]

# summarise with multiple outputs
x %>%
  group_by(y, z) %>%
  summarise(mean(colname1), mean(colname2))

# rename column
names(x)[names(x)=="oldname"] <- "newname"
rename(x, newname = oldname)

# change values of particular observations in column
x$y %>%
  recode("oldvalue1" = "newvalue1", "oldvalue2" = "newvalue2")
# create new column with values based on another column
# add unknown phyla manually
raw_biotaxa_f4 <- raw_biotaxa_f4 %>%
  mutate(phylum = case_when(genusSpecies == "Cnemidocarpa sp." ~ "Chordata",
                            genusSpecies == "Polynoidae sp." ~ "Annelida"))

# filter column by multiple values
data %>%
  + filter(column %in% c('value1', 'value2'))



# change numeric values to factor
x$y <- as.factor(x$y)

# raw richness
x %>%
  group_by(y) %>%
  summarise(Unique_Elements = n_distinct(z))

# reorder factor levels
x$y <- factor(x$y, levels = c("d", "c", "b", "a"))

# mutate column values into new column
data <- data %>% 
  mutate(newCol = str_replace_all(oldCol, c("a" = "1",
                                            "b" = "2",
                                            "c" = "3")))

# find incompatible characters in imported file
findOffendingCharacter <- function(x, maxStringLength=256){  
  print(x)
  for (c in 1:maxStringLength){
    offendingChar <- substr(x,c,c)
    #print(offendingChar) #uncomment if you want the indiv characters printed
    #the next character is the offending multibyte Character
  }    
}

lapply(x, findOffendingCharacter)


###################################################################################
# GRAPHING                                                                        #
###################################################################################

# add facet grid by column factor
+ facet_grid(.~colname)

# axis label manipulation
+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  labs(title="Lab 1", x="xlab", y="ylab")

###################################################################################
# ANALYSES                                                                        #
###################################################################################

# find slope of linear relationship between two variables by factor 
x %>% 
  group_by(colname) %>% 
  do({
    mod = lm(var1 ~ var2, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })

# Summary Function

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

###################################################################################
# OTHER OUTPUTS                                                                   #
###################################################################################

# print current UTC date/time
as_datetime(Sys.time(), tz = "UTC")

# save as CSV
write.csv(x, "name.csv")

##### END OF SCRIPT