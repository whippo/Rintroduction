x_values <- c(1:10)
y_values <- x_values^2
plot(x_values, y_values)
help(plot)
plot(x_values, y_values, col = "red", type = "l", lty = 3, lwd = 3)
install.packages("tidyverse")
library(tidyverse)

?tibble
?plot

x_and_y <- tibble(x_values, y_values)
my.function <- function(X) {
  sqrt(X) + 2
}
my.function(9)
my.function(x_values)
library(styler)
install.packages("styler")
install.packages("styler")
library(lintr)
?style_text()
remotes::install_github("r-lib/styler")
install.packages("remotes")

simulated_norm <- rnorm(n = 10, mean = 8, sd = 3)
set.seed(28)
rnorm(n = 10, mean = 8, sd = 3)

simulated_norm

my_norm_1 <- rnorm(n = 20, mean = 15, sd = 3)
my_norm_2 <- rnorm(n = 15, mean = 9, sd = 2)

t.test(my_norm_1, my_norm_2)
library(tidyverse)

scorpion_data <- read_csv("WorkshopData/scorpion_lengths.csv")
str(scorpion_data)
is_tibble(scorpion_data)

scorpion_data$bedroom
mean(scorpion_data$bedroom)
#INTRO PIPES
scorpion_data %>%
  summarise(mean(bedroom)) 



options(pillar.sigfig = 4)


attach(scorpion_data)
t.test(kitchen, bedroom)
detach(scorpion_data)


daphnia_data <- read_csv("WorkshopData/daphnia.csv")
attach(daphnia_data)
t.test(summer, winter)
t.test(summer, winter, paired = TRUE)
detach(daphnia_data)

height_data <- read_csv("WorkshopData/heights.csv")

t.test(height ~ sex, data = height_data)

bromeliad_data <- read_csv("WorkshopData/bromeliads.csv")
plot(mosquitoes ~ max.vol, data = bromeliad_data)
t.test(mosquitoes ~ species, data = bromeliad_data)

bromeliad_aov <- aov(bromeliad_data$mosquitoes ~ bromeliad_data$location)
summary(bromeliad_aov)

boxplot(mosquitoes ~ location, data = bromeliad_data)

ggplot(bromeliad_data, aes(x = location, y = mosquitoes)) +
         geom_boxplot()

install.packages('viridis')
library(viridis)

ggplot(bromeliad_data, 
       aes(x = location, y = mosquitoes, fill = location)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) 

ggplot(bromeliad_data, 
       aes(x = location, y = mosquitoes, fill = location)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 1, option = "D") +
  labs(x = "Site", y = "Mosquito Abundance")

ggplot(bromeliad_data, 
       aes(x = location, y = mosquitoes, fill = location)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE, 
                     begin = 0.3, end = 1, 
                     option = "D") +
  labs(x = "Site", y = "Mosquito Abundance") 

# DPLYR AND TIDYR

trout_data <- read_csv("WorkshopData/trout.csv")

str(trout_data)
glimpse(trout_data)

trout_rainbow <- trout_data %>%
  filter(species == "rainbow")

trout_length_spp <- trout_data %>%
  select(length, species)

trout_log_length <- trout_data %>%
  mutate(log10(length))

trout_log_mayfly <- trout_data %>%
  filter(stream == "mayfly") %>%
  mutate(log10(length)) 

trout_log_mayfly %>%
  summarise(mean(log10(length)))

trout_data %>%
  filter(stream == "mayfly") %>%
  mutate(log10(length))%>%
  summarise(mean(log10(length)))

write_csv(trout_log_length, "WorkshopData/trout_data_mutated.csv")

