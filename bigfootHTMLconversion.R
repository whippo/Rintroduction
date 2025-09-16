library(rvest)
library(data.table)
library(tidyverse)
library(janitor)

# Assuming your HTML data is in a file named "input.html"
html_file <- "C:/Users/Ross.Whippo/Downloads/bigfoot.html"

# Read the HTML file and extract the first table
html_data <- read_html(html_file)
table_data <- html_data %>%
  html_table() %>%
  .[] # Extract the first table

# Convert the table data to a data frame
df <- as.data.frame(table_data)

df1 <- df |>
  slice(-2) |>
  select(-1)

df2 <- df1 |>
  row_to_names(row_number = 1)

# Assuming you want to name your output CSV file "output.csv"
fwrite(df2, "WorkshopData/sasquatch.csv")
