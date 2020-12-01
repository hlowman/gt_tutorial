# GT Tutorial - Tidy Tuesday
# December 1, 2020
# Heili Lowman

# install.packages()

# Load packages
library(tidyverse)
library(lubridate)
library(gt)
library(paletteer)

# Get more info
?pizzaplace

# Load our data
pizza <- gt::pizzaplace
View(pizza)

# Today, we are interested in making a table of pizza sales by month and by type of pizza.

# (1) Make a new column for month.

pizza <- pizza %>% # Take the original dataset
  mutate(date_format = as_date(date)) %>% # Create newly formatted date column
  mutate(month_num = month(date_format)) # Create new month column

# (2) Create a summary stats table.

pizza_money <- pizza %>% # Take the original dataset
  group_by(month_num, type) %>% # Group by month and pizza type
  mutate(type = factor(type,
    levels = c("classic", "veggie", "chicken", "supreme"))) %>% # Releveling pizza types
  summarize(sold = n(), profit = sum(price)) %>% # Summarize profits by defined groups
  ungroup() # Always make sure to ungroup after you group!!
