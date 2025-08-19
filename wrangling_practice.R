# clear our environment
rm(list = ls())

# attach our packages
library(tidyverse)
library(palmerpenguins)
library(lubridate) # helps us work with dates

# data wrangling refresher
# 1. only include penguins as Briscoe and Dream Islands
# 2. remove the year and sex variables
# 3. add a new column called body_mass_kg with penguin mass converted from grams to kg
# 4. rename the island variable to location
penguins %>% 
  filter(island %in% c("Briscoe", "Dream")) %>% 
  select(-c(year, sex)) %>% 
  mutate(body_mass_kg = 
           body_mass_g / 1000) %>% 
  rename(location = island)

# 1. limit to only adelie penguins
# 2. Remove any observations where flipper_length_mm is NA
# 3. group the data by sex
# 4. find the mean, sd, and sample size of flipper lengths for male and female

penguins %>% 
  filter(species == "Adelie") %>% 
  filter(!is.na(flipper_length_mm),
         !is.na(sex)) %>% # remove rows that are not NA
  group_by(sex) %>% 
  summarize(mean = mean(flipper_length_mm),
            standard_dev = sd(flipper_length_mm),
            sample_size = n())
  
  

