## LOAD PACKAGES ####
library(dplyr)

## READ IN DATA ####
# Read in full season data
data = read.table("data/rcourse_lesson3_data.txt", header=T, sep="\t")

# Read in player (Buster Posey) specific data
data_posey = read.table("data/rcourse_lesson3_data_posey.txt", header=T, sep="\t")

## CLEAN DATA ####
# Add columns to full season data to make data set specific to the Giants
data_clean = data %>%
  mutate(home_visitor = ifelse(home_team == "SFN", "home", "visitor")) %>%
  mutate(allstar_break = ifelse(date < 20100713, "before", "after")) %>%
  mutate(win = ifelse(home_team == "SFN" & home_score > visitor_score, 1,
                      ifelse(visitor_team == "SFN" & home_score < visitor_score, 1, 0)))

# Combine full season data with player (Buster Posey) specific data and clean
data_posey_clean = data_posey %>%
  inner_join(data_clean) %>%
  mutate(walked = ifelse(walks > 0, "yes", "no"))

dim(data_posey)
dim(data_posey_clean)
