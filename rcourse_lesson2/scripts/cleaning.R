## LOAD PACKAGES ####
library(dplyr)

## READ IN DATA ####
data = read.table("data/rcourse_lesson2_data.txt", header=T, sep="\t")
data

## CLEAN DATA ####
data_clean = data %>%
  filter(name == "Jeremy")

head(data_clean)
xtabs(~name, data_clean)

data_clean = data %>%
  filter(name == "Jeremy") %>%
  mutate(name = factor(name))

data_clean = data %>%
  filter(name == "Jeremy") %>%
  mutate(name = factor(name)) %>%
  filter(year > 1900) %>%
  filter(year <= 2000)

min(data_clean$year)
max(data_clean$year)
