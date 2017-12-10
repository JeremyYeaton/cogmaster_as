### LIBRARIES ####
library(lme4)
library(dplyr)

## IMPORT DATA ####
politeness <- read.csv("data/politeness_data.csv")

# Check for missing values
which(is.na(politeness$frequency))

