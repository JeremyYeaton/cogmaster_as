## READ IN DATA ####
source("scripts/cleaning.R")

## LOAD PACKAGES ####
library(ggplot2)

## ORGANIZE DATA ####
data_clean = data %>%
  filter(name == "Jeremy") %>%
  mutate(name = factor(name)) %>%
  filter(year > 1900) %>%
  filter(year <= 2000) %>%
  mutate(prop_loge = log(prop)) %>%
  mutate(prop_log10 = log10(prop))

data_figs = data_clean

data_figs = data_clean %>%
  mutate(sex = factor(sex, levels=c("F", "M"), labels=c("female", "male")))

## MAKE FIGURES ####
# Histogram of dependent variable (proportion of 'Page's)
name.plot = ggplot(data_figs, aes(x = prop)) +
  geom_histogram()

# pdf("figures/name.pdf")
name.plot
# dev.off()



# Histogram of dependent variable (number of 'Page's) - e based log transform
name_loge.plot = ggplot(data_figs, aes(x = prop_loge)) +
  geom_histogram()

# pdf("figures/name_loge.pdf")
name_loge.plot
# dev.off()

# Histogram of dependent variable (number of 'Page's) - 10 based log transform
name_log10.plot = ggplot(data_figs, aes(x = prop_log10)) +
  geom_histogram()

# pdf("figures/name_log10.pdf")
name_log10.plot
# dev.off()

# Proportion of 'Page's by year (continuous predictor)
year.plot = ggplot(data_figs, aes(x = year, y = prop_log10)) +
  geom_point() +
  geom_smooth(method="lm")

# pdf("figures/year.pdf")
year.plot
# dev.off()

# Proportion of 'Page's by sex (categorical predictor)
sex.plot = ggplot(data_figs, aes(x = sex, y = prop_log10)) +
  geom_boxplot()

# pdf("figures/sex.pdf")
sex.plot
# dev.off()

