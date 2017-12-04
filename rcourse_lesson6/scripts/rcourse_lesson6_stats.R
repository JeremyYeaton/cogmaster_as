## READ IN DATA ####
source("scripts/rcourse_lesson6_cleaning.R")

## LOAD PACKAGES ####
library(lme4)

## ORGANIZE DATA ####
# Accuracy data
data_accuracy_stats = data_accuracy_clean

# Check within or between variables
xtabs(~subject_id+congruency+half, data_accuracy_stats)

xtabs(~item+congruency+half, data_accuracy_stats)

xtabs(~item+half, data_accuracy_stats)

# RT data
data_rt_stats = data_rt_clean

## BUILD MODEL FOR ACCURACY ANALYSIS ####
accuracy.glmer = glmer(accuracy ~ congruency * half +
                         (1+congruency*half|subject_id) +
                         (1+half|item), family = "binomial",
                       data = data_accuracy_stats)
#Random Effects Structure 	R Code
#maximal 	(1+x1*x2|s)
#uncorrelated intercept and slope 	(1|s) + (0+x1*x2|s)
#no interaction 	(1+x1+x2|s)
#uncorrelated intercept and slope, no interaction 	(1|s) + (0+x1+x2|s)
#no x2 	(1+x1|s)
#uncorrelated intercept and slope, no x2 	(1|s) + (0+x1|s)
#no x1 	(1+x2|s)
#uncorrelated intercept and slope, no x1 	(1|s) + (0+x2|s)
#intercept only 	(1|s)

accuracy.glmer = glmer(accuracy ~ congruency * half +
                         (1|subject_id) +
                         (0+half|subject_id) +
                         (1|item), 
                       family = "binomial",
                       data = data_accuracy_stats)

# Summarise model and save
accuracy.glmer_sum = summary(accuracy.glmer)
accuracy.glmer_sum

# Get coefficients and save
accuracy.glmer_coef = coef(accuracy.glmer)
accuracy.glmer_coef

## BUILD MODEL FOR REACTION TIME ANALYSIS ####
rt_log10.lmer = lmer(rt_log10 ~ congruency * half +
                       (1+congruency*half|subject_id) +
                       (1+half|item),
                     data = data_rt_stats)

# Summarise model and save
rt_log10.lmer_sum = summary(rt_log10.lmer)
rt_log10.lmer_sum

# Get coefficients and save
rt_log10.lmer_coef = coef(rt_log10.lmer)
rt_log10.lmer_coef
