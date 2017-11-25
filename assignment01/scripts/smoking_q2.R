###2. ####
#Carry out a one-way ANOVA to test the null hypothesis that FEV1
#does not depend on smoking category.
#(a) Formulate your conclusion in plain English, and
#(b) report the percentage of explained variance.

library(ez)
#ANOVA
by_cat.anova<- ezANOVA(f_clean,fev1,ident,between=cat.f)

by_cat.anova
