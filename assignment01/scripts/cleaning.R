###IMPORT LIBRARIES####
library(dplyr)
library(magrittr)

###READ IN DATA####
f <-read.table("data/smoking-data.dat",header=FALSE)
names(f)

#RENAME COLUMNS
colnames(f)<-cbind("cat","fev1")

#RENAME VARIABLES
f_clean <- f %>%
  mutate(cat.f= factor(if_else(cat=="1","non-smoker",
                        ifelse(cat=="2","early",
                               ifelse(cat=="3","recent",
                                      ifelse(cat=="4","current","")))),
                       ordered=T),
                       ident=order(cat))

f_clean


###I found a better way to do this after I had finished####
g_data <- g %>%
  rename(cat=V1,fev1=V2)%>%
  mutate(cat.f = factor(cat,
                        levels=c("1","2","3","4"),
                        labels=c("non","early","former","current"),
                        ordered=T),
         ident=order(cat))
g_data
