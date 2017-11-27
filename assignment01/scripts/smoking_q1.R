###1. ####
#(a) Give a numerical summary of FEV1 (mean, standard deviation and range)
#for each smoking category (recoded as a categorical variable with
#appropriate levels), and for all subjects (grand mean and overall                                                                      
#standard deviation). Results should be printed in one or two Tables. 
#(b)Use box-and-whiskers charts or density plots to show the distribution of
#individual values.

###DESCRIPTIVE STATS####
descript_tbl<-f_clean %>%
  group_by(cat.f)%>%
  summarize(mean=mean(fev1),sd=sd(fev1),range=(max(fev1)-min(fev1)))%>%
  ungroup()

overall_tbl <- f_clean %>%
  summarize("grand mean"=mean(fev1),"overall sd"=sd(fev1),"total range"=(max(fev1)-min(fev1)))


###FIGURES####
#Import library
library(ggplot2)

box_whisker_bycat.plot <- ggplot(f_clean,
                                   aes(x=factor(cat.f,
                                                levels=c("non-smoker","early","recent","current")),
                                       y=fev1)) +
  geom_boxplot()+
  labs(x="Smoking Category",y="FEV1")
box_whisker_bycat.plot

box_whisker_summary.plot <-ggplot(f_clean,aes(x="Whole Sample",y=fev1)) +
  geom_boxplot()
box_whisker_summary.plot

density_bycat.plot <- ggplot(f_clean,aes(x=fev1)) +
  geom_density()
density_bycat.plot
