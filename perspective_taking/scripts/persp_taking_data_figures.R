library(ggplot2)

#Use box-and-whiskers charts or density plots to show the distribution 
#of individual increase in the composite score for each treatment.

age.plot <- ggplot(r_clean, aes(x=age.f,y=score)) +
  geom_boxplot() + 
  ggtitle("Age") +
  xlab("Age")

daycare.plot <- ggplot(r_clean,aes(x=daycare.f,y=score)) +
  geom_boxplot() + 
  ggtitle("Daycare") +
  xlab("Daycare")

age.plot
daycare.plot

plot_data <- r_clean

treatments.plot <- ggplot(plot_data,aes(x=age.f,y=score,fill=daycare.f)) +
  geom_boxplot() +
  ggtitle("Score distribution by treatment")
treatments.plot

density.plot <- ggplot(plot_data,aes(x=score,color= treatment,fill=treatment)) +
  geom_density(alpha=0.1,kernel="gaussian") +
  ggtitle("Density of score by treatment")
density.plot

hist.plot <- ggplot(plot_data,aes(x=score)) +
  geom_histogram() +
  geom_density() +
  ggtitle("Density of score by treatment")
hist.plot
