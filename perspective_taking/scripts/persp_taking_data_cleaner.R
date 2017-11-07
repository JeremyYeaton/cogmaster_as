library(dplyr)
library(ggplot2)

r <- read.table("data/perspective_taking-data.dat", header=FALSE)
colnames(r) <- c("daycare","age","score")
r_clean <- r %>%
  arrange(score)
r_clean

r.plot = ggplot(r_clean,aes(x=c(1:40))) +
  geom_smooth(aes(y= score)) 
  geom_point(aes(y = mean(r_clean$score)))
  
r.plot

str(r_clean)

r_clean <- r %>%
  mutate(age.f = factor(r_clean$age,levels=c(1,2),
                        labels = c("Young","Old"))) %>%
  mutate(daycare.f = factor(r_clean$daycare,levels=c(1,2),
                            labels = c("No Daycare Experience",
                                       "Daycare Experience"))) %>%
  mutate(id = factor(1:40)) %>%
  mutate(treatment=paste(r_clean$age.f,r_clean$daycare.f,sep="-"))
r_clean
str(r_clean)
summary(r_clean)

xtabs(~daycare.f+age.f, data=r_clean)
xtabs(score~daycare.f+age.f,data=r_clean)
#Same as above^^
r_clean %>%
  xtabs(~daycare.f+age.f,data=.)

### BUILD SUBSETS ####
# Uncrossed groups
data_daycare <- r_clean %>%
  filter(daycare == 2)

data_nodaycare <- r_clean %>%
  filter(daycare == 1)

data_young <- r_clean %>%
  filter(age==1)
data_old <- r_clean %>%
  filter(age==2)

# Crossed groups
data_old_daycare <- r_clean %>%
  filter(age==2) %>%
  filter(daycare == 2)

data_young_daycare <- r_clean %>%
  filter(age==1) %>%
  filter(daycare == 2) 

data_old_nodaycare <- r_clean %>%
  filter(age==2) %>%
  filter(daycare == 1)
  
data_young_nodaycare <- r %>%
  filter(age==1) %>%
  filter(daycare == 1)
  
### DESCRIPTIVE STATS FOR SUBGROUPS ####
# Sample Size
daycare_n <- dim(data_daycare)[[1]]
nodaycare_n <- dim(data_nodaycare)[[1]]
young_n <- dim(data_young)[[1]]
old_n <- dim(data_old)[[1]]

# Mean
daycare_mean <- mean(data_daycare$score)
nodaycare_mean <- mean(data_nodaycare$score)
young_mean <- mean(data_young$score)
old_mean <- mean(data_old$score)

# Standard Deviation
daycare_sd <- sd(data_daycare$score)
nodaycare_sd <- sd(data_nodaycare$score)
young_sd <- sd(data_young$score)
old_sd <- sd(data_old$score)

# Range
daycare_range <- range(data_daycare$score)[[2]]-range(data_daycare$score)[[1]]
nodaycare_range <- range(data_nodaycare$score)[[2]]-range(data_nodaycare$score)[[1]]
young_range <- range(data_young$score)[[2]]-range(data_young$score)[[1]]
old_range <- range(data_old$score)[[2]]-range(data_old$score)[[1]]

### DESCRIPTIVE STATS FOR CROSS ####
# Sample Size
old_daycare_n <- dim(data_old_daycare)[[1]]
old_nodaycare_n <- dim(data_old_nodaycare)[[1]]
young_daycare_n <- dim(data_young_daycare)[[1]]
young_nodaycare_n <- dim(data_young_nodaycare)[[1]]

# Mean
old_daycare_mean <- mean(data_old_daycare$score)
old_nodaycare_mean <- mean(data_old_nodaycare$score)
young_daycare_mean <- mean(data_young_daycare$score)
young_nodaycare_mean <- mean(data_young_nodaycare$score)

# Standard Deviation
old_daycare_sd <- sd(data_old_daycare$score)
old_nodaycare_sd <- sd(data_old_nodaycare$score)
young_daycare_sd <- sd(data_young_daycare$score)
young_nodaycare_sd <- sd(data_young_nodaycare$score)

# Range
old_daycare_range <- range(data_old_daycare$score)[[2]]-range(data_old_daycare$score)[[1]]
old_nodaycare_range <- range(data_old_nodaycare$score)[[2]]-range(data_old_nodaycare$score)[[1]]
young_daycare_range <- range(data_young_daycare$score)[[2]]-range(data_young_daycare$score)[[1]]
young_nodaycare_range <- range(data_young_nodaycare$score)[[2]]-range(data_young_nodaycare$score)[[1]]

### BUILD TABLES ####
table_daycare <- matrix(
  c(daycare_n,nodaycare_n,
    daycare_mean,nodaycare_mean,
    daycare_sd,nodaycare_sd,
    daycare_range,nodaycare_range),
  nrow=2, ncol=4
)
colnames(table_daycare) <- c("Sample Size","Mean","Standard Deviation","Range")
rownames(table_daycare) <- c("Daycare Experience", "No Daycare Experience")
  
table_age <-matrix(
  c(young_n,old_n,
    young_mean,old_mean,
    young_sd,old_sd,
    young_range,old_range),
  nrow=2, ncol=4
)
colnames(table_age) <- c("Sample Size","Mean","Standard Deviation","Range")
rownames(table_age) <- c("Younger", "Older")

table_cross <-matrix(
  c(young_daycare_n,old_daycare_n,young_nodaycare_n,old_nodaycare_n,
    young_daycare_mean,old_daycare_mean,young_nodaycare_mean,old_nodaycare_mean,
    young_daycare_sd,old_daycare_sd,young_nodaycare_sd,old_nodaycare_sd,
    young_daycare_range,old_daycare_range,young_nodaycare_range,old_nodaycare_range),
  nrow=4, ncol=4
)
colnames(table_cross) <- c("Sample Size","Mean","Standard Deviation","Range")
rownames(table_cross) <- c("Younger with Daycare Experience", "Older with Daycare Experience",
                           "Younger with No Daycare Experience","Older with No Daycare Experience")

### TABLES THE RIGHT WAY ####
age_summary <- r_clean %>%
  group_by(age.f) %>%
  summarise(mean=mean(score),
            sd=sd(score),
            min=min(score),
            max=max(score),
            range=max-min) %>%
  ungroup()
  
daycare_summary <- r_clean %>%
  group_by(daycare.f) %>%
  summarise(mean=mean(score),
            sd=sd(score),
            min=min(score),
            max=max(score),
            range=max-min) %>%
  ungroup()

crossed_summary <- r_clean %>%
  group_by(daycare.f,age.f) %>%
  summarise(mean=mean(score),
            sd=sd(score),
            min=min(score),
            max=max(score),
            range=max-min) %>%
  ungroup()
crossed_summary
