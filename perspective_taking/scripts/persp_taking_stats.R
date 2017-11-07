library(ez)
library(dplyr)
library(car)

#Run a two-way ANOVA by considering increase factor score as the response variable
#and Daycare and Age group as explanatory variables, with their interaction. 
#(a) Use an interaction plot to summarize the main findings. 
#(b) Is the interaction significant at the 5% level? 
#(c) If not, summarize the effect of each factor (partial eta squared and p-value).

data_stats <- r %>%
  mutate(age=factor(age)) %>%
  mutate(daycare=factor(daycare))
data_stats$id <- factor(1:40)

daycare.ezanova = ezANOVA(r_clean,
                         dv = score,
                         wid = id,
                         within=age.f,
                         between = daycare.f,
                         type = 3)
daycare.ezanova

age.ezanova = ezANOVA(r_clean,
                          dv = score,
                          wid = factor(id),
                          between = age,
                          type = 3)

age.ezanova
