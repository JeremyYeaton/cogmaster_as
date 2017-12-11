library(ggplot2)

politeness.boxplot <- ggplot(politeness) +
  geom_boxplot(aes(fill=attitude,x= gender,y=frequency))

politeness.boxplot

politeness.BW.boxplot <- boxplot(frequency ~ attitude*gender, 
                                 col = c("white","lightgray"),
                                 politeness)
