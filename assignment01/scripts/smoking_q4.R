###4. ####
#Is there any evidence for a linear or quadratic trend for mean FEV1 when
#considering smoking status as ordered factor levels: 1 < 2 < 3 < 4 (use
#the R command factor with the ordered = TRUE option)?

library(ggplot2)

lin_model.self<-lm(fev1~factor(cat.f,ordered = T),f_clean)
summary(lin_model.self)

lin_model.plot <- ggplot(f_clean,aes(x=cat,y=fev1))+
  geom_abline()

lin_model.plot

plot(lin_model.self)




