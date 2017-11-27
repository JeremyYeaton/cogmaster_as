###4. ####
#Is there any evidence for a linear or quadratic trend for mean FEV1 when
#considering smoking status as ordered factor levels: 1 < 2 < 3 < 4 (use
#the R command factor with the ordered = TRUE option)?

###IMPORT LIBRARY####
library(ggplot2)

###CREATE MODELS####
#Linear model with ordered factor levels
lin_model.self<-lm(fev1~factor(cat.f,ordered = T),f_clean)
summary(lin_model.self)

###PLOT THE MODEL####
#Linear model with categorical variable
lin_model.plot <- ggplot(f_clean,aes(x=factor(cat,ordered=T),y=fev1),na.rm=T)+
  geom_smooth(method=lm,se=T,aes(group=1))+
  geom_point()+
  labs(x="Ordered Smoker Category",y="FEV1")

lin_model.plot

#Quadratic model
qmodel.plot <- ggplot(lm_data,aes(x=cat,y=fev1))+
  geom_point()+
  geom_smooth(method=lm,formula=y~I(x^2))+
  labs(x="Numeric Smoker Category",y="FEV1")

qmodel.plot

plot(lin_model.self)

means.tbl<- f_clean %>%
  group_by(cat.f)%>%
  summarize(mean=mean(fev1)) %>%
  ungroup()
means.tbl

summary(mean(f_clean$fev1))
