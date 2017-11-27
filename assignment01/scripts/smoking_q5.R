###5. ####
#(a) Compare the preceding results with the conclusion that would be
#reached by using a regression approach where one considers smoking status
#as a numerical variable, as well as its square, i.e., using the R command
#lm with a formula like FEV1 ~ smoking + I(smoking)^2.
#(b) What could explain the difference, if any?

###IMPORT LIBRARY####
library(ggplot2)

###UNGROUP THE DATA FRAME####
lm_data<- f_clean %>%
  ungroup()

summary(lm_data)


###CREATE MODEL####
#Build unordered lm model with smoker^2 object
num_model.self<-lm(formula=fev1~cat+I(cat^2),lm_data)
summary(num_model.self)

sqr_model.self<-lm(formula=fev1~I(cat^2),lm_data)
summary(sqr_model.self)

###PLOT THE MODEL####
#Plot with smoker^2 object
linsqr_model.plot <- ggplot(lm_data,aes(x=cat^2,y=fev1))+
  geom_smooth(method="lm",se=T,formula=y~x+I(x^2),aes(group=1))+
  geom_point()+
  labs(x="Numeric Smoker Category",y="FEV1")

linsqr_model.plot

sqr_model.plot<-ggplot(lm_data,aes(x=cat,y=fev1))+
  geom_smooth(method="lm",se=T,formula=y~I(x^2),aes(group=1))+
  #geom_smooth(method="lm",formula=y~I(x^2),aes(color="red"))+
  geom_point()+
  labs(x="Numeric Smoker Category",y="FEV1")

sqr_model.plot


sqr_model1.plot<-ggplot(lm_data,aes(x=cat,y=fev1))+
  geom_smooth(method="lm",se=T,formula=y~x+I(x^2),aes(group=1))+
  #geom_smooth(method="lm",formula=y~I(x^2),aes(color="red"))+
  geom_point()+
  labs(x="Numeric Smoker Category",y="FEV1")

sqr_model1.plot

###COMPARE TO CLASSMATES' CODE AND WEEP QUIETLY####
ggplot(data = lm_data, aes(x = cat^2, y = fev1)) +
  geom_point(shape = 5) +
  geom_smooth(formula=y~x+I(x^2),method = "lm", se=TRUE, color="red", aes(group=1)) +
  theme_minimal() 
