library(lme4)

#Failed model due to no random effects
lmer(frequency ~ attitude, data=politeness)

#Model with random effects subject and item
politeness.model1 = lmer(frequency ~attitude + (1|subject)+(1|scenario),
                        data=politeness)

summary(politeness.model1)

plot(politeness.model1)

#Model with gender as fixed effect
politeness.model2 <- lmer(frequency ~ attitude + 
                            gender + 
                            (1|subject) + 
                            (1|scenario),
                          data=politeness)

summary(politeness.model2)
plot(politeness.model2)

#Null model
politeness.null <- lmer(frequency ~ gender +
                          (1|subject)+
                          (1|scenario),
                        data=politeness, REML = F)
summary(politeness.null)

#Model 2 for comparison
politeness.model3 <- lmer(frequency ~ attitude + 
                            gender + 
                            (1|subject) + 
                            (1|scenario),
                          data=politeness, REML = F)

summary(politeness.model3)

#ANOVA to compare
anova(politeness.null,politeness.model3)

#Compare full models (crossed *) to reduced models (+)
cross.mdl <- lm(frequency~attitude*gender,data=politeness)
non.cross.mdl<- lm(frequency~attitude + gender, data= politeness)
anova(non.cross.mdl,cross.mdl)

##Coefficients####
politeness.coefs <- coef(politeness.model3)
politeness.coefs

#Random Slope
politeness.model4 <- lmer(frequency~attitude+
                            gender+
                            (1+attitude|subject)+
                            (1+attitude|scenario),
                          data = politeness,
                          REML = F)
coef(politeness.model4)

#Construct new null model
politeness.null2 <- lmer(frequency ~ gender +
                           (1+attitude|subject)+
                           (1+attitude|scenario),
                         data = politeness,
                         REML = F)
#Compare the null model to the model with random slopes
anova(politeness.null2,politeness.model4)

##ASSUMPTIONS####
#models assume no collinearity or influential data points, homoskedasticity, and normality
#check these with:
#residual plot
#histogram of residuals
#Q-Q plot

library(influence.ME)

all.res <- numeric(nrow(politeness))

politeness.model5 <- lmer(frequency~attitude+
                            gender+
                            (1+attitude|subject)+
                            (1+attitude|scenario),
                          data = politeness,
                          REML = F,
                          POP[-i,])

for(i in 1:nrow(politeness)){
  politeness.model.full <-lmer(frequency~attitude+
                                 gender+
                                 (1+attitude|subject)+
                                 (1+attitude|scenario),
                               data = politeness,
                               POP[-i,])
  all.res[i]<-fixef(politeness.model.full)[2]
}
