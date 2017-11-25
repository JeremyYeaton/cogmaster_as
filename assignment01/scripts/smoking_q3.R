###3. ####
#(a) Use post-hoc Tukey HSD tests (R command: TukeyHSD) to compare all
#pairs of means among the four groups of smokers. Summarize point
#estimates and 95% confidence intervals in a Table or graphical display,
#and indicate which pairs of means are found to be significantly different.
#(b) Compare those results with results from all pairwise comparisons for
#mean FEV1 using the Bonferroni method (R command: pairwise.t.test).

#TukeyHSD
f.thsd<-aov(fev1~cat.f,data=f_clean)

summary(f.thsd)

thsd<- TukeyHSD(f.thsd)

plot(thsd)

#compare to results from all pairwise comparisons for mean fev1 using the
#bonferroni method
bonferroni.ttest<-pairwise.t.test(f_clean$fev1,f_clean$cat.f,"bonferroni")
