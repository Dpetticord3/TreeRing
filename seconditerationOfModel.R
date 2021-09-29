VDS_Data_Climate_YearIncluded <- read.csv("D:/00_Work/Tree Ring/Github/TreeRing/VDS_Data_Climate_YearIncluded.csv")

VDS = VDS_Data_Climate_YearIncluded %>%
  mutate(CICARAT = Ci/Ca)%>%
  mutate(size_category = as.factor(size))


mymodel2 = lmer(CICARAT~size_category*Species*TotalPRCP + size_category*Species*DriestQPRCP +
                 size_category*Species*HottestMonthTAVG + size_category*Species*TotalAnnualTemperature + (1|Tree_code) +(1|Year),
               data = VDS)

#check for no pattern
plot(predict(mymodel2), residuals(mymodel2))
#check for bell curve
hist(residuals(mymodel2))
#qqplot
qqnorm(residuals(mymodel2)); qqline(residuals(mymodel2))

anova(mymodel2)
#checking third level interactions, are they too complicated, etc.
#arguments for including it on the grounds that they are important/ecologically relevant.
#generally, if its experimental, keep the whole thing. Observational, maybe pare down the model, delicately with reverence to all its parts.

step(mymodel2, reduce.random = FALSE)
#eventually drops the climate,

library(emmeans)

emmeans(mymodel2, ~Species+size_category++TotalAnnualTemperature+HottestMonthTAVG+TotalPRCP, 
        at = list(DriestQPRCP = (quantile(VDS$DriestQPRCP, probs = c(.25,.5,.75))),
                  TotalAnnualPRCP = (quantile(VDS$TotalPRCP, probs = c(.25,.5,.75))),
                  TotalAnnualTemperature = (quantile(VDS$TotalAnnualTemperature, probs = c(.25,.5,.75))),
                  HottestMonthTAVG = (quantile(VDS$HottestMonthTAVG, probs = c(.25,.5,.75)))))

emmip(mymodel2, Species~DriestQPRCP |size_category, at =list(DriestQPRCP=(quantile(VDS$DriestQPRCP,
                                                                                            probs = c(.25,.5,.75)))))

emmip(mymodel, Species~TotalAnnualTemperature |size_category, at =list(TotalAnnualTemperature=(quantile(VDS$TotalAnnualTemperature,
                                                                                                        probs = c(.25,.5,.75)))))

emmip(mymodel, Species~PRCP_Seasonality |size_category, at =list(PRCP_Seasonality=(quantile(VDS$PRCP_Seasonality,
                                                                                            probs = c(.25,.5,.75)))))                                                                                            probs = c(.25,.5,.75)))))

emtrends(mymodel2,~Species+size_category, var = "DriestQPRCP")
emtrends(mymodel2,~Species+size_category, var = "TotalPRCP")
emtrends(mymodel,~Species+size_category, var = "TotalAnnualTemperature")
emtrends(mymodel,~Species+size_category, var = "HottestMonthTAVG")



emmip(mymodel, Species~HottestMonthTAVG |size_category, at =list(HottestMonthTAVG=(quantile(VDS$HottestMonthTAVG,
                                                                                            probs = c(.25,.5,.75)))))
