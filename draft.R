VDS_Data_Climate_YearIncluded <- read.csv("D:/00_Work/Tree Ring/Github/TreeRing/VDS_Data_Climate_YearIncluded.csv")

VDS = VDS_Data_Climate_YearIncluded %>%
  mutate(CICARAT = Ci/Ca)%>%
  mutate(size_category = as.factor(size))


mymodel = lmer(CICARAT~size_category*Species*TotalPRCP + size_category*Species*PRCP_Seasonality +
                 size_category*Species*HottestMonthTAVG + size_category*Species*TotalAnnualTemperature + (1|Tree_code) +(1|Year),
               data = VDS)
view(VDS)
#check for no pattern
plot(predict(mymodel), residuals(mymodel))
#check for bell curve
hist(residuals(mymodel))
#qqplot
qqnorm(residuals(mymodel)); qqline(residuals(mymodel))

anova(mymodel)
#checking third level interactions, are they too complicated, etc.
#arguments for including it on the grounds that they are important/ecologically relevant.
#generally, if its experimental, keep the whole thing. Observational, maybe pare down the model, delicately with reverence to all its parts.

step(mymodel, reduce.random = FALSE)
#eventually drops the climate,

library(emmeans)

emmeans(mymodel, ~Species+size_category+PRCP_Seasonality+TotalPRCP+TotalAnnualTemperature+HottestMonthTAVG,
        at = list(PRCP_Seasonality = (quantile(VDS$PRCP_Seasonality, probs = c(.25,.5,.75))),
                  TotalAnnualTemperature = (quantile(VDS$TotalAnnualTemperature, probs = c(.25,.5,.75))),
                  TotalPRCP = (quantile(VDS$TotalPRCP, probs = c(.25,.5,.75))),
                  HottestMonthTAVG = (quantile(VDS$HottestMonthTAVG, probs = c(.25,.5,.75)))))

emmip(mymodel, Species~HottestMonthTAVG |size_category, at =list(HottestMonthTAVG=(quantile(VDS$HottestMonthTAVG,
                                                                                            probs = c(.25,.5,.75)))))

emmip(mymodel, Species~TotalAnnualTemperature |size_category, at =list(TotalAnnualTemperature=(quantile(VDS$TotalAnnualTemperature,
                                                                                            probs = c(.25,.5,.75)))))

emmip(mymodel, Species~PRCP_Seasonality |size_category, at =list(PRCP_Seasonality=(quantile(VDS$PRCP_Seasonality,
                                                                                            probs = c(.25,.5,.75)))))                                                                                            probs = c(.25,.5,.75)))))

emtrends(mymodel,~Species+size_category, var = "HottestMonthTAVG")
emtrends(mymodel,~Species+size_category, var = "PRCP_Seasonality")
emtrends(mymodel,~Species+size_category, var = "TotalAnnualTemperature")
emtrends(mymodel,~Species+size_category, var = "TotalPRCP")



emmip(mymodel, Species~TotalPRCP |size_category, at =list(TotalPRCP=(quantile(VDS$TotalPRCP,probs = c(.25,.5,.75)))))
emmip(mymodel, Species~HottestMonthTAVG |size_category, at =list(HottestMonthTAVG=(quantile(VDS$HottestMonthTAVG,probs = c(.25,.5,.75)))))
emmip(mymodel, Species~TotalPRCP |size_category, at =list(TotalPRCP=(quantile(VDS$TotalPRCP,probs = c(.25,.5,.75)))))
