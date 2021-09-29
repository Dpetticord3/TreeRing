both = merge(HKK_ClimateCi_Record, HKK_ClimateCi_Record_LAG, by = "Tree_code", suffixes = c(".RT", ".LAG"))

head(both)

both = both %>%
  select(-one_of("Year.LAG", "Species.LAG", "size.LAG", "Ca.LAG", "Ci.LAG", "Ci_Ca_Ratio.LAG"))

#look at corrplot? 


bothcorr = cor(both[,5:19])

corrplot(bothcorr, type="upper", order="hclust",
         col=brewer.pal(n=10, name="RdYlGn"))
####

Both_UNDERSTORY  = both %>%
  filter(size.RT =="UNDERSTORY")
Both_CANOPY = both %>%
  filter(size.RT =="CANOPY")

####

MModel_Both = lmer(Ci_Ca_Ratio.RT~size.RT*Species.RT*MMean_PRCP.RT + 
                    size.RT*Species.RT*MMean_Temperature.RT   +
                    size.RT*Species.RT*MMean_Temperature_DrySeason.RT    +
                    size.RT*Species.RT*MMean_Temperature_WetSeason.RT   +
                    size.RT*Species.RT*MMean_PRCP_DrySeason.RT  + 
                    size.RT*Species.RT*MMean_PRCP_WetSeason.RT +
                    size.RT*Species.RT*MMean_PRCP.LAG + 
                    size.RT*Species.RT*MMean_Temperature.LAG  +
                    size.RT*Species.RT*MMean_Temperature_DrySeason.LAG   +
                    size.RT*Species.RT*MMean_Temperature_WetSeason.LAG  +
                    size.RT*Species.RT*MMean_PRCP_DrySeason.LAG + 
                    size.RT*Species.RT*MMean_PRCP_WetSeason.LAG +
                    (1|Tree_code) +(1|Year.RT),
                  data = both)
##


mmodel_both_remove_colinearity = lmer(Ci_Ca_Ratio.RT~size.RT*Species.RT*MMean_PRCP.RT + 
                                        size.RT*Species.RT*MMean_Temperature.RT   +
                                        size.RT*Species.RT*MMean_PRCP_DrySeason.RT  + 
                                        size.RT*Species.RT*MMean_PRCP.LAG + 
                                        size.RT*Species.RT*MMean_Temperature.LAG  +
                                        size.RT*Species.RT*MMean_PRCP_DrySeason.LAG + 
                                        (1|Tree_code) +(1|Year.RT),
                                      data = both)
##

##


plot(predict(MModel_Both), residuals(MModel_Both))
#check for bell curve
hist(residuals(MModel_Both))
#qqplot
qqnorm(residuals(MModel_Both)); qqline(residuals(MModel_Both))


anova(MModel_Both)
#checking third level interactions

step(MModel_Both, reduce.random = FALSE)


stepwiseboth = lmer(Ci_Ca_Ratio.RT ~ size.RT + Species.RT + MMean_PRCP.RT + MMean_Temperature.RT + MMean_PRCP_DrySeason.RT + 
  MMean_PRCP_WetSeason.RT + MMean_Temperature.LAG + MMean_Temperature_DrySeason.LAG + MMean_Temperature_WetSeason.LAG + 
  (1 | Tree_code) + (1 | Year.RT) + size.RT:Species.RT + size.RT:MMean_PRCP.RT + size.RT:MMean_Temperature.RT + Species.RT:MMean_Temperature.RT + size.RT:MMean_PRCP_DrySeason.RT + Species.RT:MMean_PRCP_DrySeason.RT + size.RT:MMean_PRCP_WetSeason.RT + Species.RT:MMean_PRCP_WetSeason.RT +      size.RT:MMean_Temperature.LAG + Species.RT:MMean_Temperature.LAG + size.RT:MMean_Temperature_DrySeason.LAG + size.RT:MMean_Temperature_WetSeason.LAG + Species.RT:MMean_Temperature_WetSeason.LAG + size.RT:Species.RT:MMean_Temperature.RT + size.RT:Species.RT:MMean_PRCP_DrySeason.RT + size.RT:Species.RT:MMean_PRCP_WetSeason.RT + size.RT:Species.RT:MMean_Temperature.LAG + size.RT:Species.RT:MMean_Temperature_WetSeason.LAG, 
  data = both)


Both_Table = as.data.frame(emmeans(stepwiseboth, ~Species.RT+size.RT + MMean_PRCP.RT + MMean_Temperature.RT + MMean_PRCP_DrySeason.RT + MMean_PRCP_WetSeason.RT + MMean_Temperature.LAG + MMean_Temperature_DrySeason.LAG + MMean_Temperature_WetSeason.LAG, 
        at = list(MMean_PRCP.RT = 
                  (quantile(both$MMean_PRCP.RT, probs = c(.25,.5,.75))),
                  MMean_PRCP_DrySeason = (quantile(both$MMean_Temperature.RT, probs = c(.25,.5,.75))),
                  MMean_Temperature.RT = (quantile(both$MMean_PRCP_DrySeason.RT, probs = c(.25,.5,.75))),
                  MMean_PRCP_WetSeason.RT =(quantile(both$MMean_PRCP_WetSeason.RT, probs = c(.25,.5,.75))),
                  MMean_Temperature.LAG = (quantile(both$MMean_Temperature.LAG, probs = c(.25,.5,.75))),
                  MMean_Temperature_DrySeason.LAG = (quantile(both$MMean_Temperature_DrySeason.LAG, probs = c(.25,.5,.75))),
                  MMean_Temperature_WetSeason.LAG = (quantile(both$MMean_Temperature_WetSeason.LAG,probs = c(.25,.5,.75))))))


#here are all the environmental coefficents:
emtrends(stepwiseboth,~Species.RT+size.RT, var = "MMean_PRCP.RT")
emtrends(stepwiseboth,~Species.RT+size.RT, var = "MMean_Temperature.RT")
emtrends(stepwiseboth,~Species.RT+size.RT, var = "MMean_PRCP_DrySeason.RT")
emtrends(stepwiseboth,~Species.RT+size.RT, var = "MMean_PRCP_WetSeason.RT")
emtrends(stepwiseboth,~Species.RT+size.RT, var = "MMean_Temperature.LAG")
emtrends(stepwiseboth,~Species.RT+size.RT, var = "MMean_Temperature_DrySeason.LAG")
emtrends(stepwiseboth,~Species.RT+size.RT, var = "MMean_Temperature_WetSeason.LAG")


#Estimated marginal means plots to give us a visualization of contrasts
#between groups
emmip(stepwiseboth, Species.RT~MMean_PRCP.RT |size.RT, 
      at =list(MMean_PRCP.RT=
                 (quantile(both$MMean_PRCP.RT,
                           probs = c(.25,.5,.75))))) +
  ggtitle("Pairwise Contrasts, Same-Year MMean PRCP")+
  theme_Publication()+
  scale_colour_Publication()






















