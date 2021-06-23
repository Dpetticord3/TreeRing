###All Species Model - Canopy

Canopy_All = VDS_Data_Climate %>%
  filter(size =="27") %>%
  select(-size, -Tree_code, -Year)
#####################

#Random intercept Fixed Slope Model 
ggplot(Canopy_All, aes(y = Ci, x = Ca)) +
  geom_point() +
  xlab("Atmospheric CO2")+
  ylab("Intracellular CO2") +
  ggtitle("Canopy Trees Ci/Ca")+
  theme_Publication()+
  stat_smooth(method = "lm", formula = 'y ~ x', se=F,fullrange = T) +
  facet_wrap(~Species)
#models - canopy - all species

Mix.Canopy.AllSpecies = lmer(Ci~Ca + (1|Species), data = Canopy_All)
summary(Mix.Canopy.AllSpecies)
coef(Mix.Canopy.AllSpecies)$Species

####RandomEffectsModel

RandomEffectModel.Canopy <- lmer(Ci ~Ca+ (Ca | Species), data = Canopy_All)
summary(RandomEffectModel.Canopy)
z= coef(RandomEffectModel.Canopy)$Species

#Quantile Regression - Canopy, Afzelia

Canopy_Ax = Canopy_All %>%
  filter(Species == "Afzelia_xylocarpa")

AxCiCa= lm(Ci~Ca, data = Canopy_Ax)
summary(AxCiCa)
#Quantile
AxCiCaQR = rq(Ci~Ca, data = Canopy_Ax)
AxCiCaQR

plot(Ci ~ Ca, data = Canopy_Ax, pch = 16, main = "Canopy_Ax_Ci/Ca")
abline(lm(Ci ~ Ca,  data = Canopy_Ax), col = "red", lty = 2)
abline(rq(Ci ~ Ca,  data = Canopy_Ax), col = "blue", lty = 2)
legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)


###Stepwise approach
intercept_only = lm(Ci~Ca, data = Canopy_All)
all = lm(Ci~., data = Canopy_All)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)
#view results of forward stepwise regression
forward$anova
#FinalModel
forward$coefficients

#perform backward stepwise regression
backward <- step(all, direction='backward', scope=formula(all), trace=0)

#view results of backward stepwise regression
backward$anova
backward$coefficients

#view results of both direction stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)
both$anova
both$coefficients
