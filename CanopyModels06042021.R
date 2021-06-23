Ax_Climate = VDS_Data_Climate %>%
  filter(Species =="Afzelia_xylocarpa")

#Canopy Trees

Canopy_Ax = Ax_Climate %>%
  filter(size =="27") %>%
  select(-size, -Species, -Tree_code, -Year)

#models - canopy

LM_CanopyAx_Ca = lm(Ci~Ca, data = Canopy_Ax)
summary(LM_CanopyAx_Ca_Ci)
#####
LM_CanopyAx_Ca_TotalP = lm(Ci~Ca+TotalPRCP, data = Canopy_Ax)
summary(LM_CanopyAx_Ca_Ci_TotalP)
######
LM_CanopyAx_Ca_DryQ = lm(Ci~Ca+DriestQPRCP, data = Canopy_Ax)
summary(LM_CanopyAx_Ca_DryQ)
######
LM_CanopyAx_Ca_TotalP_DryQ = lm(Ci~Ca+TotalPRCP+DriestQPRCP, data = Canopy_Ax)
summary(LM_CanopyAx_Ca_TotalP_DryQ)


###Stepwise approach
intercept_only = lm(Ci~Ca, data = Canopy_Ax)
all = lm(Ci~., data = Canopy_Ax)

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
