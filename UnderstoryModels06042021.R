Ax_Climate = merge(x, VDS_Data_Ax, by = c("Tree_code", "size", "Year"))

Ax_Climate = Ax_Climate %>%
  select(-Year, -Tree_code, -`Ci/Ca`)
#Understory Trees

Understory_Ax = Ax_Climate %>%
  filter(size == "8") %>%
  select(-size)


#################

LM_UnderstoryAx_Ca = lm(Ci~Ca, data = Understory_Ax)
summary(LM_CanopyAx_Ca_Ci)
#####
LM_UnderstoryAx_Ca_TotalP = lm(Ci~Ca+TotalPRCP, data = Understory_Ax)
summary(LM_UnderstoryAx_Ca_TotalP)
######
LM_UnderstoryAx_Ca_DryQ = lm(Ci~Ca+DriestQPRCP, data = Understory_Ax)
summary(LM_UnderstoryAx_Ca_DryQ)
######
LM_UnderstoryAx_Ca_TotalP_DryQ = lm(Ci~Ca+TotalPRCP+DriestQPRCP, data = Understory_Ax)
summary(LM_UnderstoryAx_Ca_TotalP_DryQ)

###############

###Stepwise approach
intercept_only = lm(Ci~Ca, data = Understory_Ax)
all = lm(Ci~., data = Understory_Ax)

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

