#model selection
###########################Afzelia
AX_intercept_only = lm(Ci~Ca, data = Afzelia_x)
AX_all = lm(Ci~., data = Afzelia_x)
AX_both <- step(AX_intercept_only, direction='both', scope=formula(AX_all), trace=0)
AX_both$anova
AX_both$coefficients
ggplotRegression(lm(Ci~Ca+DriestQPRCP, data = Afzelia_x))
###########################Afzelia_US
AX_US_intercept_only = lm(Ci~Ca, data = Afzelia_US)
AX_US_all = lm(Ci~., data = Afzelia_US)
AX_US_both <- step(AX_US_intercept_only, direction='both', scope=formula(AX_US_all), trace=0)
AX_US_both$anova
AX_US_both$coefficients
ggplotRegression(lm(Ci~Ca, data = Afzelia_US))
###########################Afzelia_Canopy
AX_C_intercept_only = lm(Ci~Ca, data = Afzelia_Canopy)
AX_C_all = lm(Ci~., data = Afzelia_Canopy)
AX_C_both <- step(AX_C_intercept_only, direction='both', scope=formula(AX_C_all), trace=0)
AX_C_both$anova
AX_C_both$coefficients
ggplotRegression(lm(Ci~Ca, data = Afzelia_Canopy))
###########################Melia
MA_intercept_only = lm(Ci~Ca, data = Melia_a)
MA_all = lm(Ci~., data = Melia_a)
MA_both <- step(MA_intercept_only, direction='both', scope=formula(MA_all), trace=0)
MA_both$anova
MA_both$coefficients
ggplotRegression(lm(Ci~Ca+HottestMonthTAVG+PRCP_Seasonality, data = Melia_a))
###########################Melia_Canopy
MA_C_intercept_only = lm(Ci~Ca, data = Melia_Canopy)
MA_C_all = lm(Ci~., data = Melia_Canopy)
MA_C_both <- step(MA_C_intercept_only, direction='both', scope=formula(MA_C_all), trace=0)
MA_C_both$anova
MA_C_both$coefficients
ggplotRegression(lm(Ci~Ca+TotalPRCP, data = Melia_Canopy))
###########################Melia_US
MA_US_intercept_only = lm(Ci~Ca, data = Melia_US)
MA_US_all = lm(Ci~., data = Melia_US)
MA_US_both <- step(MA_US_intercept_only, direction='both', scope=formula(MA_US_all), trace=0)
MA_US_both$anova
MA_US_both$coefficients
ggplotRegression(lm(Ci~Ca+PRCP_Seasonality+HottestMonthTAVG, data = Melia_US))
###########################Chukrasia
C_intercept_only = lm(Ci~Ca, data = Chukrasia)
C_all = lm(Ci~., data = Chukrasia)
C_both <- step(C_intercept_only, direction='both', scope=formula(C_all), trace=0)
C_both$anova
C_both$coefficients
ggplotRegression(lm(Ci~Ca, data = Chukrasia))
###########################ChukrasiaUS
C_US_intercept_only = lm(Ci~Ca, data = Chukrasia_US)
C_US_all = lm(Ci~., data = Chukrasia_US)
C_US_both <- step(C_US_intercept_only, direction='both', scope=formula(C_US_all), trace=0)
C_US_both$anova
C_US_both$coefficients
ggplotRegression(lm(Ci~Ca+TotalAnnualTemperature, data = Chukrasia_US))
############################Chukrasia_Canopy
C_C_intercept_only = lm(Ci~Ca, data = Chukrasia_Canopy)
C_C_all = lm(Ci~., data = Chukrasia_Canopy)
C_C_both <- step(C_C_intercept_only, direction='both', scope=formula(C_C_all), trace=0)
C_C_both$anova
C_C_both$coefficients
ggplotRegression(lm(Ci~Ca, data = Chukrasia_Canopy))
############################Afzelia_Chukrasia
AXC_intercept_only = lm(Ci~Ca, data = Afzelia_Chukrasia)
AXC_all = lm(Ci~., data = Afzelia_Chukrasia)
AXC_both <- step(AXC_intercept_only, direction='both', scope=formula(AXC_all), trace=0)
AXC_both$anova
AXC_both$coefficients
ggplotRegression(lm(Ci~Ca, data = Afzelia_Chukrasia))
###########################Afzelia_Chukrasia_US
AXC_US_intercept_only = lm(Ci~Ca, data = Afzelia_Chukrasia_US)
AXC_US_all = lm(Ci~., data = Afzelia_Chukrasia_US)
AXC_US_both <- step(AXC_US_intercept_only, direction='both', scope=formula(AXC_US_all), trace=0)
AXC_US_both$anova
AXC_US_both$coefficients
ggplotRegression(lm(Ci~Ca+DriestMonthPRCP+WettestMonthPRCP+CoolestMonthTAVG, data = Afzelia_Chukrasia_US))
###########################Afzelia_Chukrasia_Canopy
AXC_C_intercept_only = lm(Ci~Ca, data = Afzelia_Chukrasia_Canopy)
AXC_C_all = lm(Ci~., data = Afzelia_Chukrasia_Canopy)
AXC_C_both <- step(AXC_C_intercept_only, direction='both', scope=formula(AXC_C_all), trace=0)
AXC_C_both$anova
AXC_C_both$coefficients
ggplotRegression(lm(Ci~Ca+CoolestMonthTMIN, data = Afzelia_Chukrasia_Canopy))
############################Afzelia_Melia
AM_intercept_only = lm(Ci~Ca, data = Afzelia_Melia)
AM_all = lm(Ci~., data = Afzelia_Melia)
AM_both <- step(AM_intercept_only, direction='both', scope=formula(AM_all), trace=0)
AM_both$anova
AM_both$coefficients
ggplotRegression(lm(Ci~Ca, data = Afzelia_Melia))
###########################Afzelia_Melia_US
AM_US_intercept_only = lm(Ci~Ca, data = Afzelia_Melia_US)
AM_US_all = lm(Ci~., data = Afzelia_Melia_US)
AM_US_both <- step(AM_US_intercept_only, direction='both', scope=formula(AM_US_all), trace=0)
AM_US_both$anova
AM_US_both$coefficients
ggplotRegression(lm(Ci~Ca+DriestQPRCP, data = Afzelia_Melia_US))
###########################Afzelia_Melia_Canopy
AM_C_intercept_only = lm(Ci~Ca, data = Afzelia_Melia_Canopy)
AM_C_all = lm(Ci~., data = Afzelia_Melia_Canopy)
AM_C_both <- step(AM_C_intercept_only, direction='both', scope=formula(AM_C_all), trace=0)
AM_C_both$anova
AM_C_both$coefficients
ggplotRegression(lm(Ci~Ca, data = Afzelia_Melia_Canopy))
############################Melia_Chukrasia
MC_intercept_only = lm(Ci~Ca, data = Melia_Chukrasia)
MC_all = lm(Ci~., data = Melia_Chukrasia)
MC_both <- step(MC_intercept_only, direction='both', scope=formula(MC_all), trace=0)
MC_both$anova
MC_both$coefficients
ggplotRegression(lm(Ci~Ca+HottestMonthTAVG, data = Melia_Chukrasia))
###########################Melia_Chukrasia_US
MC_US_intercept_only = lm(Ci~Ca, data = Melia_Chukrasia_US)
MC_US_all = lm(Ci~., data = Melia_Chukrasia_US)
MC_US_both <- step(MC_US_intercept_only, direction='both', scope=formula(MC_US_all), trace=0)
MC_US_both$anova
MC_US_both$coefficients
ggplotRegression(lm(Ci~Ca+HottestMonthTAVG, DriestQPRCP, data = Melia_Chukrasia_US))
###########################Melia_Chukrasia_Canopy
MC_C_intercept_only = lm(Ci~Ca, data = Melia_Chukrasia_Canopy)
MC_C_all = lm(Ci~., data = Melia_Chukrasia_Canopy)
MC_C_both <- step(MC_C_intercept_only, direction='both', scope=formula(MC_C_all), trace=0)
MC_C_both$anova
MC_C_both$coefficients
ggplotRegression(lm(Ci~Ca+DriestMonthPRCP, data = Melia_Chukrasia_Canopy))
############################Afzelia_Melia_Chukrasia
AMC_intercept_only = lm(Ci~Ca, data = AfzeliaMeliaChukrasia)
AMC_all = lm(Ci~., data = AfzeliaMeliaChukrasia)
AMC_both <- step(AMC_intercept_only, direction='both', scope=formula(AMC_all), trace=0)
AMC_both$anova
AMC_both$coefficients
ggplotRegression(lm(Ci~Ca+HottestMonthTMAX, data = AfzeliaMeliaChukrasia))
###########################Afzelia_Melia_Chukrasia_US
AMC_US_intercept_only = lm(Ci~Ca, data = AfzeliaMeliaChukrasia_US)
AMC_US_all = lm(Ci~., data = AfzeliaMeliaChukrasia_US)
AMC_US_both <- step(AMC_US_intercept_only, direction='both', scope=formula(AMC_US_all), trace=0)
AMC_US_both$anova
AMC_US_both$coefficients
ggplotRegression(lm(Ci~Ca+HEAT_Seasonality+DriestMonthPRCP+WettestMonthPRCP, data = AfzeliaMeliaChukrasia_US))
###########################Afzelia_Melia_Chukrasia_Canopy
AMC_C_intercept_only = lm(Ci~Ca, data = AfzeliaMeliaChukrasia_Canopy)
AMC_C_all = lm(Ci~., data = AfzeliaMeliaChukrasia_Canopy)
AMC_C_both <- step(AMC_C_intercept_only, direction='both', scope=formula(AMC_C_all), trace=0)
AMC_C_both$anova
AMC_C_both$coefficients
ggplotRegression(lm(Ci~Ca, data = AfzeliaMeliaChukrasia_Canopy))
