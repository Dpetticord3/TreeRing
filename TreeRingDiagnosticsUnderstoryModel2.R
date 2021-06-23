#Second Canopy model
US2data = ProcessedClimate %>%
  filter(dbh_sample == "8")

US2 = lm(Ci ~Ca+Year+PRCPSD+AnnualTAVG+AnnualTMIN+AnnualTMAX+AnnualAvgPRCP+TAVGSD, data  = US2data)

####################
#CanopyTrees
outlierTest(US2) # Bonferonni p-value for most extreme obs
qqPlot(US2, main="QQ Plot") #qq plot for studentized resid
leveragePlots(US2) # leverage plots 

############################Influential Observations
# added variable plots
avPlots(US2)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(US2data)-length(US2$coefficients)-2))
plot(CanopyFit2, which=4, cook.levels=cutoff)

# Influence Plot
influencePlot(US2, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

##########################################################

# Normality of Residuals
# qq plot for studentized resid
qqPlot(US2, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(US2)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xUS2<-seq(min(sresid),max(sresid),length=40)
yUS2<-dnorm(xUS2)
lines(xUS2, yUS2) 

########################################################

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(US2)
# plot studentized residuals vs. US2ted values
spreadLevelPlot(US2)

#######################################

# Evaluate Collinearity
vif(US2) # variance inflation factors
sqrt(vif(US2)) > 2 # problem?

####################################

# Evaluate Nonlinearity
# component + residual plot
crPlots(US2)
# Ceres plots
ceresPlots(US2)

#####################################

# Test for Autocorrelated Errors
durbinWatsonTest(US2)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(US2)
summary(gvmodel) 

#################################################

## Load effects
library(effects)
## Plot effect of each term
allEffects.lm.C <- allEffects(US2)
plot(allEffects.lm.C)

##################################################

#Provides index plots of Cook's distances, leverages, Studentized residuals, and outlier significance levels for a regression object.
influenceIndexPlot(model = US2, id.n = 5)

######################################################

#InverseResponsePlot
inverseResponsePlot(model = US2, id.n = 5)

##############################################
#MarginalModelPlotting

marginalModelPlots(model = US2, id.n = 5)

