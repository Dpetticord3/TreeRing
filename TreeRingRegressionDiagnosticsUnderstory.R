#Regression Diagnostics
#UnderstoryTrees
library(car)
outlierTest(UnderstoryFit) # Bonferonni p-value for most extreme obs
qqPlot(UnderstoryFit, main="QQ Plot") #qq plot for studentized resid
leveragePlots(UnderstoryFit) # leverage plots 

############################Influential Observations
# added variable plots
avPlots(UnderstoryFit)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(Understory)-length(UnderstoryFit$coefficients)-2))
plot(UnderstoryFit, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(UnderstoryFit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

##########################################################

# Normality of Residuals
# qq plot for studentized resid
qqPlot(UnderstoryFit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(UnderstoryFit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xUnderstoryFit<-seq(min(sresid),max(sresid),length=40)
yUnderstoryFit<-dnorm(xUnderstoryFit)
lines(xUnderstoryFit, yUnderstoryFit) 

########################################################

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(UnderstoryFit)
# plot studentized residuals vs. UnderstoryFitted values
spreadLevelPlot(UnderstoryFit)

#######################################

# Evaluate Collinearity
vif(UnderstoryFit) # variance inflation factors
sqrt(vif(UnderstoryFit)) > 2 # problem?

####################################

# Evaluate Nonlinearity
# component + residual plot
crPlots(UnderstoryFit)
# Ceres plots
ceresPlots(UnderstoryFit)

#####################################

# Test for Autocorrelated Errors
durbinWatsonTest(UnderstoryFit)

##################################################

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(UnderstoryFit)
summary(gvmodel) 

#################################################

## Load effects
library(effects)
## Plot effect of each term
allEffects.lm.US <- allEffects(UnderstoryFit)
plot(allEffects.lm.US)

##################################################

#Provides index plots of Cook's distances, leverages, Studentized residuals, and outlier significance levels for a regression object.
influenceIndexPlot(model = UnderstoryFit, id.n = 5)

######################################################

#InverseResponsePlot
inverseResponsePlot(model = UnderstoryFit, id.n = 5)

##############################################
#MarginalModelPlotting

marginalModelPlots(model = UnderstoryFit, id.n = 5)
