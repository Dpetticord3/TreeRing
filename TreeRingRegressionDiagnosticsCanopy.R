#Regression Diagnostics

#firstmodel
Canopy = Afzelia_Climate %>%
  filter(size != "8")
CanopyFit = lm(Ci ~Ca+Year+PRCP+TAVG+TMIN+TMAX, data  = Canopy)
####################
#CanopyTrees
outlierTest(CanopyFit) # Bonferonni p-value for most extreme obs
qqPlot(CanopyFit, main="QQ Plot") #qq plot for studentized resid
leveragePlots(CanopyFit) # leverage plots 

############################Influential Observations
# added variable plots
avPlots(CanopyFit)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(Canopy)-length(CanopyFit$coefficients)-2))
plot(CanopyFit, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(CanopyFit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

##########################################################

# Normality of Residuals
# qq plot for studentized resid
qqPlot(CanopyFit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(CanopyFit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xCanopyFit<-seq(min(sresid),max(sresid),length=40)
yCanopyFit<-dnorm(xCanopyFit)
lines(xCanopyFit, yCanopyFit) 

########################################################

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(CanopyFit)
# plot studentized residuals vs. CanopyFitted values
spreadLevelPlot(CanopyFit)

#######################################

# Evaluate Collinearity
vif(CanopyFit) # variance inflation factors
sqrt(vif(CanopyFit)) > 2 # problem?

####################################

# Evaluate Nonlinearity
# component + residual plot
crPlots(CanopyFit)
# Ceres plots
ceresPlots(CanopyFit)

#####################################

# Test for Autocorrelated Errors
durbinWatsonTest(CanopyFit)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(CanopyFit)
summary(gvmodel) 

#################################################

## Load effects
library(effects)
## Plot effect of each term
allEffects.lm.C <- allEffects(CanopyFit)
plot(allEffects.lm.C)

##################################################

#Provides index plots of Cook's distances, leverages, Studentized residuals, and outlier significance levels for a regression object.
influenceIndexPlot(model = CanopyFit, id.n = 5)

######################################################

#InverseResponsePlot
inverseResponsePlot(model = CanopyFit, id.n = 5)

##############################################
#MarginalModelPlotting

marginalModelPlots(model = CanopyFit, id.n = 5)

