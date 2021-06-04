#Regression Diagnostics
#AfzeliaTrees
Afzelia_Climate$size = as.factor(Afzelia_Climate$size)
AfzeliaFit = lm(Ci ~Ca+Year+PRCP+size+TAVG+TMIN+TMAX, data  = Afzelia_Climate)

outlierTest(AfzeliaFit) # Bonferonni p-value for most extreme obs
qqPlot(AfzeliaFit, main="QQ Plot") #qq plot for studentized resid
leveragePlots(AfzeliaFit) # leverage plots 

############################Influential Observations
# added variable plots
avPlots(AfzeliaFit)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(Afzelia_Climate)-length(AfzeliaFit$coefficients)-2))
plot(AfzeliaFit, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(AfzeliaFit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

##########################################################

# Normality of Residuals
# qq plot for studentized resid
qqPlot(AfzeliaFit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(AfzeliaFit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xAfzeliaFit<-seq(min(sresid),max(sresid),length=40)
yAfzeliaFit<-dnorm(xAfzeliaFit)
lines(xAfzeliaFit, yAfzeliaFit) 

########################################################

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(AfzeliaFit)
# plot studentized residuals vs. AfzeliaFitted values
spreadLevelPlot(AfzeliaFit)

#######################################

# Evaluate Collinearity
vif(AfzeliaFit) # variance inflation factors
sqrt(vif(AfzeliaFit)) > 2 # problem?

####################################

# Evaluate Nonlinearity
# component + residual plot
crPlots(AfzeliaFit)
# Ceres plots
ceresPlots(AfzeliaFit)

#####################################

# Test for Autocorrelated Errors
durbinWatsonTest(AfzeliaFit)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(AfzeliaFit)
summary(gvmodel) 

#################################################

## Load effects
library(effects)
## Plot effect of each term
allEffects.lm.AF <- allEffects(AfzeliaFit)
plot(allEffects.lm.AF)

##################################################

#Provides index plots of Cook's distances, leverages, Studentized residuals, and outlier significance levels for a regression object.
influenceIndexPlot(model = AfzeliaFit, id.n = 5)

######################################################

#InverseResponsePlot
inverseResponsePlot(model = AfzeliaFit, id.n = 5)

##############################################
#MarginalModelPlotting

marginalModelPlots(model = AfzeliaFit, id.n = 5)

