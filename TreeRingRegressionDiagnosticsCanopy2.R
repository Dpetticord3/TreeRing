#Second Canopy model
Canopy2 = ProcessedClimate %>%
  filter(dbh_sample != "8")

CanopyFit2 = lm(Ci ~Ca+Year+PRCPSD+AnnualTAVG+AnnualTMIN+AnnualTMAX+AnnualAvgPRCP+TAVGSD, data  = Canopy2)

####################
#CanopyTrees
outlierTest(CanopyFit2) # Bonferonni p-value for most extreme obs
qqPlot(CanopyFit2, main="QQ Plot") #qq plot for studentized resid
leveragePlots(CanopyFit2) # leverage plots 

############################Influential Observations
# added variable plots
avPlots(CanopyFit2)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(Canopy2)-length(CanopyFit2$coefficients)-2))
plot(CanopyFit2, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(CanopyFit2, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

##########################################################

# Normality of Residuals
# qq plot for studentized resid
qqPlot(CanopyFit2, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(CanopyFit2)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xCanopyFit2<-seq(min(sresid),max(sresid),length=40)
yCanopyFit2<-dnorm(xCanopyFit2)
lines(xCanopyFit2, yCanopyFit2) 

########################################################

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(CanopyFit2)
# plot studentized residuals vs. CanopyFit2ted values
spreadLevelPlot(CanopyFit2)

#######################################

# Evaluate Collinearity
vif(CanopyFit2) # variance inflation factors
sqrt(vif(CanopyFit2)) > 2 # problem?

####################################

# Evaluate Nonlinearity
# component + residual plot
crPlots(CanopyFit2)
# Ceres plots
ceresPlots(CanopyFit2)

#####################################

# Test for Autocorrelated Errors
durbinWatsonTest(CanopyFit2)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(CanopyFit2)
summary(gvmodel) 

#################################################

## Load effects
library(effects)
## Plot effect of each term
allEffects.lm.C <- allEffects(CanopyFit2)
plot(allEffects.lm.C)

##################################################

#Provides index plots of Cook's distances, leverages, Studentized residuals, and outlier significance levels for a regression object.
influenceIndexPlot(model = CanopyFit2, id.n = 5)

######################################################

#InverseResponsePlot
inverseResponsePlot(model = CanopyFit2, id.n = 5)

##############################################
#MarginalModelPlotting

marginalModelPlots(model = CanopyFit2, id.n = 5)

