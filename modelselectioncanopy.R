#ModelSelection Canopy
library(MASS)
CanopyStep = stepAIC(CanopyFit2,direction = "both")
CanopyStepOG = stepAIC(CanopyFit, direction = "both")
CanopybesStep = stepAIC(mod_canopy, direction = "both")

######
#LowestAICModel = Ci ~ PRCPSD + AnnualTMAX + AnnualAvgPRCP

CanopyBestModel = lm(Ci~PRCPSD + AnnualTMAX + AnnualAvgPRCP, data = Canopy2)

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
# plot statistic by subset size
library(car)
subsets(Canopyleaps, statistic="rsq") 

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(CanopyBestModel,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(CanopyFit2, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 

############################################
#best linear model is still only a small chunk of variance 