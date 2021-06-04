#ModelSelection Understory
library(MASS)
UnderstoryStep = stepAIC(US2,direction = "both")
#

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(US2,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(US2, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 

#lowest AIC linear model is Ci ~ Ca + PRCPSD + AnnualTAVG + AnnualAvgPRCP 