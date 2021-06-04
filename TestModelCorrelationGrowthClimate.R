HKK_CorrPlot <- read_excel("HKKClimateCorrelation.xlsx")

head(HKK_CorrPlot)

model = lm(GrowthCorr~AverageTemperature+MinTemperature+maxTemperature+sdTemperature+discrimination
           +Year+Ci+dbh_sample+Ci/Ca+Ca+d13Catm+d13Ccorr+d13Craw+Ca-Ci+Extant_dbh, data = HKK_CorrPlot)

summary(model)$coefficients

sigma(model)/mean(HKK_CorrPlot$GrowthCorr)


##########
leaps = regsubsets(Growth_Corr~AverageTemperature +MinTemperature+maxTemperature+sdTemperature+discrimination
                   +Year+Ci+Ci/Ca+Ca+d13Catm+d13Ccorr+Ca-Ci, 
                   data = HKK_CorrPlot,nbest =10)
summary(leaps)


