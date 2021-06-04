plot(PRCP ~ Ci, data = Canopy, pch = 16, main = "Canopy Trees Sensitivity to MMeanPRCP")
abline(lm(PRCP ~ Ci, data = Canopy), col = "red", lty = 2)
abline(rq(PRCP ~ Ci, data = Canopy), col = "blue", lty = 2)


plot(PRCP ~ Ci, data = Canopy, pch = 16, main = "Canopy Trees Sensitivity to MMeanPRCP")
abline(lm(PRCP ~ Ci, data = Canopy), col = "red", lty = 2)
abline(rq(PRCP ~ Ci, data = Canopy), col = "blue", lty = 2)

rqfit = rq(PRCP~Ci, data = Canopy)

mod_gam1 = gam(Ci ~s(AnnualAverageTemperature, bs = "cr"), data = Canopy)
##
mod_gam1 = gam(Ci ~s(PRCP, bs = "cr"), data = US)
