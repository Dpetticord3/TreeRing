Climate = na.omit(ClimateRecordNS) %>%
  group_by(Year) %>%
  filter(n()==12) 

HKK_NumericVariables <- read_excel("D:/00_Work/Tree Ring/Data/HKK_NumericVariables.xlsx")

Afzelia = HKK_NumericVariables %>%
  filter(Species == "Afzelia_xylocarpa")

Data = merge(Afzelia, Climate, by = "Year")

view(Data)
write.csv(Data, "Afzelia_Climate_PreCleaning.csv")

Afzelia_Climate= read.csv("Afzelia_Climate_PostCleaning.csv")

AC.pca = prcomp(Afzelia_Climate[,3:19], scale = TRUE)

fviz_eig(AC.pca)

fviz_pca_var(AC.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

Understory = Afzelia_Climate %>%
  filter(size == "8")

Canopy = Afzelia_Climate %>%
  filter(size != "8")
#make the model 
UnderstoryFit = lm(Ci ~Ca+Year+PRCP+TAVG+TMIN+TMAX, data  = Understory)
#summary statistics
summary(Understoryfit)
#plot residuals, other statistics
layout(matrix(c(1,2,3,4),2,2))
plot(Understoryfit)
#Stepwise
library(MASS)
UnderstoryStep = stepAIC(Understoryfit,direction = "both")
########################################################
#make model
CanopyFit = lm(Ci ~Ca+Year+PRCP+TAVG+TMIN+TMAX, data  = Canopy)
#summary statistics
summary(CanopyFit)
layout(matrix(c(1,2,3,4),2,2))
plot(CanopyFit)
#stepwise 
library(MASS)
Canopystep = stepAIC(CanopyFit,direction = "both")
