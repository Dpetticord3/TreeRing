####
ggplot(SP,aes(x = PRCP, y = Ci, color = factor(Size)))+
  geom_point() +
  stat_smooth(method = "lm", 
              se = F, 
              size = 1)
model = lm(Ci~PRCP, data= Canopy)
summary(model)

model = lm(Ci~AnnualAverageTemperature, data= Canopy)
summary(model)


ggplot(SP,aes(x = AnnualAverageTemperature, y = Ci, color = factor(Size)))+
  geom_point() +
  stat_smooth(method = "lm", 
              se = F, 
              size = 1)
#########
ggplot(US,aes(x = PRCP, y = Ci, color = factor(Species)))+
         geom_point() +
         stat_smooth(method = "lm", 
                     se = F, 
                     size = 1)
##
ggplot(US,aes(x = AnnualAverageTemperature, y = Ci, color = factor(Species)))+
  geom_point() +
  stat_smooth(method = "lm", 
              se = F, 
              size = 1)
##
ggplot(Canopy,aes(x = PRCP, y = Ci, color = factor(Species)))+
  geom_point() +
  stat_smooth(method = "lm", 
              se = F, 
              size = 1)
##
ggplot(Canopy,aes(x = AnnualAverageTemperature, y = Ci, color = factor(Species)))+
  geom_point() +
  stat_smooth(method = "lm", 
              se = F, 
              size = 1)
##