#add climate data to the tree ring record 

ThailandClimate <- read.csv("D:/00_Work/Tree Ring/Data/climateRecordThailand.csv")

head(ThailandClimate)
###################
Average = ThailandClimate %>%
  group_by(Year) %>%
  summarize(AverageTemperature = mean(Temperature....Celsius.))
view(Average)
#
Min = ThailandClimate %>%
  group_by(Year) %>%
  summarize(MinTemperature = min(Temperature....Celsius.))
#
max = ThailandClimate %>%
  group_by(Year) %>%
  summarize(maxTemperature = max(Temperature....Celsius.))
#
SD = ThailandClimate %>%
  group_by(Year) %>%
  summarize(sdTemperature = sd(Temperature....Celsius.))
###################

Climate =merge(Average, Min)
Climate =merge(Climate,max)
Climate = merge(Climate,SD)
#######

HKK_Correlation = merge(HKK_Numeric, Climate)
write.csv(HKK_Correlation, "HKKClimateCorrelation.csv")
HKK_CorrPlot = read.csv("HKKClimateCorrelation.csv")

ggpairs(HKK_CorrPlot[,4:11], ggplot2::aes(colour = HKK_CorrPlot$Species), title = "correlogram HKK")
