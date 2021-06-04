#add climate data to the tree ring record 

ThailandClimate <- read.csv("D:/00_Work/Tree Ring/Data/climateRecordThailand.csv")

head(ThailandClimate)

#####
#Different climate record 
climateRecordNakhonSawan <- read.csv("D:/00_Work/Tree Ring/Data/climateRecordNakhonSawan.csv")
climateRecordNakhonSawan = climateRecordNakhonSawan %>%
  drop_na()

ThailandClimate = climateRecordNakhonSawan
###################
Average = ThailandClimate %>%
  group_by(Year) %>%
  summarize(AnnualAverageTemperature = mean(TAVG))
view(Average)
#
Min = ThailandClimate %>%
  group_by(Year) %>%
  summarize(AnnualMinTemperature = min(TMIN))
#
Max = ThailandClimate %>%
  group_by(Year) %>%
  summarize(AnnualMaxTemperature = max(TMAX))
#
PRCP = ThailandClimate %>%
  group_by(Year) %>%
  summarize(PRCP = mean(PRCP))

TotalPRCP = ThailandClimate %>%
  group_by(Year)%>%
  summarize(TotalPRCP = sum(PRCP))
###################

Climate =merge(Average, Min)
Climate =merge(Climate,Max)
Climate = merge(Climate,PRCP)
Climate = merge(Climate,TotalPRCP)
#######

HKK_Correlation = merge(HKK_Numeric, Climate)

write.csv(HKK_Correlation, "HKKClimateCorrelation.csv")

HKK_CorrPlot = read_excel("HKKProjectCI .xlsx")
####
ggpairs(HKK_CorrPlot[,5:11], title = "Correlation of All HKK Trees & Climate")

####
ggpairs(HKK_CorrPlot[,5:11], ggplot2::aes(colour = HKK_CorrPlot$Size), title = "Size Class Differences in Correlation")

########
SP= read_excel("Cleaned.xlsx")
Growth = read_excel("Growth.xlsx")
####
Canopy = SP %>%
  filter(Size == "Canopy")
US = SP %>%
  filter(Size == "Understory")
################


ggpairs(Canopy[,3:7], title = "Species Level Differences in Canopy Trees")

ggpairs(US[,3:7], title = "Species Level Differences in Understory Trees")

pairs.panels(Canopy[,3:7],
             method = "pearson",
             hist.col = "blue",
             density = TRUE,
             ellipses = TRUE)

pairs.panels(US[,3:7],
             method = "pearson",
             hist.col = "red",
             density = TRUE,
             ellipses = TRUE)
