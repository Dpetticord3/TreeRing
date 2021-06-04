###
TotalPRCP = Afzelia_Climate %>%
  group_by(Year) %>%
  summarize(totalPRCP = sum(PRCP))
###
AnnualAvgPRCP = Afzelia_Climate %>%
  group_by(Year) %>%
  summarize(AnnualAvgPRCP = mean(PRCP))
###
AnnualTAVG = Afzelia_Climate %>%
  group_by(Year) %>%
  summarize(AnnualTAVG = mean(TAVG))
###
AnnualTMIN = Afzelia_Climate %>%
  group_by(Year) %>%
  summarize(AnnualTMIN = mean(TMIN))
###
AnnualTMAX = Afzelia_Climate %>%
  group_by(Year) %>%
  summarize(AnnualTMAX = mean(TMAX))
####
PRCPSD = Afzelia_Climate %>%
  group_by(Year) %>%
  summarize(PRCPSD = sd(PRCP))
####
TAVGSD = Afzelia_Climate %>%
  group_by(Year) %>%
  summarize(TAVGSD = sd(TAVG))

x = merge(TAVGSD,PRCPSD, by = "Year")
x = merge(x,AnnualTMAX, by = "Year")
x = merge(x,AnnualTMIN, by = "Year")
x = merge(x,AnnualTAVG, by = "Year")
x = merge(x,AnnualAvgPRCP, by = "Year")
x = merge(x,TotalPRCP, by = "Year")

ProcessedClimate = merge(x, Afzelia, by = "Year")
