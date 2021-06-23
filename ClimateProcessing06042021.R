#climate data processing
ClimateRecordNS=read.csv("D:/GithubRepos/TreeRing/climateRecordNakhonSawan.csv")

Climate = na.omit(ClimateRecordNS) %>%
  group_by(Year) %>%
  filter(n()==12) 

VDS_Data <- read_excel("HKK_AllTrees_Ca_Ci.xlsx")

VDS_Data_Climate_PreProcessing = merge(VDS_Data, Climate, by = "Year")

#driest quarter (January to March)

VDS_Dry = VDS_Data_Climate_PreProcessing %>%
  filter(Month <4)

VDS_TotalPRCP_DRYQ = VDS_Dry %>%
  group_by(Tree_code, Year,Species, size) %>%
  summarize(DriestQPRCP = sum(PRCP))

VDS_Wettest_Month = VDS_Data_Climate_PreProcessing %>%
  group_by(Tree_code,Year,Species, size) %>%
  summarize(WettestMonthPRCP = max(PRCP))

VDS_Driest_Month = VDS_Data_Climate_PreProcessing %>%
  group_by(Tree_code,Year, Species, size) %>%
  summarize(DriestMonthPRCP = min(PRCP))

VDS_TotalPRCP = VDS_Data_Climate_PreProcessing %>%
  group_by(Tree_code, Year,Species, size) %>%
  summarize(TotalPRCP = sum(PRCP))

VDS_Hottest_Month = VDS_Data_Climate_PreProcessing %>%
  group_by(Tree_code, Year,Species, size) %>%
  summarize(HottestMonthTMAX = max(TMAX))

VDS_Hottest_MonthAVG = VDS_Data_Climate_PreProcessing %>%
  group_by(Tree_code,Year, Species, size) %>%
  summarize(HottestMonthTAVG = max(TAVG))

VDS_Coolest_MonthAVG = VDS_Data_Climate_PreProcessing %>%
  group_by(Tree_code,Year,Species,  size) %>%
  summarize(CoolestMonthTAVG = min(TAVG))

VDS_Coolest_MonthMin = VDS_Data_Climate_PreProcessing %>%
  group_by(Tree_code,Year,Species,  size) %>%
  summarize(CoolestMonthTMIN = min(TMIN))

VDS_TotalTemperature = VDS_Data_Climate_PreProcessing %>%
  group_by(Tree_code,Year,Species,  size) %>%
  summarize(TotalAnnualTemperature = sum(TAVG))

x = merge(VDS_TotalPRCP_DRYQ,VDS_Wettest_Month, by = c("Tree_code", "size", "Species","Year"))
x = merge(x,VDS_Driest_Month, by = c("Tree_code", "size","Species", "Year"))
x = merge(x,VDS_TotalPRCP, by = c("Tree_code", "size","Species", "Year"))
x = merge(x,VDS_Hottest_Month, by = c("Tree_code", "size", "Species","Year"))
x = merge(x,VDS_Hottest_MonthAVG, by = c("Tree_code", "size", "Species","Year"))
x = merge(x,VDS_Coolest_MonthAVG, by = c("Tree_code", "size", "Species","Year"))
x = merge(x,VDS_Coolest_MonthMin, by = c("Tree_code", "size", "Species","Year"))
x = merge(x,VDS_TotalTemperature, by = c("Tree_code", "size", "Species","Year"))
x = x %>%
  mutate(PRCP_Seasonality = DriestQPRCP/TotalPRCP)
x = x %>%
  mutate(HEAT_Seasonality = HottestMonthTAVG/TotalAnnualTemperature)
VDS_Data_Climate = x %>%
  mutate(COOL_Seasonality = CoolestMonthTAVG/TotalAnnualTemperature)

VDS_Data_Climate = merge(VDS_Data_Climate, VDS_Data, c("Tree_code", "size", "Species","Year"))
#######################
#########################


VDS_Data_Ax = VDS_Data_Climate %>%
  filter(Species== "Afzelia_xylocarpa")


Ax_Climate = merge(x, VDS_Data_Ax, by = c("Tree_code", "size", "Year"))



