# I want to run a model using climate variables from the concurrent year & include 
# the climate effects of the previous year, to capture if there is a lagged signal:
# ie in this scenario a particularly dry year the year before may affect carbon storage in the current year
# which I believe is an appropriate decision
# I have removed climate variables here which have a correlation coefficient greater than 0.55.
#

#
#Packages
#####
library(readxl)
library(tidyverse)
library(dplyr)
library(lme4)
library(ggplot2)
library(ggsignif)
library(emmeans)
library(olsrr)
library(caret)
library(pls)
library(psych)
library(RColorBrewer)
library(corrplot)
library(gridExtra)
#####

#PUBLICATION THEME 
######
theme_Publication <- function(base_size=13, base_family="Garamond") {
  library(grid)
  library(ggthemes)
  library(gridExtra)
  windowsFonts("Garamond" = windowsFont("Garamond"))
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}
######

#Climate Preprocessing
######################################
climateRecordNakhonSawan <- read.csv("D:/00_Work/Tree Ring/Data/climateRecordNakhonSawan.csv")
climateRecordNakhonSawan = na.omit(climateRecordNakhonSawan)
dim(climateRecordNakhonSawan)

# we want to eliminate years with no data for certain months
climateNS = climateRecordNakhonSawan %>%
  group_by(Year) %>%
  filter(n()>9) 
#we can visualize the distribution of the data in this hist. 
#looks like we get a pretty complete picture and more years if we keep 
#years with 10 measurements & up. 
#this does mean we want to use averages/means for our climate variables
#otherwise, years with fewer occurences could get screwed. 
#we will have a robust record for 1950-1990.Little spottier in older and 
#more recent years. 
hist(climateRecordNakhonSawan$Year, breaks = 200)
#and the edited dataset, should be more full: 
hist(climateNS$Year, breaks =200)

#now we can make some variables from these new years: 

#for example, we can determine the climate in the dry season(November-March)
VDS_DrySeason1 = climateNS %>%
  filter(Month <4)
VDS_DrySeason2 = climateNS %>%
  filter(Month >10)
#we need to shift our 'later months' to the previous year, so that we can appropriately capture the entire 'rainy season'
#otherwise, our 'rainy season' for each year is Jan-March, then a huge gap to Nov-Dec
#makes more sense to have a rainy season which runs from 'last year' in Nov through to the concurrent March.
VDS_DrySeason2 %>%  mutate(Year = Year - 1)


VDS_DrySeason =rbind(VDS_DrySeason1, VDS_DrySeason2)

#first, we can look into the precipitation in this rain season (adjusted)
#so that the 'rain year' doesn't overlap with the real 'year'
#does having a particularly wet or dry 'dry season' affect Ci storage
MMean_PRCP_DrySeason = VDS_DrySeason %>%
  group_by(Year) %>%
  summarize(MMean_PRCP_DrySeason = mean(PRCP))

#we can also include temperature during that quarter. So for example, 
#does the average temperature during the dry season predict Ci storage
MMean_Temperature_DrySeason = VDS_DrySeason %>%
  group_by(Year) %>%
  summarize(MMean_Temperature_DrySeason = mean(TAVG))

#we can also define a wet season
VDS_WetSeason = climateNS %>%
  filter(Month >4) %>%
  filter(Month<10)

#and summarily create the same variables (avg wetness, avg temp)
#to see if having a light rainy season that year, or a warm one, for example, 
#affects the ci term

MMean_PRCP_WetSeason = VDS_WetSeason %>%
  group_by(Year) %>%
  summarize(MMean_PRCP_WetSeason = mean(PRCP))

MMean_Temperature_WetSeason = VDS_WetSeason %>%
  group_by(Year) %>%
  summarize(MMean_Temperature_WetSeason = mean(TAVG))

##

#we can also estimate a years 'raininess' and 'temperature' as a whole, although
#note that this may be slightly off because certain years could be missing otherwise
#particularly rainy/dry months in the data. Rememeber we only preserved years 
#with 10 months of data

MMean_PRCP = climateNS %>%
  group_by(Year) %>%
  summarize(MMean_PRCP = mean(PRCP))

MMean_Temperature = climateNS %>%
  group_by(Year) %>%
  summarize(MMean_Temperature = mean(TAVG))

ClimateVariables = cbind(MMean_PRCP, MMean_Temperature,
                         MMean_PRCP_DrySeason, MMean_PRCP_WetSeason, MMean_Temperature_DrySeason, MMean_Temperature_WetSeason)
ClimateVariables = ClimateVariables[,!duplicated(colnames(ClimateVariables))]
######################################

#at the end of this we have a dataframe called ClimateVariables which has all our info
view(ClimateVariables)
corrplot(cor(ClimateVariables), type="upper", order="hclust",col=brewer.pal(n=10, name="RdYlGn"))
#we will avoid using variables that are highly colinear - like MMean_Temperature_DrySeason, which correlates highly with mmean Temp

#next we attach this climate data to our tree record
#############
#now we want to set up two data frames. The first, matching each year to 
#the tree ring record.
HKK_Ci_Ca_Record <- read_excel("D:/00_Work/Tree Ring/Data/HKK_Ci_Ca_Record.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "numeric", "numeric", "numeric",
                                             "numeric"))
HKK_ClimateCi_Record = merge(HKK_Ci_Ca_Record, ClimateVariables, by = "Year")
#we also would like to include a dataset including a lag effect. To that end, 
#we want to see how the previous years climate predicts the current years Ci.
#the best solution I have for this is relabeling all of the trees to be one year
#younger, so that their climate data matches the year before their actual Ci data
#this is how we do that: 
HKK_Ci_Ca_Record_LAG = HKK_Ci_Ca_Record %>%  mutate(Year = Year - 1)
HKK_ClimateCi_Record_LAG = merge(HKK_Ci_Ca_Record_LAG, ClimateVariables, by = "Year")

#Then, we want to bring the lag year together with the concurrent year's data into one df
HKK_Final = merge(HKK_ClimateCi_Record, HKK_ClimateCi_Record_LAG, by = "Tree_code", suffixes = c("", ".Previous_Year"))
head(HKK_Final)
HKK_Final = HKK_Final %>%
  select(-one_of("Year.Previous_Year", "Species.Previous_Year",  "size.Previous_Year", "Ca.Previous_Year", "Ci.Previous_Year", "Ci_Ca_Ratio.Previous_Year"))

head(HKK_Final)




#############
#at the end of this we have a new dataframe called HKKFinal which we will use to build our model
#we are going to use a linear mixed-effect model, because we need to consider the random effects of 
#our years and individual trees to rule out the effect of 'weird' individual plants or years
#we are also going to include interaction effects, because we have two categorical variables, size & species

HKK_LMER = lmer(Ci_Ca_Ratio~size*Species*MMean_PRCP + 
                                        size*Species*MMean_Temperature   +
                                        size*Species*MMean_PRCP_DrySeason  + 
                                        size*Species*MMean_PRCP.Previous_Year + 
                                        size*Species*MMean_Temperature.Previous_Year  +
                                        size*Species*MMean_PRCP_DrySeason.Previous_Year + 
                                        (1|Tree_code) +(1|Year),
                                      data =HKK_Final)
#summary_statistics
################
plot(predict(HKK_LMER), residuals(HKK_LMER))
#check for bell curve
hist(residuals(HKK_LMER))
#qqplot
qqnorm(residuals(HKK_LMER)); qqline(residuals(HKK_LMER))
################

#model analysis
######################
anova(HKK_LMER)

#model selection
step(HKK_LMER, reduce.random = FALSE)
#this might be problematic^

HKK_BestModel = lmer(Ci_Ca_Ratio ~ size + Species + MMean_PRCP + MMean_Temperature + MMean_PRCP_DrySeason + 
  (1 | Tree_code) + (1 | Year) + size:Species + size:MMean_PRCP + Species:MMean_PRCP + size:MMean_Temperature + 
  Species:MMean_Temperature + size:MMean_PRCP_DrySeason + Species:MMean_PRCP_DrySeason + size:Species:MMean_PRCP + 
  size:Species:MMean_Temperature, data = HKK_Final)

#summary_statistics
################
plot(predict(HKK_BestModel), residuals(HKK_BestModel))
#check for bell curve
hist(residuals(HKK_BestModel))
#qqplot
qqnorm(residuals(HKK_BestModel)); qqline(residuals(HKK_BestModel))
################


#model analysis
######################
anova(HKK_BestModel)

#posthoc analysis

HKK_BestModel_EMMS = as.data.frame(emmeans(HKK_BestModel, ~Species+size + MMean_PRCP  + MMean_Temperature  + MMean_PRCP_DrySeason, 
          at = list(MMean_PRCP = (quantile(HKK_Final$MMean_PRCP, probs = c(.25,.5,.75))),
          MMean_Temperature = (quantile(HKK_Final$MMean_Temperature, probs = c(.25,.5,.75))),
          MMean_PRCP_DrySeason = (quantile(HKK_Final$MMean_PRCP_DrySeason, probs = c(.25,.5,.75))))))

view(HKK_BestModel_EMMS)         


#here are all the environmental coefficents:
MMean_PRCP_Trends = as.data.frame(emtrends(HKK_BestModel,~Species+size, var = "MMean_PRCP"))
MMean_Temperature_Trends = as.data.frame(emtrends(HKK_BestModel,~Species+size, var = "MMean_Temperature"))
MMean_PRCP_DrySeason_Trends = as.data.frame(emtrends(HKK_BestModel,~Species+size, var = "MMean_PRCP_DrySeason"))

PairwiseResponse_Trends_DF = merge(MMean_PRCP_Trends, MMean_Temperature_Trends, by =c("Species","size"), 
                                   suffixes = c(".MMean_PRCP", ".MMean_Temp"))
PairwiseResponse_Trends_DF = merge(PairwiseResponse_Trends_DF, MMean_PRCP_DrySeason_Trends, by = c("Species", "size"),
                                   suffixes = c("","MMean_DrySeason_PRCP"))
#Estimated marginal means plots to give us a visualization of contrasts
######################

#between groups
#MMEAN PRCP
emmip(HKK_BestModel, Species~MMean_PRCP |size, 
      at =list(MMean_PRCP=
                 (quantile(HKK_Final$MMean_PRCP,
                           probs = c(.25,.5,.75))))) +
  ggtitle("Pairwise Contrasts, MMean PRCP")+
  theme_Publication()+
  scale_colour_Publication()

#Monthly mean temperature
emmip(HKK_BestModel, Species~MMean_Temperature |size, 
      at =list(MMean_Temperature=
                 (quantile(HKK_Final$MMean_Temperature,
                           probs = c(.25,.5,.75))))) +
  ggtitle("Pairwise Contrasts, MMean PRCP")+
  theme_Publication()+
  scale_colour_Publication()

#Monthly mean PRCP in the Dry Season (Previous Nov - Concurrent March)
emmip(HKK_BestModel, Species~MMean_PRCP_DrySeason |size, 
      at =list(MMean_PRCP_DrySeason=
                 (quantile(HKK_Final$MMean_PRCP_DrySeason,
                           probs = c(.25,.5,.75))))) +
  ggtitle("Pairwise Contrasts, MMean PRCP")+
  theme_Publication()+
  scale_colour_Publication()
######################

############paired bar plots (understory/canopy for response variables)
##########################
CANOPY_TRENDS = PairwiseResponse_Trends_DF %>%
  filter(size == "CANOPY")

UNDERSTORY_TRENDS = PairwiseResponse_Trends_DF %>%
  filter(size == "UNDERSTORY")

#format for nice single plot
ggplot(data= CANOPY_TRENDS) +
  geom_bar(stat = "summary", fun = mean, aes(x = Species, y = MMean_PRCP.trend, fill = Species), 
           alpha = 0.6, col = "black") +
  geom_hline(yintercept = 0,  linetype="dashed", color = "red") +
  scale_fill_Publication()+
  theme_Publication() +
  labs(
    x = "Species",
    y = "MMean_PRCP Trend",
    title = "Canopy Tree Response to MMean_PRCP")
##########

#here is all the plots I will stitch together: 
Canopy_PRCP = ggplot(data= CANOPY_TRENDS) +
  geom_bar(stat = "summary", fun = mean, aes(x = Species, y = MMean_PRCP.trend, fill = Species), 
           alpha = 0.6, col = "black") +
  geom_hline(yintercept = 0,  linetype="dashed", color = "red") +
  scale_fill_Publication()+
  theme_Publication() 
Canopy_Temp = ggplot(data= CANOPY_TRENDS) +
  geom_bar(stat = "summary", fun = mean, aes(x = Species, y = MMean_Temperature.trend, fill = Species), 
           alpha = 0.6, col = "black") +
  geom_hline(yintercept = 0,  linetype="dashed", color = "red") +
  scale_fill_Publication()+
  theme_Publication() 

Canopy_PRCP_DRY = ggplot(data= CANOPY_TRENDS) +
  geom_bar(stat = "summary", fun = mean, aes(x = Species, y = MMean_PRCP_DrySeason.trend, fill = Species), 
           alpha = 0.6, col = "black") +
  geom_hline(yintercept = 0,  linetype="dashed", color = "red") +
  scale_fill_Publication()+
  theme_Publication() 

US_PRCP= ggplot(data= UNDERSTORY_TRENDS) +
  geom_bar(stat = "summary", fun = mean, aes(x = Species, y = MMean_PRCP.trend, fill = Species), 
           alpha = 0.6, col = "black") +
  geom_hline(yintercept = 0,  linetype="dashed", color = "red") +
  scale_fill_Publication()+
  theme_Publication() 
  
US_TEMP=ggplot(data= UNDERSTORY_TRENDS) +
  geom_bar(stat = "summary", fun = mean, aes(x = Species, y = MMean_Temperature.trend, fill = Species), 
           alpha = 0.6, col = "black") +
  geom_hline(yintercept = 0,  linetype="dashed", color = "red") +
  scale_fill_Publication()+
  theme_Publication() 
  
US_PRCP_DRY=ggplot(data= UNDERSTORY_TRENDS) +
  geom_bar(stat = "summary", fun = mean, aes(x = Species, y = MMean_PRCP_DrySeason.trend, fill = Species), 
           alpha = 0.6, col = "black") +
  geom_hline(yintercept = 0,  linetype="dashed", color = "red") +
  scale_fill_Publication()+
  theme_Publication() 

grid.arrange(Canopy_PRCP, Canopy_Temp, Canopy_PRCP_DRY, US_PRCP, US_TEMP, US_PRCP_DRY, nrow = 2)

##########################