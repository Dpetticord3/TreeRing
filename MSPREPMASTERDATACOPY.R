#idea for the lag model. lets try this;
#load important packages: 
library(readxl)
library(tidyverse)
library(psych)
library(corrplot)
library(lmerTest)
library(emmeans)
library(RColorBrewer)

#PUBLICATION THEME 
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


#read in the data set from Nakhon Swan
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

#lets do some quick visualization of these things to see if they fit what we think
#the climate has looked like
plot(ClimateVariables$Year, ClimateVariables$MMean_Temperature)
#gets slightly warmer over time for mmean temp, and also the dry season rains less
#than the wet season. Sounds good!

#now we want to set up two data frames. The first, matching each year to 
#the tree ring record.
HKK_Ci_Ca_Record <- read_excel("D:/00_Work/Tree Ring/Data/HKK_Ci_Ca_Record.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "numeric", "numeric", "numeric",
                                             "numeric"))
#The first merge will be this record with our climate data
HKK_ClimateCi_Record = merge(HKK_Ci_Ca_Record, ClimateVariables, by = "Year")
#done. We can now do analysis on this

#we also would like to include a dataset including a lag effect. To that end, 
#we want to see how the previous years climate predicts the current years Ci.
#the best solution I have for this is relabeling all of the trees to be one year
#younger, so that their climate data matches the year before their actual Ci data
#this is how we do that: 
HKK_Ci_Ca_Record_LAG = HKK_Ci_Ca_Record %>%  mutate(Year = Year - 1)

HKK_ClimateCi_Record_LAG = merge(HKK_Ci_Ca_Record_LAG, ClimateVariables, by = "Year")

#ok we now have that set up as well. 


##start to visualize the data

#RealTime
RealTimeCorrelation = cor(HKK_ClimateCi_Record[,7:13])
corrplot(RealTimeCorrelation, type="upper", order="hclust",
         col=brewer.pal(n=6, name="RdYlGn"))
#Lag
LagCorrelation = cor(HKK_ClimateCi_Record_LAG[,7:13])
corrplot(LagCorrelation, type="upper", order="hclust",
         col=brewer.pal(n=6, name="RdYlGn"))

#interestingly the lag set up does seem to suggest
#there is a positive correlation between last years 'raininess' 
#and Ci in the current year. A very rainy wet season increases Ci,
##likewise, a rainier dry season increases Ci. A hotter year suggests
#lower Ci, as does a hotter rainy season or a hotter dry season
#all of these analyses are including both size classes of tree... we could
#start to look for correlations that may indicate size based physiological 
#differences

#RealTime
HKK_ClimateCi_Record_UNDERSTORY  = HKK_ClimateCi_Record %>%
  filter(size =="UNDERSTORY")
HKK_ClimateCi_Record_CANOPY = HKK_ClimateCi_Record %>%
  filter(size =="CANOPY")

#RealTime understory corrplot
RealTimeCorrelation_UNDERSTORY = cor(HKK_ClimateCi_Record_UNDERSTORY[,7:13])
corrplot(RealTimeCorrelation_UNDERSTORY, type="upper", order="hclust",
         col=brewer.pal(n=6, name="RdYlGn"))
#RealTime -canopy corrplot
RealTimeCorrelation_CANOPY = cor(HKK_ClimateCi_Record_CANOPY[,7:13])
corrplot(RealTimeCorrelation_CANOPY, type="upper", order="hclust",
         col=brewer.pal(n=6, name="RdYlGn"))
###
#Lag dataset: 

HKK_ClimateCi_Record_LAG_UNDERSTORY  = HKK_ClimateCi_Record_LAG %>%
  filter(size =="UNDERSTORY")
HKK_ClimateCi_Record_LAG_CANOPY = HKK_ClimateCi_Record_LAG %>%
  filter(size =="CANOPY")

#Lag Understory
LagCorrelation_UNDERSTORY = cor(HKK_ClimateCi_Record_LAG_UNDERSTORY[,7:13])
corrplot(LagCorrelation_UNDERSTORY, type="upper", order="hclust",
         col=brewer.pal(n=6, name="RdYlGn"))
#Lag Canopy
LagCorrelation_CANOPY = cor(HKK_ClimateCi_Record_LAG_CANOPY[,7:13])
corrplot(LagCorrelation_CANOPY, type="upper", order="hclust",
         col=brewer.pal(n=6, name="RdYlGn"))


####################
#There are also a variety of species level possible interactions and correlations 
#can do these on command if necessary

#lets instead focus on trying to run a model
#There  are some important things to consider when setting this up
#Again, we know that there must be some signal for increased Ca to increase Ci.
#That makes intuitive sense, if there is more Ca, the Ci increases. We are looking
#for other interactions between Ci and Ca that could be modified by climate
#This is because Ci/Ca balance is regulated by physiology - the closing and opening 
#of stomata (generally in response to water)
#we also know that there is a general increase in Ca over time, 
#and a general time signal for temperature as well. 
#but we need to include mixed effects in our model to account for weird individual
#trees and (potentially) weird years which may have outsized effects
#we are going to use the ci/ca ratio here, that way we can study the pattern in the 
#ratio and take away the Ca fertilization effect.
#we will include factor level terms like species and size (categorical variables)
#and we will include all of the continuous variables we generated above
#I am also including random effects for both the year and the individual tree

#here is the model run on real time assignments
MModel_RealTime = lmer(Ci_Ca_Ratio~size*Species*MMean_PRCP + 
                      size*Species*MMean_Temperature  +
                      size*Species*MMean_Temperature_DrySeason   +
                      size*Species*MMean_Temperature_WetSeason  +
                      size*Species*MMean_PRCP_DrySeason + 
                      size*Species*MMean_PRCP_WetSeason + 
                      (1|Tree_code) +(1|Year),
                     data = HKK_ClimateCi_Record)

#here is the model run on the lag year dataset
MModel_Lag = lmer(Ci_Ca_Ratio~size*Species*MMean_PRCP + 
                       size*Species*MMean_Temperature  +
                       size*Species*MMean_Temperature_DrySeason   +
                       size*Species*MMean_Temperature_WetSeason  +
                       size*Species*MMean_PRCP_DrySeason + 
                       size*Species*MMean_PRCP_WetSeason + 
                       (1|Tree_code) +(1|Year),
                       data = HKK_ClimateCi_Record_LAG)


###we have created the models, lets look into them now. We will start with RT
#do all the analysis and then switch to the lag year


#check for no pattern
plot(predict(MModel_RealTime), residuals(MModel_RealTime))
#check for bell curve
hist(residuals(MModel_RealTime))
#qqplot
qqnorm(residuals(MModel_RealTime)); qqline(residuals(MModel_RealTime))

anova(MModel_RealTime)
#checking third level interactions, are they too complicated, etc.
#arguments for including it on the grounds that they are important/ecologically
#relevant.#generally, if its experimental, keep the whole thing. Observational,
#maybe pare down the model, delicately with reverence to all its parts.

step(MModel_RealTime, reduce.random = FALSE)

RealTimeStepwise =  lmer(Ci_Ca_Ratio ~ size + Species + MMean_PRCP_DrySeason + 
MMean_PRCP_WetSeason + (1 | Tree_code) + (1 | Year) + size:Species + 
size:MMean_PRCP_DrySeason + Species:MMean_PRCP_DrySeason + 
Species:MMean_PRCP_WetSeason,
data = HKK_ClimateCi_Record)

#we've created this 'best model' from our automated model selection. Lets see 
#what the post hoc analysis says: 


emmeans(RealTimeStepwise, ~Species+size+MMean_PRCP_DrySeason+MMean_PRCP_WetSeason, 
 at = list(MMean_PRCP_DrySeason = 
 (quantile(HKK_ClimateCi_Record$MMean_PRCP_DrySeason, probs = c(.25,.5,.75))),
 MMean_PRCP_WetSeason = (quantile(HKK_ClimateCi_Record$MMean_PRCP_WetSeason, 
 probs = c(.25,.5,.75)))))

emtrends(RealTimeStepwise,~Species+size, var = "MMean_PRCP_DrySeason")
emtrends(RealTimeStepwise,~Species+size, var = "MMean_PRCP_WetSeason")

#Estimated marginal means plots to give us a visualization of contrasts
#between groups
emmip(RealTimeStepwise, Species~MMean_PRCP_DrySeason |size, 
      at =list(MMean_PRCP_DrySeason=
      (quantile(HKK_ClimateCi_Record$MMean_PRCP_DrySeason,
       probs = c(.25,.5,.75))))) +
       ggtitle("RealTime Model Pairwise Contrasts, Dry Season Precipitation")+
       theme_Publication()+
       scale_colour_Publication()

emmip(RealTimeStepwise, Species~MMean_PRCP_WetSeason |size, 
      at =list(MMean_PRCP_WetSeason=
     (quantile(HKK_ClimateCi_Record$MMean_PRCP_WetSeason,
      probs = c(.25,.5,.75))))) +
      ggtitle("RealTime Model Pairwise Contrasts, Wet Season Precipitation")+
      theme_Publication()+
      scale_colour_Publication()
#########################################################
#first impressions, the model chosen emphasizes rain over temperature
#I show the emmeans contrasts to show how different size/species combos
#respond to the model chosen best parameters: monthly average rainfall
#in the wet and the dry season
#This seems pretty informative to me: the rain in the wet season likely is 
#also an indicator for the sunlight.
#in the dry season, perhaps this reflects hydraulic stress


############################################################
#looking now at the lagged model: 


plot(predict(MModel_Lag), residuals(MModel_Lag))
#check for bell curve
hist(residuals(MModel_Lag))
#qqplot
qqnorm(residuals(MModel_Lag)); qqline(residuals(MModel_Lag))

anova(MModel_Lag)
#checking third level interactions

step(MModel_Lag, reduce.random = FALSE)

#the lagged model is even simpler. Uses basically just MMeanTempWet season

LagStepwise =  lmer(Ci_Ca_Ratio ~size + Species + MMean_Temperature_WetSeason + 
                      (1 | Tree_code) + (1 | Year) + size:Species,
                         data = HKK_ClimateCi_Record_LAG)

#we've created this 'best model' from our automated model selection. Lets see 
#what the post hoc analysis says: 


emmeans(LagStepwise, ~Species+size+MMean_Temperature_WetSeason, 
        at = list(MMean_Temperature_WetSeason = 
                    (quantile(HKK_ClimateCi_Record_LAG$MMean_Temperature_WetSeason,
                              probs = c(.25,.5,.75)))))

emtrends(LagStepwise,~Species+size, var = "MMean_Temperature_WetSeason")

#Estimated marginal means plots to give us a visualization of contrasts
#between groups
emmip(LagStepwise, Species~MMean_Temperature_WetSeason |size, 
      at =list(MMean_Temperature_WetSeason=
                 (quantile(HKK_ClimateCi_Record$MMean_Temperature_WetSeason,
                           probs = c(.25,.5,.75))))) +
  ggtitle("One Year Delayed Pairwise Contrasts, Wet Season Precipitation")+
  theme_Publication()+
  scale_colour_Publication()


