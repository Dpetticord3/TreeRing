write.csv(VDS_Data_Climate, "VDS_Data_Climate.csv")
VDS_Data_Climate= read.csv("VDS_Data_Climate.csv")

#First Step - Separate out Each Species, no Size Effect
#Afzelia
Afzelia_x = VDS_Data_Climate %>%
  filter(Species == "Afzelia_xylocarpa")
Afzelia_US = Afzelia_x %>%
  filter(size == "8")
Afzelia_Canopy = Afzelia_x %>%
  filter(size != "8")
#Melia
Melia_a = VDS_Data_Climate %>%
  filter(Species == "Melia_azedarach")
Melia_US = Melia_a %>%
  filter(size == "8")
Melia_Canopy = Melia_a%>%
  filter(size != "8")
#Chukrasia
Chukrasia  = VDS_Data_Climate %>%
  filter(Species == "Chukrasia_tabularis")
Chukrasia_US = Chukrasia %>%
  filter(size == "8")
Chukrasia_Canopy = Chukrasia %>%
  filter(size != "8")

#Afzelia/Melia
Afzelia_Melia_US = merge(Afzelia_US, Melia_US)
