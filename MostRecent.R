#First Step - Separate out Each Species, no Size Effect
#Afzelia
Afzelia_x = VDS_Data_Climate %>%
  filter(Species == "Afzelia_xylocarpa")
#Melia
Melia_a = VDS_Data_Climate %>%
  filter(Species == "Melia_azedarach")
#Chukrasia
Chukrasia  = VDS_Data_Climate %>%
  filter(Species == "Chukrasia_tabularis")