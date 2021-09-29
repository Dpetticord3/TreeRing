VDS_Data_Climate= read.csv("VDS_Data_Climate.csv")
VDS_Data_Climate = VDS_Data_Climate %>%
  select(-X)
#First Step - Separate out Each Species, no Size Effect
#Afzelia
Afzelia_x = VDS_Data_Climate %>%
  filter(Species == "Afzelia_xylocarpa") %>%
  select(-Species)
Afzelia_US = Afzelia_x %>%
  filter(size == "8") %>%
  select(-size)
Afzelia_Canopy = Afzelia_x %>%
  filter(size != "8") %>%
  select(-size)
Afzelia_x = Afzelia_x %>%
  select(-size)
#Melia
Melia_a = VDS_Data_Climate %>%
  filter(Species == "Melia_azedarach")%>%
  select(-Species)
Melia_US = Melia_a %>%
  filter(size == "8") %>%
  select(-size)
Melia_Canopy = Melia_a%>%
  filter(size != "8") %>%
  select(-size)
Melia_a = Melia_a %>%
  select(-size)
#Chukrasia
Chukrasia  = VDS_Data_Climate %>%
  filter(Species == "Chukrasia_tabularis")%>%
  select(-Species)
Chukrasia_US = Chukrasia %>%
  filter(size == "8") %>%
  select(-size)
Chukrasia_Canopy = Chukrasia %>%
  filter(size != "8") %>%
  select(-size)
Chukrasia = Chukrasia %>%
  select(-size)
#Afzelia/Melia
Afzelia_Melia_US = bind_rows(Afzelia_US, Melia_US)
Afzelia_Melia_Canopy = bind_rows(Afzelia_Canopy, Melia_Canopy)
Afzelia_Melia = bind_rows(Afzelia_x, Melia_a)

#Melia/Chukrasia
Melia_Chukrasia_US = bind_rows(Melia_US, Chukrasia_US)
Melia_Chukrasia_Canopy= bind_rows(Melia_Canopy, Chukrasia_Canopy)
Melia_Chukrasia = bind_rows(Melia_a, Chukrasia)

#Afzelia/Chukrasia 
Afzelia_Chukrasia_US =bind_rows(Afzelia_US, Chukrasia_US)
Afzelia_Chukrasia_Canopy = bind_rows(Afzelia_Canopy, Chukrasia_Canopy)
Afzelia_Chukrasia = bind_rows(Afzelia_x, Chukrasia)

#AfzeliaMeliaChukrasia
AfzeliaMeliaChukrasia_US = VDS_Data_Climate %>%
  filter(size == "8") %>%
  select(-size) %>%
  select(-Species)
AfzeliaMeliaChukrasia_Canopy = VDS_Data_Climate %>%
  filter(size != "8") %>%
  select(-size)%>%
  select(-Species)
AfzeliaMeliaChukrasia = VDS_Data_Climate %>%
  select(-size) %>%
  select(-Species)

