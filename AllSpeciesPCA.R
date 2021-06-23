####Categorical PCA
Canopy_VDS_Data_Climate = VDS_Data_Climate %>%
  filter(size == "27") %>%
  select(-size, -Year, -Tree_code) %>%
  relocate(Species, .before = DriestQPRCP)

Understory_VDS_Data_Climate = VDS_Data_Climate %>%
  filter(size == "8") %>%
  select(-size, -Year, -Tree_code) %>%
  relocate(Species, .before = DriestQPRCP)


CanopySpecies = as.factor(Canopy_VDS_Data_Climate$Species)
UnderstorySpecies = as.factor(Understory_VDS_Data_Climate$Species)

############################################################

ALL_Species_Canopy_PCA = prcomp(Canopy_VDS_Data_Climate[,2:15], scale = TRUE)
fviz_eig(ALL_Species_Canopy_PCA)

fviz_pca_biplot(ALL_Species_Canopy_PCA,
                col.var = "#2E9FDF", 
                col.ind = CanopySpecies, # color by groups
                palette = c("red","blue","gold","green"),
                addEllipses = TRUE, # Concentration ellipses
                ellipse.type = "confidence",
                legend.title = "Groups",
                repel = TRUE
)

fviz_pca_var(ALL_Species_Canopy_PCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
################################################################## Understory Trees

ALL_Species_Understory_PCA = prcomp(Understory_VDS_Data_Climate[,2:15], scale = TRUE)
fviz_eig(ALL_Species_Understory_PCA)

fviz_pca_biplot(ALL_Species_Understory_PCA,
                col.var = "#2E9FDF", 
                col.ind = UnderstorySpecies, # color by groups
                palette = c("red","blue","gold","green"),
                addEllipses = TRUE, # Concentration ellipses
                ellipse.type = "confidence",
                legend.title = "Groups",
                repel = TRUE
)

#############All species, size as a factor: 
size = as.factor(VDS_Data_Climate$size)

ALL_Species_SizeClass_PCA = prcomp(VDS_Data_Climate[,5:18], scale = TRUE)
fviz_eig(ALL_Species_SizeClass_PCA)

fviz_pca_biplot(ALL_Species_SizeClass_PCA,
                col.var = "#2E9FDF", 
                col.ind = size, # color by groups
                palette = c("red","blue"),
                addEllipses = TRUE, # Concentration ellipses
                ellipse.type = "confidence",
                legend.title = "Groups",
                repel = TRUE
)
