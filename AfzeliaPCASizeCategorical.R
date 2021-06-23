####Categorical PCA

groups = as.factor(Ax_Climate$size)
PCA_Afzelia = prcomp(Ax_Climate[,c(2:14)], scale = TRUE)
fviz_eig(PCA_Afzelia)

fviz_pca_biplot(PCA_Afzelia,
             col.var = "#2E9FDF", # Color by contributions to the PC
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

fviz_pca_var(PCA_Afzelia,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
