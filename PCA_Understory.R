####PCA

PCA_Understory = prcomp(Understory_Ax, scale = TRUE)
fviz_eig(PCA_Understory)

fviz_pca_ind(PCA_Understory,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(PCA_Understory,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

