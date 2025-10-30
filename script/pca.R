library(tidyverse)
library(metafor)
library(meta)
library(readxl)
library(writexl)
library('corrr')
library(factoextra)
library(fastDummies)

#install.packages("FactoMineR")
library("FactoMineR")

install.packages("ggcorrplot")
library(ggcorrplot)


# Upload data
df<-read_xlsx("./input/management/all_dis_cov.xlsx") %>%
            filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass")) %>%
            filter(!is.na(effectSize)) %>%
            dplyr::select(effectSize,Crop_Group,aridity,pH,soc,phosphorus,
                          bd,sand,silt,clay,dem,slope,wrb,kg_clim,landform)

library(PCAmixdata)
library(FactoMineR)
library(ade4)
df<-na.omit(df)

# PCAmix (PCAmixdata)
split <- splitmix(df)

head(split)

pcamix <- PCAmix(X.quanti=split$X.quanti,
                 X.quali=split$X.quali,
                 rename.level=TRUE, 
                 graph=FALSE)
# FAMD (FactoMineR)
famd <- FactoMineR::FAMD(df, graph = FALSE, ncp = 4)

# dudi.mix (ade4)
# dudimix <- ade4::dudi.mix(df,scannf = FALSE, nf = 4)

head(pcamix$scores)
head(famd$ind$coord)

# Eigenvalues
pcamix$eig[1:5,1]
famd$eig[,1]

# variables
pcamix$sqload
famd$var$coord

# Numerical variables coordinates (correlations)
pcamix$quanti$coord
famd$quanti.var$coord

#Plots of the observations
n <- nrow(df)
100/n # mean contribution

# plot(pcamix,choice="ind", lim.contrib.plot = 0.5, cex=0.8)
plot(famd, choix="ind")

famd$call

library("factoextra")
eig.val <- get_eigenvalue(famd)
head(eig.val)
# Correlation circle

plot(pcamix, choice = "cor")
plot(famd, choix = "quanti")

# cat_var<-df %>% select(wrb,kg_clim,landform) %>% 
#                 data.matrix(cat_var) %>%
#                 dplyr::mutate(across(1:3, as.factor)) %>%
#                 fastDummies::dummy_cols() %>%
#                 select(4:39)
# 
# # Check null values
# colSums(is.na(df))
# 
# # Normalize data
# data_normalized <- df %>% select(aridity:slope) %>%
#                         scale()
# 
# pca_data<-bind_cols(data_normalized,cat_var)
#   
# data.pca <- princomp(pca_data)
# summary(data.pca)
# 
# data.pca$loadings[, 1:4]
# fviz_eig(data.pca, addlabels = TRUE)
# 
# # Graph of the variables
# fviz_pca_var(data.pca, col.var = "black")
# fviz_cos2(data.pca, choice = "var", axes = 1:4)
# fviz_pca_var(data.pca, col.var = "cos2",
#             gradient.cols = c("black", "orange", "green"),
#             repel = TRUE)
# 
# 
