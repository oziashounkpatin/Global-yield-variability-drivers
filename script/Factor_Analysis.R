# Install/load packages
library(caret)
library(xgboost)
library(dplyr)
library(spdep)        # For spatial weights
library(blockCV)      # For spatial cross-validation
library(raster)       # If you have raster-based spatial layers
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
library(mlr) 
library(terra)
library(tmap)
library(scico)

# Load all the key covariates
# sg<-rast("D:/covariates/soil_grid.tif")[[7:8]]
# names(sg)<-c("soc","ar")
# soc<-rast("C:/Users/hounkpk1/Food_System/input/covariates/250mSoilgrids/soc.tif")
# p<-rast("C:/Users/hounkpk1/Food_System/input/covariates/phosphorus/phosphorus_proj.tif")
# sg_pj<-terra::project(sg,p)

#---define grid
area_grid<-rast("C:/Users/hounkpk1/Food_System/input/covariates/grid_5arcmin/areagrid.tif")
ar_p<-subset(area_grid, c("aridity","Phosphorus"))
ar<-ar_p[[1]]
p<-ar_p[[2]]

# Define classification rules
# Format: matrix with columns [lower_bound, upper_bound, new_value]
ar_classification_matrix <- matrix(c(
  0, 0.05, 1,   # Values 0-25 -> 1
  0.05, 0.20, 2,  # Values 25-50 -> 2
  0.20, 0.50, 3,  # Values 50-75 -> 3
  0.50, 0.65, 4,  # Values 50-75 -> 3
  0.65, 10, 5  # Values 50-75 -> 3
), ncol = 3, byrow = TRUE)

p_classification_matrix <- matrix(c(
  0, 5, 1,   # Values 0-25 -> 1
  5, 10, 2,  # Values 25-50 -> 2
  10, 20, 3,  # Values 50-75 -> 3
  20, 30, 4,  # Values 50-75 -> 4
  30, 200, 5  # Values 50-75 -> 4
), ncol = 3, byrow = TRUE)

# Classify the raster
classified_ar <- classify(ar, ar_classification_matrix)
classified_p <- classify(p, p_classification_matrix)

# Plot the original and classified rasters
par(mfrow = c(2, 2))
plot(ar, main = "Original Aridity Raster")
plot(classified_ar, main = "Classified Raster")
plot(p, main = "Original P Raster")
plot(classified_p, main = "Classified Raster")
# Reset to default (single plot layout)
par(mfrow = c(1, 1))

# Classify the raster
r1 <- classify(ar, ar_classification_matrix)
r2 <- classify(p, p_classification_matrix)

# Combine into unique class codes (e.g., 100*landcover + soiltype)
combined <- r1 * 100 + r2

# Optionally convert to factor (if categories are discrete)
combined <- as.factor(combined)

# Load mask
crop_mask<-rast("C:/Users/hounkpk1/Food_System/input/cropland_mask/mask_crop.tif")
combined_res<-resample(combined,crop_mask)
r_ag<- combined_res %>% terra::mask(crop_mask, inverse=F) 

# Get the frequency of cell values
value_counts <- freq(r_ag)

# Calculate the percentage for each value
value_counts$percentage <- (value_counts$count / ncell(r_ag)) * 100 
value_counts<-as.data.frame(value_counts) %>% arrange(desc(count))

# Print the frequency table with percentages
print(value_counts)

plot(r_ag)

# recode cell
r_rec<-r_ag
# NT=11, CC=12, OF=13, AF=14
r_recod<- r_rec %>% subst(1, 999) %>%
                    subst(2, 999) %>%
                    subst(101, 999) %>%
                    subst(102,999) %>%
                    subst(103,999) %>%
                    subst(104,999) %>%
                    subst(105, 999) %>%
                    subst(201, 999) %>%
                    subst(202,999) %>%
                    subst(203,999) %>%
                    subst(204,999) %>%
                    subst(205,999) %>%
                    subst(302, 999) %>%
                    subst(304, 999) %>%
                    subst(305,999) %>%
                    subst(401,999) %>%
                    subst(402,999) %>%
                    subst(403, 999) %>%
                    subst(404, 999) %>%
                    subst(405,999) %>%
                    subst(504,999) 

r_recod<-as.factor(r_recod)
levels(r_recod)

# Associate class labels
class_labels <- c("Semi-arid very low_P",	"Semi-arid_medium P",	"Humid very low P",
                  "Humid low P","Humid medium P",	"Humid very high P",	
                  "other Aridirty P level")
ID<-c(301, 303, 501, 502, 503, 505, 999)

# Define class labels
levels(r_recod) <- data.frame(ID = ID, Class = class_labels)  # Associate labels with raster values
levels(r_recod)

# Choose a scico palette with 7 colors
class_colors <- scico(7, palette = "vik", direction = 1)  # Replace "batlow" with your preferred palette
print(class_colors)
 
# Plot the raster with tmap
sf_shoreLine<-st_read("C:/Users/hounkpk1/Food_System/boundaries/land/coastline.shp")

af_map<-tm_shape(r_recod) +
  tm_raster(style = "cat", palette = class_colors, title = "") +
  tm_shape(sf_shoreLine) +
  tm_lines(col = "grey",lwd = 0.25)+
  tm_layout(legend.title.size = 1, 
            legend.text.size = 0.8,
            frame = FALSE,
            title.frame=NA,
            bg.color = NA, ) +
  tm_legend(outside = TRUE)

af_map

# Saving the maps
png_af_map<- af_map +
    tm_layout(legend.show=FALSE)

# save with tmap 
tmap_save(af_map,
          filename =  "./output/Factor_Analysis/class_af_AR_P.pdf",
          width = 80,units='mm', dpi = 450)

# save with tmap 
tmap_save(png_af_map,
          filename =  "./output/Factor_Analysis/class_af_AR_P.png",
          width = 80,units='mm', dpi = 450)

writeRaster(r_recod, "./output/Factor_Analysis/r_recod.tif")
writeRaster(r_rec, "./output/Factor_Analysis/r_ar_P.tif")

plot(r_recod)
