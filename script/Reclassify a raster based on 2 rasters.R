library(dplyr)
library(tidyverse)
library(data.table)
library(sf)
library(terra)
library(fasterRaster)
library(readxl)
library(writexl)


# https://bookdown.org/jgscott/DSGI/the-bootstrap.html
# https://rdrr.io/github/adamlilith/fasterRaster/man/fasterRaster.html
#Bootstrapping 95% confidence interval effect size ln mean bootes r lnrr


# #ph<-rast("input/covariates/grid_5arcmin/all_lastest_rast/r_soilgrids.tif")[[1]]
#ai<-rast("input/covariates/grid_5arcmin/all_lastest_rast/Aridity.tif")
#plot(as.factor(ai))
# 
#ph<-rast("C:/Users/hounkpk1/OneDrive - Aalto University/Maps/pH_250m/pH_0_30m.tif")
# 
# # creating vectors defining the values of interest and then indexing within a loop
# 
# # classify the pH values into three groups 
# # all values >= 0 and <= 6.3 become 1, etc.
# # Acidic	pH < 6.3
# # Neutral	6.3 < pH < 7.4
# # Alkaline	pH > 7.4
# 
ph_mat <- c(0, 6.3, 1,
       6.3, 7.4, 2,
       7.4, 11, 3)

# # Ariciy classes
# # Hyper-Arid	AI < 0.05
# # Arid	0.05 < AI < 0.2
# # Semiarid	0.2 < AI < 0.5
# # Sub-Humid	0.5 < AI < 0.65
# # Humid	AI > 0.65
# 
# 
ai_mat <- c(0,   0.05, 1,
            0.05, 0.2,  2,
            0.2, 0.5,  3,
            0.5, 0.65, 4,
            0.65, 11, 5)
# 
# # Reclassify the rasters
ph_rclmat  <- matrix(ph_mat, ncol=3, byrow=TRUE)
ai_rclmat <- matrix(ai_mat, ncol=3, byrow=TRUE)
# 
# 
# # Reclassify the maps
# r_ph_rcl <- classify(ph, ai_rclmat , include.lowest=TRUE, right=FALSE)
# r_ai_rcl <- classify(ai, ai_rclmat, include.lowest=TRUE, right=FALSE)
# 
# r_ph_rcl <-as.factor(r_ph_rcl)
# 
# # reproject ph
# ph_pj<-terra::project(r_ph_rcl,r_ai_rcl,method="near")
# ph_pj_res<-terra::resample(ph_pj,r_ai_rcl, method="near")
# 
# ph_ai<-c(ph_pj_res,r_ai_rcl)

# Save maps
# writeRaster(ph_pj_res, "C:/Food_System/Aridity_pH/pH_res.tif",overwrite=T)
# writeRaster(r_pH_class, "C:/Food_System/Aridity_pH/pH_250m.tif",overwrite=T)
# writeRaster(r_ai_rcl, "C:/Food_System/Aridity_pH/ai_5arc.tif",overwrite=T)
# writeRaster(ph_ai, "C:/Food_System/Aridity_pH/ph_ai.tif",overwrite=T)
# 
# # where is grass
# grassDir <- "C:/Program Files/GRASS GIS 8.4" 
# faster(grassDir = grassDir)
# 
# # Load the rasters
# ph<-rast("C:/Food_System/Aridity_pH/pH_res.tif")
# ai<-rast("C:/Food_System/Aridity_pH/ai_5arc.tif")
# 
# 
# # Convert spatraster to graster
# gph<-fast(ph)
# gph<-as.int(gph)
# 
# # convert to integer
# gai<-fast(ai)
# gai<-as.int(gai)
# 
# # create ph data levels
# ph_lev <- data.frame(
#      value = c(1, 2, 3),
#      ph = c("acid", "neutral", "alkaline")
# )
# 
# # create ai data levels
# ai_lev <- data.frame(
#      value = c(1, 2, 3,4,5),
#      ai= c("hyperarid", "arid", "semiarid","subhumid","humid")
# )
# 
# # assign levels to rasters
# 
# levels(gph) <- list(ph_lev)
# levels(gai) <- list(ai_lev)
# 
# # Combine categories
# combined_rast<-fasterRaster::combineCats(gph, gai)
# plot(combined_rast)
# 
# levels(combined_rast)
# 
# library(raster)
# r<-terra::rast(combined_rast)
# 
# 
# writeRaster(combined_rast, "C:/Food_System/Aridity_pH/Comb_ph_ai.tif",overwrite=T)

ar_ph<-rast("C:/Food_System/Aridity_pH/Comb_ph_ai.tif")

plot(as.factor(r))

# Load data
# # Load and prepare data -----------------------------------------------------#
df<-read_xlsx("./input/management/all_dis_cov.xlsx",guess_max = 100)
#head(as.data.frame(df))
names(df)

# Upload data
dat<-read_xlsx("./input/management/all_dis_cov1.xlsx",guess_max = 10000) %>%
                filter(key %in% c("AF","CC","NT","OF"))

names(dat)

dat$slope<-NULL

dat_ph<- dat %>% dplyr::mutate(ph_class = case_when(
    (pH >= ph_rclmat [1,1] & pH < ph_rclmat [1,2])~ph_rclmat [1,3],
    (pH >= ph_rclmat [2,1] & pH < ph_rclmat [2,2])~ph_rclmat [2,3],
    (pH >= ph_rclmat [3,1] & pH < ph_rclmat [3,2])~ph_rclmat [3,3],
    ))
     
dat_ai<- dat_ph %>% mutate(aridity_class = case_when(
    (aridity >= ai_rclmat [1,1] & aridity < ai_rclmat [1,2])~ai_rclmat [1,3],
    (aridity >= ai_rclmat [2,1] & aridity < ai_rclmat [2,2])~ai_rclmat [2,3],
    (aridity >= ai_rclmat [3,1] & aridity < ai_rclmat [3,2])~ai_rclmat [3,3],
    (aridity >= ai_rclmat [4,1] & aridity < ai_rclmat [4,2])~ai_rclmat [4,3],
    (aridity >= ai_rclmat [5,1] & aridity < ai_rclmat [5,2])~ai_rclmat [5,3],
    ))

table_leg<-read_xlsx("./Aridity_pH/legend.xlsx",guess_max = 10000) 

# convert to spatial data
sel_dat <- dat_ai %>% drop_na(x,y)
sp_dat <- sel_dat %>% 
  drop_na(y)%>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

slope<-rast("C:/Users/hounkpk1/OneDrive - Aalto University/Thesis_data/misc/slope.tif")

slope_ar_ph<-c(slope,ar_ph)

comb_sp<-terra::extract(slope_ar_ph, sp_dat,na.rm = TRUE,bind=T, ID=F) 
#comb_sp<-terra::extract(ai, comb_sp1,na.rm = TRUE,bind=T, ID=F) 

comb_df <- as.data.frame(comb_sp) %>% rename(ai_ph=r_cross_qAEESusM1PaP)

df1 <- comb_df %>% left_join(table_leg, join_by(ai_ph))
names(df1)

summary(as.factor(df1$ai_ph))

write_xlsx(df1,"./Next_Article/input/all_dis_cov.xlsx")

