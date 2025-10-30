library(dplyr)
library(tidyverse)
library(terra)
library(readxl)
library(writexl)
library(sf)
library(ggplot2)
library(plyr)
library(rgdal)
library(tidyterra)
library(terra)

# Gere grid
area_grid<-rast("C:/Users/hounkpk1/Food_System/input/covariates/grid_5arcmin/areagrid.tif")
dem<-area_grid["dem"]
sl<-area_grid["slope"]

n_dem<-rast("D:/opengeo_hub/dem_120m.tif")
sl120<-rast("D:/opengeo_hub/slope_120m.tif")
sl240<-rast("D:/opengeo_hub/slope_240m.tif")

dat<-read_xlsx("./input/data/reg_data.xlsx",guess_max = 1000) 

# convert to spatial data
sel_dat <- dat %>% drop_na(x,y)
sp_dat <- sel_dat %>% 
  drop_na(y)%>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)
ter_data<-vect(sp_dat)

# Soil grids # y coord. ref homolosine
ext_soil1 <- as.data.frame(terra::extract(sl120, ter_data)) 
ext_soil2 <- as.data.frame(terra::extract(sl240, ter_data)) 
ext_soil3 <- as.data.frame(terra::extract(n_dem, ter_data)) 

new_add<-bind_cols(dat$slope,ext_soil1[,2], ext_soil2[,2], ext_soil3[,2],dat$dem) %>%
dplyr::rename(slope=1,sl120=2,sl240=3,dem_240=4,dem=5) %>% select(1:5)

write_xlsx(new_add,"./input/data/new_slope_dem/dem_sl_data.xlsx")

