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

# Load single maps
wrb_soil<- rast("./input/covariates/wrb/wrb.tif")
names(wrb_soil)<-"wrb_new"

# Load all data
dat<-read_xlsx("./input/data/all_dis_cov1.xlsx",guess_max = 10000)

# convert to spatial data
sel_dat <- dat %>% drop_na(x,y)
sp_dat <- sel_dat %>% 
  drop_na(y)%>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)
ter_data<-vect(sp_dat)

# Soil grids # y coord. ref homolosine
ext_soil <- as.data.frame(terra::extract(wrb_soil, ter_data, 
                          na.rm = TRUE,bind=T,ID=F)) 
head(ext_soil)

ext_soil$wrb<-NULL

ext_soil<-bind_cols(dat[,1:2],ext_soil)

write_xlsx(ext_soil, "./input/data/reg_data.xlsx" )
write_xlsx(ext_soil, "C:/RAP_Drivers/input/data/all_dis_cov3.xlsx" )
