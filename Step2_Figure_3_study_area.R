
library(dplyr)
library(tidyverse)
library(terra)
library(readxl)
library(sf)
library(tmap)
library(ggplot2)
library(gridExtra)
library(patchwork)

theme_set(theme_bw())

# Upload data
dat<-read_xlsx("./input/rfps_data.xlsx", sheet = "full_data") %>%
                filter(key %in% c("AF","CC","NT","OF"))

sel_dat<-dat %>%
  dplyr::select(y, x, key, effectSize)

sel_dat$y<-as.numeric(sel_dat$y)
sel_dat$x<-as.numeric(sel_dat$x)

# convert to spatial data
sel_dat <- sel_dat %>% drop_na(x,y)
sp_dat <- sel_dat %>% 
  drop_na(y)%>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

af_sp<-sp_dat %>% dplyr::filter(key %in% "AF")
cc_sp<-sp_dat %>% dplyr::filter(key %in% "CC")
nt_sp<-sp_dat %>% dplyr::filter(key %in% "NT")
of_sp<-sp_dat %>% dplyr::filter(key %in% "OF")

study_area<-function(spat_data){
  
          # download admin 0 boundary
          world<- rnaturalearth::ne_countries(scale = 50) %>% 
          # remove antarctica
          filter(!iso_a3 == 'ATA')
   
          tm_shape(world, crs = "+proj=robin") +
          tm_polygons(col="lightgrey", fill="white",lwd = 0.1) +
          tm_shape(spat_data) +
            tm_symbols(size=0.1, #shape="key", 
                       col = "black", border.col = NA) +
            #tm_facets("key", ncol = 2, free.coords = FALSE) +
                 tm_layout(legend.show=FALSE,
                          bg.color = NA,
                          frame = FALSE)
}

af_map<-study_area(af_sp)
cc_map<-study_area(cc_sp)
nt_map<-study_area(nt_sp)
of_map<-study_area(of_sp)

# Output folder
out_dir <- "./output/recap_fig/fig5/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tmap_save(af_map,
          filename = str_c(out_dir, "Fig_1_1_AF.png"),
          width = 80,units='mm')

tmap_save(cc_map,
          filename = str_c(out_dir, "Fig_1__2_CC.png"),
          width = 80,units='mm')

tmap_save(nt_map,
          filename = str_c(out_dir, "Fig_1_3_NT.png"),
          width = 80,units='mm')

tmap_save(of_map,
          filename = str_c(out_dir, "Fig_1_4_OF.png"),
          width = 80,units='mm')

