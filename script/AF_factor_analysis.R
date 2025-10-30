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

# Load single maps
ar_p<- rast( "./output/Factor_Analysis/r_recod.tif") 

# Convert ES to perc
perc<- function(data){
  data_conv=100*(exp(data) - 1)
  return(data_conv)
}

# Load all data
dat<-read_xlsx("./input/data/reg_data.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF"), !Crop_Group %in% c("Grass")) %>%
    dplyr::select(effectSize,x,y,aridity,pH,soc,phosphorus,bd,
                  sand, silt,clay,dem,landform,wrb_new) %>%
    mutate(ES=perc(effectSize)) %>%
    filter(!ES > 100) %>%
    dplyr::select(!effectSize)  %>%
    dplyr::rename(effectSize=ES, wrb=wrb_new) %>%
    relocate(last_col(), .after = y) %>%
    drop_na(effectSize) %>%
    mutate(across(c(landform, wrb), as.factor))

# convert to spatial data
sel_dat <- dat %>% drop_na(x,y)
sp_dat <- sel_dat %>% 
  drop_na(y)%>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)
ter_data<-vect(sp_dat)


# Soil grids # y coord. ref homolosine
df_es_ar_p <- as.data.frame(terra::extract(ar_p, ter_data, 
                          na.rm = TRUE,bind=T,ID=F)) 
df_es_ar_p$Class<-as.factor(df_es_ar_p$Class)
df_es_ar_p<-na.omit(df_es_ar_p)

# Create the boxplot
# Sample data
set.seed(123)

# Choose a scico palette
palette_colors <- c("#001260", "#06558B", "#71A7C4", "#EBE5E0", "#D29773", "#AA4613")  # "batlow" or try "roma", "oslo", "lajolla", etc.

names(df_es_ar_p)

# Create the boxplot
plt_map_es<-ggplot(df_es_ar_p, aes(x = Class, y = effectSize, fill = Class)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
  #geom_quasirandom(size = 1.5, alpha = 0.6, width = 0.2) +
  scale_fill_manual(values = palette_colors) +
  labs(
    title = "",
    x = NULL,
    y = "% change (crop yield)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black", size=20),
    axis.title = element_text(size=20),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

plt_map_es

ggsave(plt_map_es,filename = "./output/Factor_Analggsave(plt_map_es,filename = "./output/Factor_Analggsave(plt_map_es,filename = "./output/Factor_Analysis/af_boxplot.png",
       width = 50, height = 30, dpi = 450, units = "cm")