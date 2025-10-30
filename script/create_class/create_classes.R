library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(magrittr)
library(stringr)
library(soiltexture)
library(reshape2)

df<-read_xlsx("./input/data/all_dis_cov.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))

names(df)


################################################################################
#---------------Soil properties
################################################################################


# SOC classes

#---1:"<5"
#---2:"5-10"
#---3:"<10"

soc_mat <- c(0, 5, 1,
             5, 10,2,
             10, 180,3)

soc_rclmat  <- matrix(soc_mat, ncol=3, byrow=TRUE)

dat_soc<- df %>% dplyr::mutate(soc_class = case_when(
    (soc >= soc_rclmat [1,1] & soc < soc_rclmat [1,2])~soc_rclmat [1,3],
    (soc >= soc_rclmat [2,1] & soc < soc_rclmat [2,2])~soc_rclmat [2,3],
    (soc >= soc_rclmat [3,1] & soc < soc_rclmat [3,2])~soc_rclmat [3,3],
    ))

# P classes
p_mat <- c(0, 10.9, 1,
           10.9, 21.4,2,
           21.4, 185,3)

summary(df$phosphorus)

p_rclmat  <- matrix(p_mat, ncol=3, byrow=TRUE)

dat_p<- df %>% dplyr::mutate(p_class = case_when(
    (phosphorus >= p_rclmat [1,1] & phosphorus < p_rclmat [1,2])~p_rclmat [1,3],
    (phosphorus >= p_rclmat [2,1] & phosphorus < p_rclmat [2,2])~p_rclmat [2,3],
    (phosphorus >= p_rclmat [3,1] & phosphorus < p_rclmat [3,2])~p_rclmat [3,3],
    ))

# Soil texture

# Bind all with xy
df_all_xy<-bind_cols(df$clay,df$silt,df$sand) %>%
                            dplyr::rename(Clay=1,
                                          Silt=2,
                                          Sand=3)

# Get data for texture alone
df_all_xy<-bind_cols(df$clay,df$silt,df$sand)
my.text<- df_all_xy %>% dplyr::select(1,2,3) %>%
                            dplyr::rename(CLAY=1,
                                          SILT=2,
                                          SAND=3)
my.text<-as.data.frame(my.text)
my.text_nor <- TT.normalise.sum(tri.data = my.text)

# Get classes
tex_clas<-TT.points.in.classes( 
  tri.data    = my.text, 
  class.sys   = "HYPRES.TT"  
)  


# 1: fine (clay, silty clay loam, clay loam, and sandy clay) : cl SiClLo ClLo SiCl SaCl SaClLo
# 2: medium (silt loam and loam) Si SiLo Lo
# 3: coarse (sandy loam and sandy) SaLo Sa LoSa 

df_tex_clas <- as.data.frame(unlist(tex_clas))

head(df_tex_clas)

# Create one texture class based on classification---3 groups as you suggested and change like you want
df_texture <- df_tex_clas %>% mutate(texture =case_when(VF==1 & F==1 & M==0 & MF==0 & C==0 ~ "1",
                                                        VF==0 & F==1 & M==0 & MF==0 & C==0 ~ "1",
                                                        VF==1 & F==0 & M==0 & MF==0 & C==0 ~ "1",
                                                        VF==0 & F==0 & M==1 & MF==1 & C==0 ~ "2",
                                                        VF==0 & F==0 & M==1 & MF==0 & C==0 ~ "2",
                                                        VF==0 & F==0 & M==0 & MF==1 & C==0 ~ "2",
                                                        VF==0 & F==0 & M==0 & MF==0 & C==1 ~ "3"))

df_texture$texture<-as.factor(df_texture$texture)
summary(df_texture$texture)
# BD classes

# 1: BD < 1.2 kg/dm3
# 2: 1.2 kg/dm3 < BD < 1.47 kg/dm3
# 3: BD > 1.47 kg/dm3

bd_mat <- c(0, 1.2, 1,
             1.2, 1.47,2,
             1.47, 1.7,3)

bd_rclmat  <- matrix(bd_mat, ncol=3, byrow=TRUE)

dat_bd<- df %>% dplyr::mutate(bd_class = case_when(
    (bd >= bd_rclmat [1,1] & bd < bd_rclmat [1,2])~bd_rclmat [1,3],
    (bd >= bd_rclmat [2,1] & bd < bd_rclmat [2,2])~bd_rclmat [2,3],
    (bd >= bd_rclmat [3,1] & bd < bd_rclmat [3,2])~bd_rclmat [3,3],
    ))

################################################################################
#---------------Topography
################################################################################

# DEM
# 1: Elevation < 250 m
# 2: 250 m < Elevation < 1000 m 
# 3: Elevation > 1000 m 

dem_mat <- c(0, 250, 1,
             250, 1000,2,
             1000, 4000,3)

dem_rclmat  <- matrix(dem_mat, ncol=3, byrow=TRUE)

dat_dem<- df %>% dplyr::mutate(dem_class = case_when(
    (dem >= dem_rclmat [1,1] & dem < dem_rclmat [1,2])~dem_rclmat [1,3],
    (dem >= dem_rclmat [2,1] & dem < dem_rclmat [2,2])~dem_rclmat [2,3],
    (dem >= dem_rclmat [3,1] & dem < dem_rclmat [3,2])~dem_rclmat [3,3],
    ))

summary(dat_dem$dem_class)

df$dem

# Slope classes

slope_mat <- c(0, 0.20, 1,
             0.20, 1.00,2,
             1.00, 5.00,3,
             5.00, 15.00,4,
             15.00, 80,5)

slope_rclmat  <- matrix(slope_mat, ncol=3, byrow=TRUE)

dat_slope<- df %>% dplyr::mutate(slope_class = case_when(
    (slope >= slope_rclmat [1,1] & slope < slope_rclmat [1,2])~slope_rclmat [1,3],
    (slope >= slope_rclmat [2,1] & slope < slope_rclmat [2,2])~slope_rclmat [2,3],
    (slope >= slope_rclmat [3,1] & slope < slope_rclmat [3,2])~slope_rclmat [3,3],
    (slope >= slope_rclmat [4,1] & slope < slope_rclmat [4,2])~slope_rclmat [4,3],
    (slope >= slope_rclmat [5,1] & slope < slope_rclmat [5,2])~slope_rclmat [5,3],
    ))

summary(as.factor(dat_slope$slope_class))


################################################################################
#---------------Climate
################################################################################

# 1: GDD < 800°C/y
# 2: 800°C/y < GDD < 2700°C/y
# 3: 2700°C/y < GDD < 4000°C/y
# 4: 4000°C/y < GDD < 6000°C/y
# 5: GDD > 6000°C/y

# GDD
gdd_mat <- c(0,   800, 1,
            800, 2700,  2,
            2700, 4000,  3,
            4000, 6000, 4,
            6000, 10000, 5)

# # Reclassify the rasters
gdd_rclmat <- matrix(gdd_mat, ncol=3, byrow=TRUE)

dat_gdd_maize<- df %>% mutate(GDD_maize_class = case_when(
    (GDD_maize >= gdd_rclmat [1,1] & GDD_maize < gdd_rclmat [1,2])~gdd_rclmat [1,3],
    (GDD_maize >= gdd_rclmat [2,1] & GDD_maize < gdd_rclmat [2,2])~gdd_rclmat [2,3],
    (GDD_maize >= gdd_rclmat [3,1] & GDD_maize < gdd_rclmat [3,2])~gdd_rclmat [3,3],
    (GDD_maize >= gdd_rclmat [4,1] & GDD_maize < gdd_rclmat [4,2])~gdd_rclmat [4,3],
    (GDD_maize >= gdd_rclmat [5,1] & GDD_maize < gdd_rclmat [5,2])~gdd_rclmat [5,3],
    ))

summary(as.factor(dat_gdd_maize$GDD_maize_class))

dat_gdd_wheat<- df %>% mutate(GDD_wheat_class = case_when(
    (GDD_wheat >= gdd_rclmat [1,1] & GDD_wheat < gdd_rclmat [1,2])~gdd_rclmat [1,3],
    (GDD_wheat >= gdd_rclmat [2,1] & GDD_wheat < gdd_rclmat [2,2])~gdd_rclmat [2,3],
    (GDD_wheat >= gdd_rclmat [3,1] & GDD_wheat < gdd_rclmat [3,2])~gdd_rclmat [3,3],
    (GDD_wheat >= gdd_rclmat [4,1] & GDD_wheat < gdd_rclmat [4,2])~gdd_rclmat [4,3],
    (GDD_wheat >= gdd_rclmat [5,1] & GDD_wheat < gdd_rclmat [5,2])~gdd_rclmat [5,3],
    ))

dat_gdd_rice<- df %>% mutate(GDD_rice_class = case_when(
    (GDD_rice >= gdd_rclmat [1,1] & GDD_rice < gdd_rclmat [1,2])~gdd_rclmat [1,3],
    (GDD_rice >= gdd_rclmat [2,1] & GDD_rice < gdd_rclmat [2,2])~gdd_rclmat [2,3],
    (GDD_rice >= gdd_rclmat [3,1] & GDD_rice < gdd_rclmat [3,2])~gdd_rclmat [3,3],
    (GDD_rice >= gdd_rclmat [4,1] & GDD_rice < gdd_rclmat [4,2])~gdd_rclmat [4,3],
    (GDD_rice >= gdd_rclmat [5,1] & GDD_rice < gdd_rclmat [5,2])~gdd_rclmat [5,3],
    ))

dat_gdd_soybean<- df %>% mutate(GDD_soybean_class = case_when(
    (GDD_soybean >= gdd_rclmat [1,1] & GDD_soybean < gdd_rclmat [1,2])~gdd_rclmat [1,3],
    (GDD_soybean >= gdd_rclmat [2,1] & GDD_soybean < gdd_rclmat [2,2])~gdd_rclmat [2,3],
    (GDD_soybean >= gdd_rclmat [3,1] & GDD_soybean < gdd_rclmat [3,2])~gdd_rclmat [3,3],
    (GDD_soybean >= gdd_rclmat [4,1] & GDD_soybean < gdd_rclmat [4,2])~gdd_rclmat [4,3],
    (GDD_soybean >= gdd_rclmat [5,1] & GDD_soybean < gdd_rclmat [5,2])~gdd_rclmat [5,3],
    ))

# Bind alls
df_all<-bind_cols(df,dat_dem$dem_class,dat_slope$slope_class,
                  dat_soc$soc_class,dat_bd$bd_class,dat_p$p_class,
                  df_texture$texture,dat_gdd_maize$GDD_maize_class,
                  dat_gdd_wheat$GDD_wheat_class,dat_gdd_rice$GDD_rice_class,
                  dat_gdd_soybean$GDD_soybean_class)

names(df_all)

df_final<-df_all %>% dplyr::rename(dem_class=90,slope_class=91,
                                   soc_class=92, bd_class=93, p_class=94,
                                   texture=95,gdd_maize_class=96,
                                   gdd_wheat_class=97,gdd_rice_class=98, 
                                   gdd_soybean_class=99)

head(df_final[,89:99])

write_xlsx(df_final,"./input/data/all_dis_cov2_last.xlsx")
