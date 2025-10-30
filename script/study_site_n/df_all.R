# --- READ + NORMALIZE ---------------------------------------------------------
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(tibble)
library(tidyverse)


df1<-read_xlsx("./input/data/all_dis_cov2_last.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF"), !Crop_Group %in% c("Grass"))%>%
    # dplyr::select(effectSize,key,Crop_Group,kg_clim,aridity_class,ph_class,soc_class,
    #               p_class, bd_class, texture,gdd_maize_class, gdd_wheat_class,gdd_rice_class,
    #               gdd_soybean_class, dem_class,slope_class ) %>%
                   drop_na(Crop_Group) %>%
    dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others",
      "V_F_others",Crop_Group)) %>%
     mutate(ES=perc(effectSize)) #%>%
     # filter(!ES > 100) %>%
     # select(!effectSize)  %>%
     # dplyr::rename(effectSize=ES)

df2<-read_xlsx("./input/data/all_dis_cov2_last.xlsx",guess_max = 1000) %>%
    filter(key %in% c("CC"), !Crop_Group %in% c("Grass"))%>%
    # dplyr::select(effectSize,key,Crop_Group,kg_clim,aridity_class,ph_class,soc_class,
    #               p_class, bd_class, texture,gdd_maize_class, gdd_wheat_class,gdd_rice_class,
    #               gdd_soybean_class, dem_class,slope_class ) %>%
                  drop_na(Crop_Group) %>%
    dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others",
      "V_F_others",Crop_Group)) %>%
     mutate(ES=perc(effectSize)) #%>%
     # filter(!ES > 100) %>%
     # select(!effectSize)  %>%
     # dplyr::rename(effectSize1=ES)

df3<-read_xlsx("./input/data/all_dis_cov2_last.xlsx",guess_max = 1000) %>%
    filter(key %in% c("NT"), !Crop_Group %in% c("Grass"))%>%
    # dplyr::select(effectSize,key,Crop_Group,kg_clim,aridity_class,ph_class,soc_class,
    #               p_class, bd_class, texture,gdd_maize_class, gdd_wheat_class,gdd_rice_class,
    #               gdd_soybean_class, dem_class,slope_class ) %>%
                   drop_na(Crop_Group) %>%
    dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others",
      "V_F_others",Crop_Group)) %>%
     mutate(ES=perc(effectSize)) #%>%
     # filter(!ES > 100) %>%
     # select(!effectSize)  %>%
     # dplyr::rename(effectSize1=ES)


df4<-read_xlsx("./input/data/all_dis_cov2_last.xlsx",guess_max = 1000) %>%
    filter(key %in% c("OF"), !Crop_Group %in% c("Grass"))%>%
    # dplyr::select(effectSize,key,Crop_Group,kg_clim,aridity_class,ph_class,soc_class,
    #               p_class, bd_class, texture,gdd_maize_class, gdd_wheat_class,gdd_rice_class,
    #               gdd_soybean_class, dem_class,slope_class ) %>%
                   drop_na(Crop_Group) %>%
    dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others",
      "V_F_others",Crop_Group)) %>%
     mutate(ES=perc(effectSize)) #%>%
     # filter(!ES > 100) %>%
     # select(!effectSize)  %>%
     # dplyr::rename(effectSize1=ES)

df<-bind_rows(df1,df2,df3,df4)
dim(df)

names(df)
write_xlsx(df, "./study_site/df_all.xlsx")
dim(unique(df$references))

dim(df1)
dim(df2)
dim(df3)
dim(df4)
