library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(magrittr)
library(stringr)
library(boot)
library(bootES)
library(rcompanion) 
library(writexl)
library("plotrix")
library(ggh4x)
library(showtext)
library(egg)
library(patchwork)
library(vctrs)

# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# VIP: https://www.w3online.net/article/learn_method__plotting_the_confidence_intervals_using_plotci()_function_2

# Convert ES to perc
perc<- function(data){
  data_conv=100*(exp(data) - 1)
  return(data_conv)
}

df_arid<-read_xlsx("./input/data/all_dis_cov3.xlsx",guess_max = 1000) %>%
    filter(key %in% c("OF"), !Crop_Group %in% c("Grass"),kg_clim %in% c("Arid"))%>%
        dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others",
      "V_F_others",Crop_Group)) %>%
     mutate(ES=perc(effectSize)) %>%
     filter(!ES > 100) %>%
     select(!effectSize)  %>%
     rename(effectSize=ES)
  
  
    #dplyr::select(effectSize,key,Crop_Group)
# 
# write_xlsx(df_joh,"./input/dataset_Ozias.xlsx")

df<-read_xlsx("./input/data/all_dis_cov2.xlsx",guess_max = 1000) %>%
    filter(key %in% c("OF"), !Crop_Group %in% c("Grass"),kg_clim %in% c("Arid"))%>%
    dplyr::select(effectSize,key,Crop_Group,N_input,soil_cover,weed_control,rotation,
                 kg_clim,aridity_class,ph_class,soc_class,
                  p_class, bd_class, texture,gdd_maize_class, gdd_wheat_class,gdd_rice_class,
                  gdd_soybean_class, dem_class,slope_class,landform) %>%
                  drop_na(Crop_Group) %>%
    dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others",
      "V_F_others",Crop_Group)) %>%
     mutate(ES=perc(effectSize)) %>%
    filter(!ES > 100) %>%
     select(!effectSize)  %>%
     rename(effectSize=ES)

# theme
tr_theme1<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.background = element_rect(fill='transparent',color=NA),
         legend.key=element_blank(),
         legend.box.background = element_rect(fill='transparent'),
         axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1),
         #axis.ticks = element_blank(),
         #axis.text = element_blank(),
         #axis.line = element_blank(),
         #axis.title=element_blank()
         #panel.border = element_rect(colour = "black", fill=NA, size=5)
       )

# draw plot 
hist_of<-ggplot(df, aes(x = effectSize)) + 
    geom_histogram(aes(y =after_stat(density)),
                   #breaks = seq(-50, 50, by = 10), 
                   colour = "black", 
                   fill = "white") +
       stat_function(fun = dnorm, 
              args = list(mean = mean(df$effectSize), 
              sd = sd(df$effectSize)))+
    tr_theme1

# function to boostrap
es_boot<-function(data)
  
{
  
library(dplyr)
library(tidyverse)
library(boot)
library(bootES)

set.seed(7)

my.mean <- function(x, d) {return(mean(x[d]))}
boot.out<-boot(data, statistic = my.mean,R=1000)
boot_ci<-bootES(data, ci.type="bca", R=1000, ci.conf=0.95,
                   plot=T,L =empinf(boot.out, index=1L, type="jack"))

all<-c(boot_ci$t0,boot_ci$bounds)

return(all)

}

#---@ overall
bt_all<-es_boot(df$effectSize)
bt_all<-as.data.frame(rbind(bt_all,bt_all))
# colnames(bt_all)<-c("Type", "mean","ci_low","ci_high")
bt_all<-as.data.frame(bt_all) %>%
        rownames_to_column(var = "Type") %>%
        dplyr::rename(mean=2, ci_low=3,ci_high=4) %>%
        slice_head()

cat<-c("overall")
cov<-c("no_cov")
all<-bind_cols(cov,cat,bt_all) %>%
     dplyr::rename(cov=1,cat=2)

all

#---@ for the managements
li_mgt<-split(df$effectSize,df$key)

bt_mgt<-as.data.frame(t(map_df(li_mgt, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

bt_mgt
cat<-c("OF")
cov<-c("mgt")
mgt<-bind_cols(cov,cat,bt_mgt) %>%
     dplyr::rename(cov=1,cat=2)
mgt

#---@ for the crop groups
li_crop<-split(df$effectSize,df$Crop_Group)

bt_crop<-as.data.frame(t(map_df(li_crop, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

cat<-c("Cash crop","Maize","V_F_others","Wheat")
cov<-c("crop","crop","crop","crop")
crop<-bind_cols(cov,cat,bt_crop) %>%
     dplyr::rename(cov=1,cat=2) %>%
     slice(2,4,1,3)
crop

# #---@ for the kg_clim
# li_kg_clim<-split(df$effectSize,df$kg_clim)
# 
# bt_kg_clim<-as.data.frame(t(map_df(li_kg_clim, es_boot))) %>%
#    rownames_to_column(var = "Type") %>%
#    dplyr::rename(mean=2, ci_low=3,ci_high=4)
# 
# cat<-c("Arid","Continental","Temperate","Tropical")
# cov<-c("no_cov","no_cov","no_cov","no_cov")
# clim<-bind_cols(cov,cat,bt_kg_clim) %>%
#      dplyr::rename(cov=1,cat=2)
# clim

#---@ for the aridity_class
li_aridity_class<-split(df$effectSize,df$aridity_class)

bt_aridity_class<-as.data.frame(t(map_df(li_aridity_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

# # Ariciy classes
# # Hyper-Arid	AI < 0.05
# # Arid	0.05 < AI < 0.2
# # Semiarid	0.2 < AI < 0.5
# # Sub-Humid	0.5 < AI < 0.65
# # Humid	AI > 0.65
bt_aridity_class

cat<-c("<0.05","0.05-0.20")
cov<-c("aridity index","aridity index")

aridity<-bind_cols(cov,cat,bt_aridity_class) %>%
     dplyr::rename(cov=1,cat=2) 

aridity

#---@ for the ph_class
li_ph_class<-split(df$effectSize,df$ph_class)

bt_ph_class<-as.data.frame(t(map_df(li_ph_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

# # Acidic	pH < 6.3
# # Neutral	6.3 < pH < 7.4
# # Alkaline	pH > 7.4

cat<-c("acidic")
cov<-c("pH")
pH<-bind_cols(cov,cat,bt_ph_class) %>%
     dplyr::rename(cov=1,cat=2)
pH

#---@ for the soc_class
li_soc_class<-split(df$effectSize,df$soc_class)

bt_soc_class<-as.data.frame(t(map_df(li_soc_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#---1:"<5"
#---2:"5-10"
#---3:"<10"

cat<-c("<5","5-10")
cov<-c("soc","soc")
soc<-bind_cols(cov,cat,bt_soc_class) %>%
     dplyr::rename(cov=1,cat=2)
soc

#---@ for Phosphorus
li_p_class<-split(df$effectSize,df$p_class)

bt_p_class<-as.data.frame(t(map_df(li_p_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

# P classes
# p_mat <- c(0, 10.9, 1,
#            10.9, 21.4,2,
#            21.4, 185,3)

cat<-c("10.9-21.4",">21.4")
cov<-c("phosphorus","phosphorus")
p<-bind_cols(cov,cat,bt_p_class) %>%
     dplyr::rename(cov=1,cat=2)
p

#---@ for the bd_class
li_bd_class<-split(df$effectSize,df$bd_class)

bt_bd_class<-as.data.frame(t(map_df(li_bd_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

# bd_mat <- c(0, 1.2, 1,
#              1.2, 1.47,2,
#              1.47, 1.7,3)

cat<-c("1.20-1.47")
cov<-c("bd")
bd<-bind_cols(cov,cat,bt_bd_class) %>%
     dplyr::rename(cov=1,cat=2)
bd

#---@ for the texture_class
li_texture_class<-split(df$effectSize,df$texture)

bt_texture_class<-as.data.frame(t(map_df(li_texture_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)


cat<-c("fine")
cov<-c("texture")
tex<-bind_cols(cov,cat,bt_texture_class) %>%
     dplyr::rename(cov=1,cat=2)
tex


#---@ for the gdd_maize_class
li_gdd_maize_class<-split(df$effectSize,df$gdd_maize_class)

summary(as.factor(df$gdd_maize_class))

bt_gdd_maize_class<-as.data.frame(t(map_df(li_gdd_maize_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

# gdd_mat <- c(0,   800, 1,
#             800, 2700,  2,
#             2700, 4000,  3,
#             4000, 6000, 4,
#             6000, 10000, 5)

cat<-c("GDD_maize3","GDD_maize4")
cov<-c("GDD_maize","GDD_maize")

gdd_maize<-bind_cols(cov,cat,bt_gdd_maize_class) %>%
     dplyr::rename(cov=1,cat=2) 

gdd_maize

#---@ for the gdd_wheat_class
li_gdd_wheat_class<-split(df$effectSize,df$gdd_wheat_class)

bt_gdd_wheat_class<-as.data.frame(t(map_df(li_gdd_wheat_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

cat<-c("GDD_wheat5")
cov<-c("GDD_wheat")
gdd_wheat<-bind_cols(cov,cat,bt_gdd_wheat_class) %>%
     dplyr::rename(cov=1,cat=2) 

gdd_wheat

# #---@ for the gdd_rice_class
# li_gdd_rice_class<-split(df$effectSize,df$gdd_rice_class)
# 
# bt_gdd_rice_class<-as.data.frame(t(map_df(li_gdd_rice_class, es_boot))) %>%
#    rownames_to_column(var = "Type") %>%
#    dplyr::rename(mean=2, ci_low=3,ci_high=4)
# 
# cat<-c("GDD_rice2","GDD_rice3","GDD_rice4","GDD_rice5")
# cov<-c("GDD_rice","GDD_rice",
#        "GDD_rice","GDD_rice")
# 
# gdd_rice<-bind_cols(cov,cat,bt_gdd_rice_class) %>%
#      dplyr::rename(cov=1,cat=2) 
# 
# gdd_rice
# 
# #---@ for the gdd_soybean_class
# li_gdd_soybean_class<-split(df$effectSize,df$gdd_soybean_class)
# 
# bt_gdd_soybean_class<-as.data.frame(t(map_df(li_gdd_soybean_class, es_boot))) %>%
#    rownames_to_column(var = "Type") %>%
#    dplyr::rename(mean=2, ci_low=3,ci_high=4)
# 
# cat<-c("GDD_soybean1","GDD_soybean2","GDD_soybean3","GDD_soybean4","GDD_soybean5")
# cov<-c("GDD_soybean","GDD_soybean","GDD_soybean",
#        "GDD_soybean","GDD_soybean")
# 
# gdd_soybean<-bind_cols(cov,cat,bt_gdd_soybean_class) %>%
#      dplyr::rename(cov=1,cat=2) 
# 
# gdd_soybean
# 
# #---@ for elevation
# dem_mat <- c(0, 250, 1,
#              250, 1000,2,
#              1000, 4000,3)


#---@ for the dem_class
li_dem_class<-split(df$effectSize,df$dem_class)

bt_dem_class<-as.data.frame(t(map_df(li_dem_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

cat<-c("250-1000",">1000")
cov<-c("dem","dem")
dem<-bind_cols(cov,cat,bt_dem_class) %>%
     dplyr::rename(cov=1,cat=2)
dem


#---@ for slope

#---@ for the slope_class
li_slope_class<-split(df$effectSize,df$slope_class)

bt_slope_class<-as.data.frame(t(map_df(li_slope_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

# # # slope classes
# slope_mat <- c(0, 0.20, 1,
#              0.20, 1.00,2,
#              1.00, 5.00,3,
#              5.00, 15.00,4,
#              15.00, 80,5)

cat<-c("0.2-1","1-5","5-15")
cov<-c("slope_class","slope_class","slope_class")

slope<-bind_cols(cov,cat,bt_slope_class) %>%
     dplyr::rename(cov=1,cat=2) 

slope

# WRB

wrb_full_cod<-c(0,1,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,20,22,23,24,26,29)
wrb_full_leg<-c("Acrisols","Albeluvisols","Alisols","Andosols","Arenosols",
       "Calcisols","Cambisols","Chernozems","Ferralsols","Fluvisols",	
       "Gleysols","Gypsisols","Histosols","Kastanozems","Leptosols",
       "Lixisols","Luvisols","Phaeozems","Plinthosols","Podzols",
       "Regosols","Solonetz","Vertisols")

wrb_full<-data.frame(cov=wrb_full_leg, cat=wrb_full_cod)

#---@ for the wrb
li_wrb<-split(df_arid$effectSize,df_arid$wrb)
li_wrb<-list_drop_empty(li_wrb)

bt_wrb<-as.data.frame(t(map_df(li_wrb, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#----------create data frame for wrb in AF
cat_wrb<-data.frame(cat=bt_wrb$Type) 
cat_wrb$cat<-as.double(cat_wrb$cat)

wrb_jn<- right_join(as.data.frame(wrb_full), 
            cat_wrb, 
            by="cat") 

wrb_bind<-bind_cols(wrb_jn, bt_wrb) %>%
     dplyr::rename(cov=1,cat=2)


#---@ for the Landform


  df$Class<-with(df,    ifelse(landform ==0, 0,
                        ifelse(landform ==1, 1,
                        ifelse(landform ==2, 2,
                        ifelse(landform ==3, 3,
                        ifelse(landform ==4, 4,
                        ifelse(landform ==5, 5,
                        ifelse(landform ==6, 6,
                        ifelse(landform ==7, 7,
                        ifelse(landform ==8, 8,
                        ifelse(landform ==9, 9,
                        ifelse(landform ==10, 9,
                        ifelse(landform ==11, 11,
                        ifelse(landform ==12, 11,
                        ifelse(landform ==13, 13,
                        ifelse(landform ==14, 13,
                        ifelse(landform ==15, 15,
                        ifelse(landform ==16, 15,
                        ifelse(landform ==17, 17,
                        ifelse(landform ==18, 17,
                        ifelse(landform ==19, 19,
                        ifelse(landform ==20, 19,
                        ifelse(landform ==21, 21,
                        ifelse(landform ==22, 21,0))))))))))))))))))))))))
       
l_df<- df %>% filter(!landform==0)

Class_full_cod<-c(1,2,3,4,5,6,7,8,9,11,13,15,17,19,21)
Class_full_leg<-c("Mtn_sumt","Cliff_sl","Lwhi_mtn","Shills_dcsl","Lhgsl_steep",
       "Lhgsl_mod","Mtn_vs","Mod_hills","Tfphi_dis","Tfphi_surf",	
       "Val_sl","Tfplw_dis","Tfplw_surf","Hi_plain","Lw_plain")

landform_full<-data.frame(cov=Class_full_leg, cat=Class_full_cod)

#----------Boosting for mean and ci
lilandform<-split(l_df$effectSize,l_df$Class)
lilandform<-list_drop_empty(lilandform)
#lilandform$`13`<-NULL

bt_landform<-as.data.frame(t(map_df(lilandform, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#----------create data frame for landform in AF
cat_landform<-data.frame(cat=bt_landform$Type) 
cat_landform$cat<-as.double(cat_landform$cat)

landform_jn<- right_join(as.data.frame(landform_full), 
            cat_landform, 
            by="cat") 

landform_bind<-bind_cols(landform_jn, bt_landform) %>%
     dplyr::rename(cov=1,cat=2)

# # Count the observations per each class
# n_all <- df %>% 
#   dplyr::summarise(numbers=n()) %>%
#   add_column(Type_Cat="overall") %>%
#   dplyr::select(2,1) %>%
#   as.data.frame()
# 
# df_count<- df %>% select(3,9:20)
# n_df<- as.data.frame(unlist(apply(df_count, 2 , table))) %>% 
#   rownames_to_column(var = "Type_Cat") %>%
#   dplyr::rename(numbers=2) 
# 
# count_data<-bind_rows(n_all,n_df) %>% 
#    filter(!numbers %in% c("1","2"), 
#           !Type_Cat %in% c(#"gdd_wheat_class.5",
#                            # "gdd_wheat_class.4",
#                             "gdd_rice_class.5",
#                             "gdd_rice_class.4",
#                            "gdd_soybean_class.5",
#                            "gdd_soybean_class.4",
#                            "gdd_soybean_class.2",
#                            "gdd_soybean_class.3"))
# 
# count_data

# convert to character
wrb_bind$cat<-as.character(wrb_bind$cat)
landform_bind$cat<-as.character(landform_bind$cat)
# Bind alls
df_all<-bind_rows(all,crop, aridity,
                  pH,soc,p,bd,tex,
                  gdd_maize,
                  gdd_wheat,
                  dem,slope,wrb_bind,landform_bind
                  )

# df_bind1<-bind_cols(df_all,count_data)

df_bind2 <- df_all %>% mutate(cat1 = case_when(

    (!cat %in% c("GDD_maize1","GDD_maize2","GDD_maize3","GDD_maize4","GDD_maize5",
                 "GDD_wheat1","GDD_wheat2","GDD_wheat3","GDD_wheat4","GDD_wheat5",
                 "GDD_rice1","GDD_rice2","GDD_rice3","GDD_rice4","GDD_rice5",
                 "GDD_soybean1","GDD_soybean2","GDD_soybean3","GDD_soybean4","GDD_soybean5"))~cat,
  
      (cat %in% c("GDD_maize1"))~"<0.8",
      (cat %in% c("GDD_maize2"))~"0.8-2.7",
      (cat %in% c("GDD_maize3"))~"2.7-4",
      (cat %in% c("GDD_maize4"))~"4-6",
      (cat %in% c("GDD_maize5"))~"6-10",
      (cat %in% c("GDD_wheat1"))~"<0.8",
      (cat %in% c("GDD_wheat2"))~"0.8-2.7",
      (cat %in% c("GDD_wheat3"))~"2.7-4",
      (cat %in% c("GDD_wheat4"))~"4-6",
      (cat %in% c("GDD_wheat5"))~"6-10",
      (cat %in% c("GDD_rice1"))~"<0.8",
      (cat %in% c("GDD_rice2"))~"0.8-2.7",
      (cat %in% c("GDD_rice3"))~"2.7-4",
      (cat %in% c("GDD_rice4"))~"4-6",
      (cat %in% c("GDD_rice5"))~"6-10",
      (cat %in% c("GDD_soybean1"))~"<0.8",
      (cat %in% c("GDD_soybean2"))~"0.8-2.7",
      (cat %in% c("GDD_soybean3"))~"2.7-4",
      (cat %in% c("GDD_soybean4"))~"4-6",
      (cat %in% c("GDD_soybean5"))~"6-10"
     ))

# df_bind3 <- df_bind2 %>% 
#           mutate(cat2 = paste0(cat1, " (", numbers, ")"))
# 
# df_bind3 %>% 
#   dplyr::summarise(across(everything(), ~ sum(is.na(.x))))

# df_bind<- df_bind2 %>% slice(1,3,4,5,7,2,6,8:nrow(df_all))
df_bind2$no <- 1:nrow(df_bind2)

# Plot with ggplot

#---Prepare Data

gdd<-df_bind2 %>% dplyr::filter(cov %in% c("GDD_maize", "GDD_rice", "GDD_soybean",
                               "GDD_wheat"))
gdd

undropped_rows <- df_bind2 %>% filter(!str_detect(cat, "GDD"))  %>% 
                                    arrange(desc(no))

final_df<-bind_rows(gdd,undropped_rows) 
final_df[,c(1,2)]

final_df1 <- final_df %>%
              slice(29:25,24:23,17,22,19:18,21:20,16,15:14,13:11,
                    10:1) %>%
              dplyr::mutate(ID = row_number()) %>%
              arrange(desc(ID))

final_df1

# save the data and plots
write_xlsx(df_bind2,"./output/mean_ci/OF_clim/OF_Arid_mean_ci.xlsx")
write_xlsx(final_df,"./output/mean_ci/OF_clim/arng_OF_Arid_mean_ci.xlsx")
write_xlsx(final_df1,"./output/mean_ci/OF_clim/arng_OF_Arid_mean_ci_sliced.xlsx")

# Plot with ggplot

tr_theme2<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.background = element_rect(fill='transparent',color=NA),
         legend.key=element_blank(),
         legend.box.background = element_rect(fill='transparent'),
         axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1),
         #axis.ticks = element_blank(),
         #axis.text = element_blank(),
         #axis.line = element_blank(),
         axis.title=element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
       )


fig1<- final_df1 %>%
  arrange(mean) %>%
  mutate(cat3 = factor(cat, levels= unique(final_df1$cat))) %>%
  ggplot( aes(x=cat3, y=mean)) +
  geom_point(fill= "#0C7BDC", color= "#0C7BDC", size=1) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
  geom_errorbar(aes(x = cat3, ymin = ci_low, ymax = ci_high),
                color= "#0C7BDC",width=0.1,size=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  # geom_vline(xintercept=c(1.5,3.5,5.5,8.5,10.5,11.5,12.5,
  #                         14.5,16.5,17.5,21.5,
  #                         39.5),linewidth=0.25)+
  scale_x_discrete(labels=final_df1$cat)+
  tr_theme2 +
  coord_flip()

fig1

# 
# # Save the figures
# ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
# ggsave(fig1, filename = "./output/graphs/OF_clim/arid/OF_Arid_figure.png", width = 10, height = 15, dpi = 300, units = "cm")
# ggsave(fig1, filename = "./output/graphs/OF_clim/arid/OF_Arid_figure.pdf", width = 10, height = 15, dpi = 300, units = "cm")
# ggsave(hist_nt, filename = "./output/graphs/hist/OF_Arid_hist.png", width = 7, height = 10, dpi = 300, units = "cm")
# 
# # save the data and plots
# write_xlsx(df_bind3,"./output/mean_ci/OF_clim/OF_Arid_mean_ci.xlsx")
# write_xlsx(final_df,"./output/mean_ci/OF_clim/arng_OF_Arid_mean_ci.xlsx")
# 
# # save the data and plots
# write_xlsx(df_bind1,"./output/mean_ci/NT_mean_ci.xlsx")
# 
# #Get covariate data
#   # df_mgt<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#   #                        filter(cat %in% c("overall","AF","CC","NT","OF"))
#   
#   df_crops<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                          filter(cat %in% c("overall","Maize","Rice","Soybean",
#                                            "Wheat","Cash crop","Cereal",
#                                            "V_F_others"))
#  
#   df_pH <- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                          filter(cat %in% c("acidic","neutral","alkaline"))
#  
#   df_soc<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                          filter(cat %in% c("<5","5-10",">10"))
#   
#   
#   df_p<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                          filter(cat %in% c("<10.9","10.9-21.4",">21.4"))
#   
#   df_bd<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                          filter(cat %in% c("<1.20","1.20-1.47",">1.47"))
#   
#   df_tex<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                          filter(cat %in% c("fine","medium","coarse"))
#   
#   df_dem<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                          filter(cat %in% c("<250","250-1000",">1000"))
#   
#   df_slope<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                          filter(cat %in% c("<0.20","0.2-1","1-5",
#                                            "5-15",">15"))
# df_clim<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                          filter(cat %in% c("Arid","Continental","Temperate",
#                                            "Tropical"))
# df_ar<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                          filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50",
#                                            "0.05-0.65",">0.65"))
# 
# crop
# 
# df_gdd_maize<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                       filter(cat %in% c("GDD_maize1","GDD_maize2","GDD_maize3",
#                                         "GDD_maize4","GDD_maize5"))
# 
# 
# df_gdd_rice<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                       filter(cat %in% c("GDD_rice1","GDD_rice2","GDD_rice3",
#                                         "GDD_rice4","GDD_rice5"))
# 
# df_gdd_soy<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                       filter(cat %in% c("GDD_soybean1","GDD_soybean2","GDD_soybean3",
#                                         "GDD_soybean4","GDD_soybean5"))
# 
# df_gdd_wheat<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
#                       filter(cat %in% c("GDD_wheat1","GDD_wheat2","GDD_wheat3",
#                                         "GDD_wheat4","GDD_wheat5"))
# 
# 
# # Building function to plot each covariate
# gg_plot <- function(data){
#   library(dplyr)
#   library(tidyverse)
#   library(ggplot2)
#   data %>% dplyr::arrange(data[,3]) %>%
#   mutate(cat = factor(data[,1], levels= unique(data[,1]))) %>%
#   ggplot(aes(x=cat,y= data[,3])) +
#   geom_point(fill= "#0C7BDC", color= "#0C7BDC", size=1) +
#   #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
#   geom_errorbar(aes(x = cat, ymin = data[,4], ymax = data[,5]),
#                 color= "#0C7BDC",width=0.1,size=0.5) +
#   geom_hline(yintercept = 0, linetype="dotted", 
#                 color = "red", linewidth=1)+
#   scale_y_continuous(limits = c(-0.3, 0.9))+
#   scale_x_discrete(labels=data[,2])+
#   tr_theme2 +
#   #facet_wrap(~cov)+
#   coord_flip()
# }
#  
# # Apply function to get the ggplot of each covariate
# # p0<-gg_plot(df_mgt)
# p1<-gg_plot(df_crops)+ theme(axis.text.x=element_blank())
# p2<-gg_plot(df_pH)+ theme(axis.text.x=element_blank())
# p3<-gg_plot(df_soc)+ theme(axis.text.x=element_blank())
# p4<-gg_plot(df_p)+ theme(axis.text.x=element_blank())
# p5<-gg_plot(df_bd)+ theme(axis.text.x=element_blank())
# p6<-gg_plot(df_tex)+ theme(axis.text.x=element_blank())
# p7<-gg_plot(df_dem)+ theme(axis.text.x=element_blank())
# p8<-gg_plot(df_slope)+ theme(axis.text.x=element_blank())
# 
# p9<-gg_plot(df_clim)+ theme(axis.text.x=element_blank())
# p10<-gg_plot(df_ar)+ theme(axis.text.x=element_blank())
# p11<-gg_plot(df_gdd_maize) + theme(axis.text.x=element_blank())
# p12<-gg_plot(df_gdd_rice) + theme(axis.text.x=element_blank())
# p13<-gg_plot(df_gdd_soy)+ theme(axis.text.x=element_blank())
# p14<-gg_plot(df_gdd_wheat)
# 
# #https://patchwork.data-imaginist.com/reference/wrap_plots.html
# # https://bioinformatics.ccr.cancer.gov/docs/data-visualization-with-r/Lesson6_V2/
# # s1<-p1 + force_panelsizes(rows = unit(4, "cm"),
# #                    cols = unit(7, "cm"))+
# #   
# # 
# # s2<-p2 + force_panelsizes(rows = unit(2, "cm"),
# #                    cols = unit(7, "cm")) 
# 
# a_nt<-p1 + p2+ p3 +p4+p5+p6+p7+
#   plot_layout(heights = c(0.39, 0.20,0.20,0.20,
#                           0.20,0.20,0.20,0.20,
#                           0.50,0.50,0.50),
#                           ncol =1)
# a_nt
#   
# b_nt<-p8+p9+p10+p11+p12+p13+p14+
#   plot_layout(heights = c(0.40,0.40, 0.40,0.40,
#                           0.50,0.50,0.50,0.50,
#                           0.50,0.50),
#                          ncol =1)
# 
# ggsave(a_nt,filename = "./output/graphs/NT_clim/overall/NT_all1.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# ggsave(b_nt,filename = "./output/graphs/NT_clim/overall/NT_all2.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# # Plot each covariate
# a_p1<-p1+
#   plot_layout(heights = c(0.39, 0.20,0.20,0.20,
#                           0.20,0.20,0.20,0.20,
#                           0.50,0.50,0.50),
#                           ncol =1)
# 
# a_p2<-p2+
#   plot_layout(heights = c(0.20,0.39, 0.20,0.20,
#                           0.20,0.20,0.20,0.20,
#                           0.50,0.50,0.50),
#                           ncol =1)
# 
# a_p3<-p3+
#   plot_layout(heights = c(0.20, 0.39,0.20,0.20,
#                           0.20,0.20,0.20,0.20,
#                           0.50,0.50,0.50),
#                           ncol =1)
# 
# a_p4<-p4+
#   plot_layout(heights = c(0.20, 0.39,0.20,0.20,
#                           0.20,0.20,0.20,0.20,
#                           0.50,0.50,0.50),
#                           ncol =1)
# 
# a_p5<-p5+
#   plot_layout(heights = c(0.20, 0.39,0.20,0.20,
#                           0.20,0.20,0.20,0.20,
#                           0.50,0.50,0.50),
#                           ncol =1)
# 
# a_p6<-p6+
#   plot_layout(heights = c(0.20, 0.39,0.20,0.20,
#                           0.20,0.20,0.20,0.20,
#                           0.50,0.50,0.50),
#                           ncol =1)
# 
# a_p7<-p7+
#   plot_layout(heights = c(0.20, 0.39,0.20,0.20,
#                           0.20,0.20,0.20,0.20,
#                           0.50,0.50,0.50),
#                           ncol =1)
# 
# a_p8<-p8+
#   plot_layout(heights = c(0.40,0.40, 0.40,0.40,
#                           0.50,0.50,0.50,0.50,
#                           0.50,0.50),
#                           ncol =1)
# 
# a_p9<-p9+
#   plot_layout(heights = c(0.40,0.40, 0.40,0.40,
#                           0.50,0.50,0.50,0.50,
#                           0.50,0.50),
#                           ncol =1)
# 
# a_p10<-p10+
#   plot_layout(heights = c(0.40,0.40, 0.40,0.40,
#                           0.50,0.50,0.50,0.50,
#                           0.50,0.50),
#                           ncol =1)
# 
# a_p11<-p11+
#   plot_layout(heights = c(0.40,0.40, 0.40,0.40,
#                           0.50,0.50,0.50,0.50,
#                           0.50,0.50),
#                           ncol =1)
# 
# a_p12<-p12+
#   plot_layout(heights = c(0.40,0.40, 0.40,0.40,
#                           0.50,0.50,0.50,0.50,
#                           0.50,0.50),
#                           ncol =1)
# 
# a_p13<-p13+
#   plot_layout(heights = c(0.40,0.40, 0.40,0.40,
#                           0.50,0.50,0.50,0.50,
#                           0.50,0.50),
#                           ncol =1)
# 
# a_p14<-p14+
#   plot_layout(heights = c(0.40,0.40, 0.40,0.40,
#                           0.50,0.50,0.50,0.50,
#                           0.50,0.50),
#                           ncol =1)
# 
# # Save each one of the covariates
# ggsave(a_p1,filename = "./output/graphs/NT_clim/overall/nt_a_p1.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p2,filename = "./output/graphs/NT_clim/overall/nt_a_p2.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p3,filename = "./output/graphs/NT_clim/overall/nt_a_p3.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p4,filename = "./output/graphs/NT_clim/overall/nt_a_p4.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p5,filename = "./output/graphs/NT_clim/overall/nt_a_p5.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p6,filename = "./output/graphs/NT_clim/overall/nt_a_p6.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p7,filename = "./output/graphs/NT_clim/overall/nt_a_p7.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p8,filename = "./output/graphs/NT_clim/overall/nt_a_p8.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p9,filename = "./output/graphs/NT_clim/overall/nt_a_p9.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p10,filename = "./output/graphs/NT_clim/overall/nt_a_p10.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p11,filename = "./output/graphs/NT_clim/overall/nt_a_p11.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p12,filename = "./output/graphs/NT_clim/overall/nt_a_p12.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p13,filename = "./output/graphs/NT_clim/overall/nt_a_p13.png",
#        width = 10, height = 19, dpi = 300, units = "cm")
# 
# ggsave(a_p14,filename = "./output/graphs/NT_clim/overall/nt_a_p14.png",
#        width = 10, height = 19, dpi = 300, units = "cm")




