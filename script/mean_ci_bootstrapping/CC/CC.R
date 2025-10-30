
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

# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# VIP: https://www.w3online.net/article/learn_method__plotting_the_confidence_intervals_using_plotci()_function_2

# df_joh<-read_xlsx("./Next_Article/input/all_dis_cov2.xlsx",guess_max = 1000) %>%
#     filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))%>%
#     dplyr::select(effectSize,key,Crop_Group)
# 
# write_xlsx(df_joh,"./Next_Article/input/dataset_Ozias.xlsx")

df<-read_xlsx("./Next_Article/input/all_dis_cov2.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))%>%
    dplyr::select(effectSize,key,Crop_Group,kg_clim,aridity_class,ph_class,soc_class,
                  p_class, bd_class, texture,gdd_maize_class, gdd_wheat_class,gdd_rice_class,
                  gdd_soybean_class, dem_class,slope_class ) %>%
                  drop_na(Crop_Group) 

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
ggplot(df, aes(x = effectSize)) + 
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

#---@ for the managements
li_mgt<-split(df$effectSize,df$key)

bt_mgt<-as.data.frame(t(map_df(li_mgt, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

bt_mgt
cat<-c("AF","CC","NT","OF")
cov<-c("mgt","mgt","mgt","mgt")
mgt<-bind_cols(cov,cat,bt_mgt) %>%
     dplyr::rename(cov=1,cat=2)
mgt

#---@ for the crop groups
li_crop<-split(df$effectSize,df$Crop_Group)

bt_crop<-as.data.frame(t(map_df(li_crop, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

cat<-c("Cash crop","Cereal","Maize","Rice","Soybean","Veg&Fruit and others","Wheat")
cov<-c("crop","crop","crop","crop","crop","crop","crop")
crop<-bind_cols(cov,cat,bt_crop) %>%
     dplyr::rename(cov=1,cat=2) %>%
     slice(3,4,5,7,1,2,6)
crop

#---@ for the kg_clim
li_kg_clim<-split(df$effectSize,df$kg_clim)

bt_kg_clim<-as.data.frame(t(map_df(li_kg_clim, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

cat<-c("Arid","Continental","Temperate","Tropical")
cov<-c("no_cov","no_cov","no_cov","no_cov")
clim<-bind_cols(cov,cat,bt_kg_clim) %>%
     dplyr::rename(cov=1,cat=2)
clim

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

cat<-c("<0.05","0.05-0.20","0.20-0.50","0.50-0.65",">0.65")
cov<-c("aridity index","aridity index","aridity index",
       "aridity index","aridity index")

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

cat<-c("acidic","neutral","alkaline")
cov<-c("pH","pH","pH")
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

cat<-c("<5","5-10",">10")
cov<-c("soc","soc","soc")
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

cat<-c("<10.9","10.9-21.4",">21.4")
cov<-c("phosphorus","phosphorus","phosphorus")
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

cat<-c("<1.20","1.20-1.47",">1.47")
cov<-c("bd","bd","bd")
bd<-bind_cols(cov,cat,bt_bd_class) %>%
     dplyr::rename(cov=1,cat=2)
bd

#---@ for the texture_class
li_texture_class<-split(df$effectSize,df$texture)

bt_texture_class<-as.data.frame(t(map_df(li_texture_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)


cat<-c("fine","medium","coarse")
cov<-c("texture","texture","texture")
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

cat<-c("GDD_maize1","GDD_maize2","GDD_maize3","GDD_maize4","GDD_maize5")
cov<-c("GDD_maize","GDD_maize","GDD_maize",
       "GDD_maize","GDD_maize")

gdd_maize<-bind_cols(cov,cat,bt_gdd_maize_class) %>%
     dplyr::rename(cov=1,cat=2) 

gdd_maize

#---@ for the gdd_wheat_class
li_gdd_wheat_class<-split(df$effectSize,df$gdd_wheat_class)

bt_gdd_wheat_class<-as.data.frame(t(map_df(li_gdd_wheat_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

cat<-c("GDD_wheat2","GDD_wheat3","GDD_wheat4","GDD_wheat5")
cov<-c("GDD_wheat","GDD_wheat",
       "GDD_wheat","GDD_wheat")
gdd_wheat<-bind_cols(cov,cat,bt_gdd_wheat_class) %>%
     dplyr::rename(cov=1,cat=2) 

gdd_wheat

#---@ for the gdd_rice_class
li_gdd_rice_class<-split(df$effectSize,df$gdd_rice_class)

bt_gdd_rice_class<-as.data.frame(t(map_df(li_gdd_rice_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

cat<-c("GDD_rice1","GDD_rice2","GDD_rice3","GDD_rice4","GDD_rice5")
cov<-c("GDD_rice","GDD_rice","GDD_rice",
       "GDD_rice","GDD_rice")

gdd_rice<-bind_cols(cov,cat,bt_gdd_rice_class) %>%
     dplyr::rename(cov=1,cat=2) 

gdd_rice

#---@ for the gdd_soybean_class
li_gdd_soybean_class<-split(df$effectSize,df$gdd_soybean_class)

bt_gdd_soybean_class<-as.data.frame(t(map_df(li_gdd_soybean_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

cat<-c("GDD_soybean1","GDD_soybean2","GDD_soybean3","GDD_soybean4","GDD_soybean5")
cov<-c("GDD_soybean","GDD_soybean","GDD_soybean",
       "GDD_soybean","GDD_soybean")

gdd_soybean<-bind_cols(cov,cat,bt_gdd_soybean_class) %>%
     dplyr::rename(cov=1,cat=2) 

gdd_soybean

#---@ for elevation
dem_mat <- c(0, 250, 1,
             250, 1000,2,
             1000, 4000,3)


#---@ for the dem_class
li_dem_class<-split(df$effectSize,df$dem_class)

df$dem_class

bt_dem_class<-as.data.frame(t(map_df(li_dem_class, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

cat<-c("<250","250-1000",">1000")
cov<-c("dem","dem","dem")
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

cat<-c("<0.20","0.2-1","1-5","5-15",">15")
cov<-c("slope_class","slope_class","slope_class",
       "slope_class","slope_class")

slope<-bind_cols(cov,cat,bt_slope_class) %>%
     dplyr::rename(cov=1,cat=2) 

slope


# Bind alls
df_all<-bind_rows(all,mgt, crop, soc,bd,p,tex,dem,slope,
                  clim, aridity,gdd_maize,gdd_wheat,
                  gdd_rice,gdd_soybean)

df_all

df_all %>% 
  summarise(across(everything(), ~ sum(is.na(.x))))

# save the data and plots
write_xlsx(df_all,"./Next_Article/input/results_1_data3.xlsx")

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

fig1<-df_all %>%
  arrange(mean) %>%
  mutate(cat = factor(cat, levels=c(
"GDD_soybean5",
"GDD_soybean4",
"GDD_soybean3",
"GDD_soybean2",
"GDD_soybean1",
"GDD_rice5",
"GDD_rice4",
"GDD_rice3",
"GDD_rice2",
"GDD_rice1",
"GDD_wheat5",
"GDD_wheat4",
"GDD_wheat3",
"GDD_wheat2",
"GDD_maize5",
"GDD_maize4",
"GDD_maize3",
"GDD_maize2",
"GDD_maize1",
">0.65",
"0.50-0.65",
"0.20-0.50",
"0.05-0.20",
"<0.05",
"Tropical",
"Temperate",
"Continental",
"Arid",
">15",
"5-15",
"1-5",
"0.2-1",
"<0.20",
">1000",
"250-1000",
"<250",
"coarse",
"medium",
"fine",
">21.4",
"10.9-21.4",
"<10.9",
">1.47",
"1.20-1.47",
"<1.20",
">10",
"5-10",
"<5",
"Veg&Fruit and others",
"Cereal",
"Cash crop",
"Wheat",
"Soybean",
"Rice",
"Maize",
"OF",
"NT",
"CC",
"AF",
"overall"))) %>%
  ggplot( aes(x=cat, y=mean)) +
  geom_point(fill= "#0C7BDC", color= "#0C7BDC", size=3) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
  geom_errorbar(aes(x = cat, ymin = ci_low, ymax = ci_high),
                color= "#0C7BDC",width=0.1,size=1.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", size=1)+
  geom_vline(xintercept=c(5.5,10.5,14.5,19.5,24.5,28.5,
                          33.5,36.5,39.5,42.5,45.5,48.5,
                          55.5),size=0.25)+
  tr_theme2 +
  coord_flip()


# Save the figures
ggsave(fig1, filename = "./Next_Article/output/graphs/figure2.png", width = 12, height = 10, dpi = 300, units = "cm")
