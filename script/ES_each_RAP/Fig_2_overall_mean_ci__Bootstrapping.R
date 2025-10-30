
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

# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# VIP: https://www.w3online.net/article/learn_method__plotting_the_confidence_intervals_using_plotci()_function_2

# df_joh<-read_xlsx("./input/data/all_dis_cov2.xlsx",guess_max = 1000) %>%
#     filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))%>%
#     dplyr::select(effectSize,key,Crop_Group)
# 
# write_xlsx(df_joh,"./input/data/dataset_Ozias.xlsx")


# Convert ES to perc
perc<- function(data){
  data_conv=100*(exp(data) - 1)
  return(data_conv)
}

df<-read_xlsx("./input/data/all_dis_cov2_last.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))%>%
    dplyr::select(effectSize,key,Crop_Group,kg_clim,aridity_class,ph_class,soc_class,
                  p_class, bd_class, texture,gdd_maize_class, gdd_wheat_class,gdd_rice_class,
                  gdd_soybean_class, dem_class,slope_class ) %>%
                  drop_na(Crop_Group) %>%
    dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others",
      "V_F_others",Crop_Group)) %>%
     mutate(ES=perc(effectSize)) %>%
     filter(!ES > 100) %>%
     select(!effectSize)  %>%
     dplyr::rename(effectSize=ES)

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
         axis.text = element_text(size = 10),
         #axis.ticks = element_blank(),
         #axis.text = element_blank(),
         #axis.line = element_blank(),
         #axis.title=element_blank()
         #panel.border = element_rect(colour = "black", fill=NA, size=5)
       )

# Plot with ggplot
tr_theme2<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.background = element_rect(fill='transparent',color=NA),
         legend.key=element_blank(),
         legend.box.background = element_rect(fill='transparent'),
         #axis.line = element_line(linewidth= 0.025, colour = "black", linetype=1),
         #axis.ticks = element_blank(),
         axis.text = element_text(size = 10),
         #axis.text = element_blank(),
         #axis.line = element_blank(),
         axis.title=element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=0.25)
       )

# draw plot 
hist_all<-ggplot(df, aes(x = effectSize)) + 
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

cat<-c("Cash crop","Maize","Other cereal","Rice","Soybean","V_F_others","Wheat")
cov<-c("crop","crop","crop","crop","crop","crop","crop")
crop<-bind_cols(cov,cat,bt_crop) %>%
     dplyr::rename(cov=1,cat=2) #%>%
     #slice(3,4,5,7,1,2,6)


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
cov<-c("GDD_rice","GDD_rice",
       "GDD_rice","GDD_rice","GDD_rice")

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


# Count the observations per each class
n_all <- df %>% 
  dplyr::summarise(numbers=n()) %>%
  add_column(Type_Cat="overall") %>%
  dplyr::select(2,1) %>%
  as.data.frame()

df_count<- df %>% select(2:16)
df_count

n_df<- as.data.frame(unlist(apply(df_count, 2 , table))) %>% 
  rownames_to_column(var = "Type_Cat") %>%
  dplyr::rename(numbers=2) 

count_data<-bind_rows(n_all,n_df) %>% 
   filter(!numbers %in% c("1","2"))
          # !Type_Cat %in% c(#"gdd_wheat_class.5",
          #                  # "gdd_wheat_class.4",
          #                   "gdd_rice_class.5",
          #                   "gdd_rice_class.4",
          #                  "gdd_soybean_class.5",
          #                  "gdd_soybean_class.4",
          #                  "gdd_soybean_class.2",
          #                  "gdd_soybean_class.3"))

# Bind alls

df_all<-bind_rows(all,mgt,crop,clim,aridity,
                  pH,soc,p,bd,tex,
                  gdd_maize,gdd_wheat,
                  gdd_rice,gdd_soybean,
                  dem,slope
                  )

dim(count_data)
dim(df_all)

crop
count_data
head(df_all)

#df_bind1<-bind_cols(df_all,count_data)

# head(count_data)
# 
# head(df_bind1)

df_bind2 <- df_all %>% mutate(cat1 = case_when(

    (!cat %in% c("GDD_maize1","GDD_maize2","GDD_maize3","GDD_maize4","GDD_maize5",
                 "GDD_wheat1","GDD_wheat2","GDD_wheat3","GDD_wheat4","GDD_wheat5",
                 "GDD_rice1","GDD_rice2","GDD_rice3","GDD_rice4","GDD_rice5",
                 "GDD_soybean1","GDD_soybean2","GDD_soybean3","GDD_soybean4","GDD_soybean5"))~cat,
    
    # GDD
# gdd_mat <- c(0,   800, 1,
#             800, 2700,  2,
#             2700, 4000,  3,
#             4000, 6000, 4,
#             6000, 10000, 5)
  
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

df_bind2

# Plot with ggplot

#---Prepare Data

gdd<-df_bind2 %>% dplyr::filter(cov %in% c("GDD_maize", "GDD_rice", "GDD_soybean",
                               "GDD_wheat"))  

undropped_rows <- df_bind2 %>% filter(!str_detect(cat, "GDD"))  %>% 
                                    arrange(desc(no))

final_df1<-bind_rows(gdd,undropped_rows) 
final_df1[,c(1,2)]

final_df <- final_df1 %>%
              slice(63,
                    62:59,
                    56,55,54,52,58,57,53,
                    42:20,
                    51:48,
                    47:43,
                    1:5,10:19,6:9
                    ) %>%
              dplyr::mutate(ID = row_number()) %>%
              arrange(desc(ID))

min(final_df$mean);max(final_df$mean)
min(final_df$mean);max(final_df$mean)

fig1<- final_df %>%
  arrange(mean) %>%
  mutate(cat3 = factor(cat, levels= unique(final_df$cat))) %>%
  ggplot(aes(x=cat3, y=mean)) +
  geom_point(fill= "#0C7BDC", color= "#0C7BDC", size=1) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
  geom_errorbar(aes(x = cat3, ymin = ci_low, ymax = ci_high),
                color= "#0C7BDC",width=0.1,size=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  geom_vline(xintercept=c(4.5, 9.5,14.5,19.5,24.5,28.5,
                          33.5,36.5,39.5,42.5,45.5,48.5,51.5,
                          58.5),linewidth=0.25)+
  scale_x_discrete(labels=final_df$cat)+
  tr_theme2 +
  #facet_wrap(~cov)+
  coord_flip()
fig1


# Get covariate data
  df_mgt<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("overall","AF","CC","NT","OF"))
  
  df_crops<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("Maize","Rice","Soybean",
                                           "Wheat","Cash crop","Other cereal",
                                           "V_F_others"))  %>%
                         slice(1,3,7,4,5,6,2)
 
  df_pH <- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("acidic","neutral","alkaline"))
 
  df_soc<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<5","5-10",">10"))
  
  
  df_p<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<10.9","10.9-21.4",">21.4"))
  
  df_bd<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<1.20","1.20-1.47",">1.47"))
  
  df_tex<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("fine","medium","coarse"))
  
  df_dem<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<250","250-1000",">1000"))
  
  df_slope<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.20","0.2-1","1-5",
                                           "5-15",">15"))
df_clim<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("Arid","Continental","Temperate",
                                           "Tropical"))
df_ar<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50",
                                           "0.50-0.65",">0.65"))

df_gdd_maize<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_maize1","GDD_maize2","GDD_maize3",
                                        "GDD_maize4","GDD_maize5"))

df_gdd_maize$cat1<-c("6-10","4-6","2.7-4","0.8-2.7","<0.8")


df_gdd_rice<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_rice1","GDD_rice2","GDD_rice3",
                                        "GDD_rice4","GDD_rice5"))
df_gdd_rice$cat1<-c("6-10","4-6","2.7-4","0.8-2.7","<0.8")

df_gdd_soy<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_soybean1","GDD_soybean2","GDD_soybean3",
                                        "GDD_soybean4","GDD_soybean5"))
df_gdd_soy$cat1<-c("6-10","4-6","2.7-4","0.8-2.7","<0.8")

df_gdd_wheat<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_wheat1","GDD_wheat2","GDD_wheat3",
                                        "GDD_wheat4","GDD_wheat5"))
df_gdd_wheat$cat1<-c("6-10","4-6","2.7-4","0.8-2.7")

min(final_df$mean);max(final_df$mean)
min(final_df$ci_low);max(final_df$ci_low)
min(final_df$ci_high);max(final_df$ci_high)



# Building function to plot each covariate
gg_plot1 <- function(data){
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  data %>% dplyr::arrange(data[,2]) %>%
  mutate(cat = factor(data[,1], levels= unique(data[,1]))) %>%
  ggplot(aes(x=cat,y= data[,2])) +
  geom_point(fill= "#000000", color= "#000000", size=1) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
  geom_errorbar(aes(x = cat, ymin = data[,3], ymax = data[,4]),
                color= "#000000",width=0.1,size=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  scale_y_continuous(limits = c(-25, 30))+
  scale_x_discrete(labels=data[,1])+
  tr_theme2 +
  #facet_wrap(~cov)+
  coord_flip()
}

gg_plot2 <- function(data){
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  data %>% dplyr::arrange(data[,2]) %>%
  mutate(cat = factor(data[,1], levels= unique(data[,1]))) %>%
  ggplot(aes(x=cat,y= data[,2])) +
  geom_point(fill= "#000000", color= "#000000", size=1) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
  geom_errorbar(aes(x = cat, ymin = data[,3], ymax = data[,4]),
                color= "#000000",width=0.1,size=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  scale_y_continuous(limits = c(-25, 30))+
  scale_x_discrete(labels=data[,5])+
  tr_theme2 +
  #facet_wrap(~cov)+
  coord_flip()
}

# Apply function to get the ggplot of each covariate
p0<-gg_plot1(df_mgt)+theme(axis.text.x=element_blank())
p1<-gg_plot1(df_crops)+ theme(axis.text.x=element_blank())
p2<-gg_plot1(df_pH)+ theme(axis.text.x=element_blank())
p3<-gg_plot1(df_soc)+ theme(axis.text.x=element_blank())
p4<-gg_plot1(df_p)+ theme(axis.text.x=element_blank())
p5<-gg_plot1(df_bd)+ theme(axis.text.x=element_blank())
p6<-gg_plot1(df_tex)+ theme(axis.text.x=element_blank())
p7<-gg_plot1(df_dem) + theme(axis.text.x=element_blank())
p8<-gg_plot1(df_slope) + theme(axis.text.x=element_blank())

p9<-gg_plot1(df_clim)+ theme(axis.text.x=element_blank())
p10<-gg_plot1(df_ar)+ theme(axis.text.x=element_blank())

df_gdd_maize$label<-c("6-10","4-6","2.7-4","0.8-2.7","<0.8")
df_gdd_wheat$label<-c("6-10","4-6","2.7-4","0.8-2.7")
df_gdd_soy$label<-c("6-10","4-6","2.7-4","0.8-2.7","<0.8")
df_gdd_rice$label<-c("6-10","4-6","2.7-4","0.8-2.7","<0.8")

p11<-gg_plot2(df_gdd_maize) + theme(axis.text.x=element_blank())
p12<-gg_plot2(df_gdd_rice) + theme(axis.text.x=element_blank())
p13<-gg_plot2(df_gdd_soy)+ theme(axis.text.x=element_blank())
p14<-gg_plot2(df_gdd_wheat) + theme(axis.text.x=element_blank())

df_mgt$Component<-NULL

# save the data and plots
write_xlsx(df_all,"./output/mean_ci/overall_mean_ci.xlsx")

# # Save the figures
# ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
# ggsave(fig1, filename = "./output/graphs/all/Overall_figure.pdf", width = 10, height = 20, dpi = 300, units = "cm")
 #ggsave(hist_all, filename = "./output/graphs/hist/Overall_hist.png", width = 8, height = 10, dpi = 300, units = "cm")

# # save for each covariate
# showtext_auto()
# showtext_opts(dpi = 300)
# ggsave(s1, filename = "./output/graphs/all/1_crops.png", width = 7, height = 4, dpi = 300, units = "cm")
# ggsave(s2, filename = "./output/graphs/all/2_ph.png", width = 5, height = 4, dpi = 300, units = "cm")

#https://patchwork.data-imaginist.com/reference/wrap_plots.html
# https://bioinformatics.ccr.cancer.gov/docs/data-visualization-with-r/Lesson6_V2/
# s1<-p1 + force_panelsizes(rows = unit(4, "cm"),
#                    cols = unit(7, "cm"))
# s2<-p2 + force_panelsizes(rows = unit(2, "cm"),
#                    cols = unit(7, "cm")) 


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
library(terra)

# Convert ES to perc
perc<- function(data){
  data_conv=100*(exp(data) - 1)
  return(data_conv)
}

################################################################################
# WRB & LANDFORM
################################################################################

df<-read_xlsx("./input/data/all_dis_cov3.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))%>%
    dplyr::select(effectSize,key,Crop_Group,wrb,landform) %>%
                  drop_na(Crop_Group) %>%
    dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others",
      "V_F_others",Crop_Group))%>%
     mutate(ES=perc(effectSize)) %>%
     filter(!ES > 100) %>%
     select(!effectSize)  %>%
     dplyr::rename(effectSize=ES)

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
         axis.text = element_text(size = 10)
         #axis.ticks = element_blank(),
         #axis.text = element_blank(),
         #axis.line = element_blank(),
         #axis.title=element_blank()
         #panel.border = element_rect(colour = "black", fill=NA, size=5)
       )


tr_theme2<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.background = element_rect(fill='transparent',color=NA),
         legend.key=element_blank(),
         legend.box.background = element_rect(fill='transparent'),
         #axis.line = element_line(linewidth= 0.025, colour = "black", linetype=1),
         #axis.ticks = element_blank(),
         axis.text = element_text(size = 10),
         #axis.text = element_blank(),
         #axis.line = element_blank(),
         axis.title=element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=0.25)
       )

# draw plot 
hist_all<-ggplot(df, aes(x = effectSize)) + 
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

df$wrb<-as.factor(df$wrb)
df_count1<- df %>% select(4)

# Count the observations per each class
n_df1<- as.data.frame(unlist(apply(df_count1, 2 , table))) %>% 
  rownames_to_column(var = "Type_Cat") %>%
  dplyr::rename(numbers=2) 

n_df1$Type_Cat<-as.numeric(as.character(n_df1$Type_Cat)) 
n_df1<-n_df1 %>% arrange()

n_wrb <- n_df1 %>% 
          mutate(cat2 = paste0(Type_Cat, " (", numbers, ")")) %>%
          arrange(Type_Cat)


#---@ for the wrb
li_wrb<-split(df$effectSize,df$wrb)

bt_wrb<-as.data.frame(t(map_df(li_wrb, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

bt_wrb
cat<-c(0,1,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,20,22,23,24,26,29)
cov<-c("Acrisols","Albeluvisols","Alisols","Andosols","Arenosols",
       "Calcisols","Cambisols","Chernozems","Ferralsols","Fluvisols",	
       "Gleysols","Gypsisols","Histosols","Kastanozems","Leptosols",
       "Lixisols","Luvisols","Phaeozems","Plinthosols","Podzols",
       "Regosols","Solonetz","Vertisols")

df_wrb<-bind_cols(cov,cat,bt_wrb) %>%
     dplyr::rename(cov=1,cat=2)

df_wrb_bnd<-bind_cols(df_wrb,n_wrb[,3]) %>% rename(cat2=7)
final_wrb<- df_wrb_bnd %>% select(cat,cat2,mean,ci_low,ci_high)

#---@ for the landform

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

l_df$Class<-as.factor(l_df$Class)

unique(l_df$Class)

l_df_count1<- l_df %>% select(Class)

# Count the observations per each class
n_l_df1<- as.data.frame(unlist(apply(l_df_count1, 2 , table))) %>% 
  rownames_to_column(var = "Type_Cat") %>%
  dplyr::rename(numbers=2) 

n_l_df1$Type_Cat<-as.numeric(as.character(n_l_df1$Type_Cat)) 
n_l_df1<-n_l_df1 %>% arrange()

n_Class <- n_l_df1 %>% 
          mutate(cat2 = paste0(Type_Cat, " (", numbers, ")")) %>%
          arrange(Type_Cat)

  
l_df$landform<-as.factor(l_df$Class)

li_landform<-split(l_df$effectSize,l_df$Class)

head(li_landform)


#l_df %>% count(Class)

bt_landform<-as.data.frame(t(map_df(li_landform, es_boot))) %>%
   rownames_to_column(var = "Type") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4) %>%
   slice(1:24)

cat<-c(1,2,3,4,5,6,7,8,9,11,13,15,17,19,21)
cov<-c("Mtn_sumt","Cliff_sl","Lwhi_mtn","Shills_dcsl","Lhgsl_steep",
       "Lhgsl_mod","Mtn_vs","Mod_hills","Tfphi_dis","Tfphi_surf",	
       "Val_sl","Tfplw_dis","Tfplw_surf","Hi_plain","Lw_plain")

df_landform<-bind_cols(cov,cat,bt_landform) %>%
     dplyr::rename(cov=1,cat=2)

df_wrb_landf<-bind_rows(df_wrb,df_landform)
# Save data
write_xlsx(df_wrb_landf, "./output/table/overall_wrb_ldf.xlsx")

# Building function to plot each covariate
gg_plot1 <- function(data){
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  data %>% dplyr::arrange(data[,3]) %>%
  mutate(cat = factor(data[,1], levels= unique(data[,1]))) %>%
  ggplot(aes(x=cat,y= data[,3])) +
  geom_point(fill= "#000000", color= "#000000", size=1) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
  geom_errorbar(aes(x = cat, ymin = data[,4], ymax = data[,5]),
                color= "#000000",width=0.1,linewidth=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  scale_y_continuous(limits = c(-25, 30))+
  scale_x_discrete(labels=rev(data[,1]))+
  tr_theme2 +
  #facet_wrap(~cov)+
  coord_flip()
}

# Building function to plot each covariate
gg_plot_wrb <- function(data){
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  data %>% dplyr::arrange(data[,3]) %>%
  mutate(cat = factor(data[,1], levels= sort(unique(data[,1]), decreasing=T))) %>%
  ggplot(aes(x=cat,y= data[,3])) +
  geom_point(fill= "#000000", color= "#000000", size=1) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
  geom_errorbar(aes(x = cat, ymin = data[,4], ymax = data[,5]),
                color= "#000000",width=0.1,linewidth=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  scale_y_continuous(limits = c(-25, 30))+
  scale_x_discrete(labels=rev(data[,1]))+
  tr_theme2 +
  #facet_wrap(~cov)+
  coord_flip()
}

# Building function to plot each covariate
gg_plot_lf <- function(data){
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  data %>% dplyr::arrange(data[,3]) %>%
  mutate(cat = factor(data[,1], levels= unique(data[,1]))) %>%
  ggplot(aes(x=cat,y= data[,3])) +
  geom_point(fill= "#000000", color= "#000000", size=1) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
  geom_errorbar(aes(x = cat, ymin = data[,4], ymax = data[,5]),
                color= "#000000",width=0.1,linewidth=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  scale_y_continuous(limits = c(-25, 30))+
  scale_x_discrete(labels=data[,1])+
  tr_theme2 +
  #facet_wrap(~cov)+
  coord_flip()
}

df_wrb$Type<-NULL
df_landform$Type<-NULL

p15<-gg_plot_wrb(df_wrb)
p15

p16<-gg_plot_lf(df_landform)
p16

p15 + p16

# Merge all data
df_mgt1 <- df_mgt %>% mutate(component = "Management", variable="RAP", 
                             order=c(5,4,3,2,1))  %>% 
                     select(component,variable, everything())

df_crops1 <- df_crops %>% mutate(component = "Crops", variable="Crops", 
                                 order=c(7,6,5,4,3,2,1))  %>% 
                     select(component,variable, everything())

df_clim1 <- df_clim %>% mutate(component = "Climate",variable="Climatic regions",
                               order=c(4,3,2,1))  %>% 
                     select(component,variable, everything())

df_ar1 <- df_ar %>% mutate(component = "Climate",variable="Aridity", 
                           order=c(5,4,3,2,1))  %>% 
                     select(component,variable, everything())

df_gdd_maize1<- df_gdd_maize %>% mutate(component = "Climate",variable="GDD Maize",order=c(5,4,3,2,1))  %>% 
                     select(component,variable, everything()) %>% 
                     select(component,variable,cat1,mean,ci_low,ci_high,order)%>%
                     rename(cat=cat1)

df_gdd_rice1<- df_gdd_rice %>% mutate(component = "Climate",variable="GDD Rice",order=c(5,4,3,2,1))  %>% 
                     select(component,variable, everything())%>% 
                     select(component,variable,cat1,mean,ci_low,ci_high,order)%>%
                     rename(cat=cat1)

df_gdd_soy1<- df_gdd_soy %>% mutate(component = "Climate",variable="GDD Soybean",order=c(5,4,3,2,1))  %>% 
                     select(component,variable, everything())%>% 
                     select(component,variable,cat1,mean,ci_low,ci_high,order)%>%
                     rename(cat=cat1)

df_gdd_wheat1<- df_gdd_wheat %>% mutate(component = "Climate",variable="GDD Wheat",order=c(5,4,3,2))  %>% 
                     select(component,variable, everything())%>% 
                     select(component,variable,cat1,mean,ci_low,ci_high,order)%>%
                     rename(cat=cat1)

df_SOC1<- df_soc %>% mutate(component = "Soil properties",variable="SOC",order=c(3,2,1))  %>% 
                     select(component,variable, everything())

df_p1<- df_p %>% mutate(component = "Soil properties",variable="Phosphorus",order=c(3,2,1))  %>% 
                     select(component,variable, everything())

df_pH1<- df_pH %>% mutate(component = "Soil properties",variable="pH",order=c(3,2,1))  %>% 
                     select(component,variable, everything())

df_bd1<- df_bd %>% mutate(component = "Soil properties",variable="BD",order=c(3,2,1))  %>%
                     select(component,variable, everything())

df_tex1<- df_tex %>% mutate(component = "Soil properties",variable="Texture",order=c(3,2,1))  %>%
                     select(component,variable, everything())

df_wrb11<-df_wrb %>% select(cov,mean,ci_low,ci_high) %>% rename(cat=cov)
df_wrb1<- df_wrb11 %>% mutate(component = "Soil properties",variable="Soil type",
                              order=c(0,1,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,20,22,23,24,26,29))  %>%
                     select(component,variable, everything())

df_dem1<- df_dem %>% mutate(component = "Topography",variable="Elevation",order=c(3,2,1))  %>%
                     select(component,variable, everything())

df_slope1<- df_slope %>% mutate(component = "Topography",variable="Slope",order=c(5,4,3,2,1))  %>%
                     select(component,variable, everything())

df_landform11<-df_landform %>% select(cov,mean,ci_low,ci_high) %>% rename(cat=cov)
df_landform1<- df_landform11 %>% mutate(component = "Topography",variable="Landform",
                            order=c(1,2,3,4,5,6,7,8,9,11,13,15,17,19,21))  %>%
                     select(component,variable, everything())


# Bind all
mega_df<-bind_rows(df_mgt1,df_crops1,df_clim1,df_ar1,df_gdd_maize1,df_gdd_rice1,
                   df_gdd_soy1,df_gdd_wheat1,df_p1,df_pH1,df_SOC1,df_bd1,df_tex1,df_wrb1,
                   df_dem1,df_slope1,df_landform1) %>%
                    rename(level=cat,est=mean,lwr=ci_low,upr=ci_high)

head(mega_df)
# =========================================================
# 1) Data template (REPLACE with your real estimates)
#    Columns:
#      component  = "Climate" | "Soil" | "Topography"
#      variable   = variable heading within a component (e.g., "Aridity", "GDD – Maize")
#      level      = category label shown on y-axis
#      est        = point estimate (% change)
#      lwr, upr   = lower & upper CI bounds (% change)
#      order      = integer to control the order of levels (top→bottom) within each variable
# =========================================================


write_xlsx(mega_df,"C:/Users/hounkpk1/RAP_Drivers/output/Figure_data/Figure2.xlsx")
# New plots
# •	RAP
# •	Crops
# •	Aridity
# •	GDD – Maize
# •	GDD – Rice
# •	GDD – Soybean
# •	GDD – Wheat

################################################################################
# WRB & LANDFORM-------------------------------------------------------------END
################################################################################

a<-p0+p1+p2+p3+p4+p5+p6+p7+p15+
  plot_layout(heights = c(0.90, 1.5,0.70,0.70,
                          0.70,0.70,0.70,0.70,5),
                          ncol =1)
a

b<-p8+p9+p10+p11+p12+p13+p14+p16+
  plot_layout(heights = c(0.70,0.70, 0.70,0.80,
                          0.80,0.80,0.70,4),ncol =1)
b

#------------------------------------------------------------------
a<-p0+p1+p11+p12+p13+p14+p9+p10+p15+
  plot_layout(heights = c(1, 1.5,1,1,
                          1,1,1,1.5,5),
                          ncol =1)
a

b<- p2+p3+p4+p5+p6+p7+p8+p16+
  plot_layout(heights = c(0.70,0.70, 0.70,0.80,
                          0.80,0.80,0.70,4),ncol =1)
b

ggsave(a,filename = "./output/graphs/Fig_2_a.png",width = 10, height = 30, dpi = 450, units = "cm")
ggsave(b,filename = "./output/graphs/Fig_2_b.png",width = 10, height = 30, dpi = 450, units = "cm")

ggsave(a,filename = "./output/graphs/Fig_2_a.pdf",width = 10, height = 30, dpi = 450, units = "cm")
ggsave(b,filename = "./output/graphs/Fig_2_b.pdf",width = 10, height = 30, dpi = 450, units = "cm")



