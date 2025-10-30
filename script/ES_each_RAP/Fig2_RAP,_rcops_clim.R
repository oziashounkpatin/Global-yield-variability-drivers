# New plots
# •	RAP
# •	Crops
# •	Aridity
# •	GDD – Maize
# •	GDD – Rice
# •	GDD – Soybean
# •	GDD – Wheat


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
                                           "5-15","15"))
df_clim<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("Arid","Continental","Temperate",
                                           "Tropical"))
df_ar<- final_df %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50",
                                           "0.05-0.65",">0.65"))

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
p2<-gg_plot1(df_clim)+ theme(axis.text.x=element_blank())
p3<-gg_plot1(df_ar)+ theme(axis.text.x=element_blank())

df_gdd_maize$label<-c("6-10","4-6","2.7-4","0.8-2.7","<0.8")
df_gdd_wheat$label<-c("6-10","4-6","2.7-4","0.8-2.7")
df_gdd_soy$label<-c("6-10","4-6","2.7-4","0.8-2.7","<0.8")
df_gdd_rice$label<-c("6-10","4-6","2.7-4","0.8-2.7","<0.8")

p11<-gg_plot2(df_gdd_maize) + theme(axis.text.x=element_blank())
p12<-gg_plot2(df_gdd_rice) + theme(axis.text.x=element_blank())
p13<-gg_plot2(df_gdd_soy)+ theme(axis.text.x=element_blank())
p14<-gg_plot2(df_gdd_wheat) + theme(axis.text.x=element_blank())
# save the data and plots
write_xlsx(df_all,"./output/mean_ci/overall_mean_ci.xlsx")

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




