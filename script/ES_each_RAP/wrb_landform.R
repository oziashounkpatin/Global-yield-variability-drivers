
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

df_wrb$Type<-NULL
df_landform$Type<-NULL

p15<-gg_plot1(df_wrb)
p15

p16<-gg_plot1(df_landform)
p16

p15 + p16

################################################################################
# WRB & LANDFORM-------------------------------------------------------------END
################################################################################