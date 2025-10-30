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

# Convert ES to perc
perc<- function(data){
  data_conv=100*(exp(data) - 1)
  return(data_conv)
}


# Load data for wrb and 
df<-read_xlsx("./input/data/all_dis_cov3.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))%>%
    dplyr::select(effectSize,key,Crop_Group,wrb,landform) %>%
                  drop_na(Crop_Group) %>%
    dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others",
      "V_F_others",Crop_Group)) %>%
     mutate(ES=perc(effectSize)) %>%
     filter(!ES > 100) %>%
     select(!effectSize)  %>%
     dplyr::rename(effectSize=ES)

# Turn to factor
df$wrb<-as.factor(df$wrb)

# data per management
df_af<- df %>% filter(key %in% c("AF"))
df_cc<- df %>% filter(key %in% c("CC"))
df_nt<- df %>% filter(key %in% c("NT"))
df_of<- df %>% filter(key %in% c("OF"))

wrb_full_cod<-c(0,1,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,20,22,23,24,26,29)
wrb_full_leg<-c("Acrisols","Albeluvisols","Alisols","Andosols","Arenosols",
       "Calcisols","Cambisols","Chernozems","Ferralsols","Fluvisols",	
       "Gleysols","Gypsisols","Histosols","Kastanozems","Leptosols",
       "Lixisols","Luvisols","Phaeozems","Plinthosols","Podzols",
       "Regosols","Solonetz","Vertisols")

wrb_full<-data.frame(cov=wrb_full_leg, cat=wrb_full_cod)

################################################################################
#---@ FOR THE WRB FOR THE WRB  FOR THE WRB  FOR THE WRB  FOR THE WRB  FOR THE WRB 
################################################################################


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@-----AF

#----------Boosting for mean and ci
af_li_wrb<-split(df_af$effectSize,df_af$wrb)
af_li_wrb<-list_drop_empty(af_li_wrb)

af_bt_wrb<-as.data.frame(t(map_df(af_li_wrb, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#----------create data frame for wrb in AF
af_cat_wrb<-data.frame(cat=af_bt_wrb$Type) 
af_cat_wrb$cat<-as.double(af_cat_wrb$cat)

af_wrb_jn<- right_join(as.data.frame(wrb_full), 
            af_cat_wrb, 
            by="cat") 

af_wrb_bind<-bind_cols(af_wrb_jn, af_bt_wrb) %>%
     dplyr::rename(cov=1,cat=2)

#----------create data frame for remaining wrb not in AF
afleg_wrd_dif<-setdiff(wrb_full_leg, af_wrb_jn$cov)
afcod_wrd_dif<-setdiff(wrb_full_cod,  af_wrb_jn$cat)

#----------merge data for wrb in AF and not in AF
af_wrb_diff<-data.frame(cov=afleg_wrd_dif,
                         cat=afcod_wrd_dif, 
                         type=afcod_wrd_dif,
                         mean=NA,
                         ci_low=NA,
                         ci_high=NA)
  
df_af_wrb<-bind_rows(af_wrb_bind,af_wrb_diff) %>% 
                        arrange(cat)  %>% 
                        select(cov, cat, mean, ci_low, ci_high)

gg_plot(df_af_wrb) + scale_x_discrete(labels=rev(df_af_wrb[,1]))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@-----CC

#----------Boosting for mean and ci
cc_li_wrb<-split(df_cc$effectSize,df_cc$wrb)
cc_li_wrb<-list_drop_empty(cc_li_wrb)

cc_bt_wrb<-as.data.frame(t(map_df(cc_li_wrb, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#----------create data frame for wrb in CC
cc_cat_wrb<-data.frame(cat=cc_bt_wrb$Type) 
cc_cat_wrb$cat<-as.double(cc_cat_wrb$cat)

cc_wrb_jn<- right_join(as.data.frame(wrb_full), 
            cc_cat_wrb, 
            by="cat") 

cc_wrb_bind<-bind_cols(cc_wrb_jn, cc_bt_wrb) %>%
     dplyr::rename(cov=1,cat=2)

#----------create data frame for remaining wrb not in CC
ccleg_wrd_dif<-setdiff(wrb_full_leg, cc_wrb_jn$cov)
cccod_wrd_dif<-setdiff(wrb_full_cod,  cc_wrb_jn$cat)

#----------merge data for wrb in CC and not in CC
cc_wrb_diff<-data.frame(cov=ccleg_wrd_dif,
                         cat=cccod_wrd_dif, 
                         type=cccod_wrd_dif,
                         mean=NA,
                         ci_low=NA,
                         ci_high=NA)
  
df_cc_wrb<-bind_rows(cc_wrb_bind,cc_wrb_diff) %>% 
                        arrange(cat)  %>% 
                        select(cov, cat, mean, ci_low, ci_high)

gg_plot(df_cc_wrb) + scale_x_discrete(labels=rev(df_cc_wrb[,1]))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@-----NT

#----------Boosting for mean and ci
nt_li_wrb<-split(df_nt$effectSize,df_nt$wrb)
nt_li_wrb<-list_drop_empty(nt_li_wrb)

nt_bt_wrb<-as.data.frame(t(map_df(nt_li_wrb, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#----------create data frame for wrb in NT
nt_cat_wrb<-data.frame(cat=nt_bt_wrb$Type) 
nt_cat_wrb$cat<-as.double(nt_cat_wrb$cat)

nt_wrb_jn<- right_join(as.data.frame(wrb_full), 
            nt_cat_wrb, 
            by="cat") 

nt_wrb_bind<-bind_cols(nt_wrb_jn, nt_bt_wrb) %>%
     dplyr::rename(cov=1,cat=2)

#----------create data frame for remaining wrb not in NT
ntleg_wrd_dif<-setdiff(wrb_full_leg, nt_wrb_jn$cov)
ntcod_wrd_dif<-setdiff(wrb_full_cod,  nt_wrb_jn$cat)

#----------merge data for wrb in NT and not in NT
nt_wrb_diff<-data.frame(cov=ntleg_wrd_dif,
                         cat=ntcod_wrd_dif, 
                         type=ntcod_wrd_dif,
                         mean=NA,(ntcod_wrd_dif),
                         ci_low=NA,(ntcod_wrd_dif),
                         ci_high=NA,(ntcod_wrd_dif))
  
df_nt_wrb<-bind_rows(nt_wrb_bind,nt_wrb_diff) %>% 
                        arrange(cat)  %>% 
                        select(cov, cat, mean, ci_low, ci_high)

gg_plot(df_nt_wrb) + scale_x_discrete(labels=rev(df_nt_wrb[,1]))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@-----OF

#----------Boosting for mean and ci
of_li_wrb<-split(df_of$effectSize,df_of$wrb)
of_li_wrb<-list_drop_empty(of_li_wrb)

of_bt_wrb<-as.data.frame(t(map_df(of_li_wrb, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#----------create data frame for wrb in OF
of_cat_wrb<-data.frame(cat=of_bt_wrb$Type) 
of_cat_wrb$cat<-as.double(of_cat_wrb$cat)

of_wrb_jn<- right_join(as.data.frame(wrb_full), 
            of_cat_wrb, 
            by="cat") 

of_wrb_bind<-bind_cols(of_wrb_jn, of_bt_wrb) %>%
     dplyr::rename(cov=1,cat=2)

#----------create data frame for remaining wrb not in OF
ofleg_wrd_dif<-setdiff(wrb_full_leg, of_wrb_jn$cov)
ofcod_wrd_dif<-setdiff(wrb_full_cod,  of_wrb_jn$cat)

#----------merge data for wrb in OF and not in OF
of_wrb_diff<-data.frame(cov=ofleg_wrd_dif,
                         cat=ofcod_wrd_dif, 
                         type=ofcod_wrd_dif,
                         mean=NA,(ofcod_wrd_dif),
                         ci_low=NA,(ofcod_wrd_dif),
                         ci_high=NA,(ofcod_wrd_dif))
  
df_of_wrb<-bind_rows(of_wrb_bind,of_wrb_diff) %>% 
                        arrange(cat)  %>% 
                        select(cov, cat, mean, ci_low, ci_high)

gg_plot(df_of_wrb) + scale_x_discrete(labels=rev(df_of_wrb[,1]))

################################################################################
#---@ FOR THE LANDFORMFOR THE LANDFORM FOR THE LANDFORM FOR THE LANDFORM 
################################################################################
library(terra)
rast("C:/Users/hounkpk1/OneDrive - Aalto University/Feature_NT/land_form.tif")

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

# data per management
l_df_af<- l_df %>% filter(key %in% c("AF"))
l_df_cc<- l_df %>% filter(key %in% c("CC"))
l_df_nt<- l_df %>% filter(key %in% c("NT"))
l_df_of<- l_df %>% filter(key %in% c("OF"))

Class_full_cod<-c(1,2,3,4,5,6,7,8,9,11,13,15,17,19,21)
Class_full_leg<-c("Mtn_sumt","Cliff_sl","Lwhi_mtn","Shills_dcsl","Lhgsl_steep",
       "Lhgsl_mod","Mtn_vs","Mod_hills","Tfphi_dis","Tfphi_surf",	
       "Val_sl","Tfplw_dis","Tfplw_surf","Hi_plain","Lw_plain")

Class_full<-data.frame(cov=Class_full_leg, cat=Class_full_cod)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@-----AF

#----------Boosting for mean and ci
af_li_Class<-split(l_df_af$effectSize,l_df_af$Class)
af_li_Class<-list_drop_empty(af_li_Class)

af_bt_Class<-as.data.frame(t(map_df(af_li_Class, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#----------create data frame for Class in AF
af_cat_Class<-data.frame(cat=af_bt_Class$Type) 
af_cat_Class$cat<-as.double(af_cat_Class$cat)

af_Class_jn<- right_join(as.data.frame(Class_full), 
            af_cat_Class, 
            by="cat") 

af_Class_bind<-bind_cols(af_Class_jn, af_bt_Class) %>%
     dplyr::rename(cov=1,cat=2)

#----------create data frame for remaining Class not in AF
afleg_wrd_dif<-setdiff(Class_full_leg, af_Class_jn$cov)
afcod_wrd_dif<-setdiff(Class_full_cod,  af_Class_jn$cat)

#----------merge data for Class in AF and not in AF
af_Class_diff<-data.frame(cov=afleg_wrd_dif,
                         cat=afcod_wrd_dif, 
                         type=afcod_wrd_dif,
                         mean=NA,(afcod_wrd_dif),
                         ci_low=NA,(afcod_wrd_dif),
                         ci_high=NA,(afcod_wrd_dif))
  
l_df_af_Class<-bind_rows(af_Class_bind,af_Class_diff) %>% 
                        arrange(cat)  %>% 
                        select(cov, cat, mean, ci_low, ci_high)

gg_plot(l_df_af_Class) + scale_x_discrete(labels=rev(l_df_af_Class[,1]))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@-----CC

#----------Boosting for mean and ci
cc_li_Class<-split(l_df_cc$effectSize,l_df_cc$Class)
cc_li_Class<-list_drop_empty(cc_li_Class)

cc_bt_Class<-as.data.frame(t(map_df(cc_li_Class, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#----------create data frame for Class in CC
cc_cat_Class<-data.frame(cat=cc_bt_Class$Type) 
cc_cat_Class$cat<-as.double(cc_cat_Class$cat)

cc_Class_jn<- right_join(as.data.frame(Class_full), 
            cc_cat_Class, 
            by="cat") 

cc_Class_bind<-bind_cols(cc_Class_jn, cc_bt_Class) %>%
     dplyr::rename(cov=1,cat=2)

#----------create data frame for remaining Class not in CC
ccleg_wrd_dif<-setdiff(Class_full_leg, cc_Class_jn$cov)
cccod_wrd_dif<-setdiff(Class_full_cod,  cc_Class_jn$cat)

#----------merge data for Class in CC and not in CC
cc_Class_diff<-data.frame(cov=ccleg_wrd_dif,
                         cat=cccod_wrd_dif, 
                         type=cccod_wrd_dif,
                         mean=NA,
                         ci_low=NA,
                         ci_high=NA)
  
l_df_cc_Class<-bind_rows(cc_Class_bind,cc_Class_diff) %>% 
                        arrange(cat)  %>% 
                        select(cov, cat, mean, ci_low, ci_high)

gg_plot(l_df_cc_Class) + scale_x_discrete(labels=rev(l_df_cc_Class[,1]))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@-----NT

#----------Boosting for mean and ci
nt_li_Class<-split(l_df_nt$effectSize,l_df_nt$Class)
nt_li_Class<-list_drop_empty(nt_li_Class)

nt_bt_Class<-as.data.frame(t(map_df(nt_li_Class, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#----------create data frame for Class in NT
nt_cat_Class<-data.frame(cat=nt_bt_Class$Type) 
nt_cat_Class$cat<-as.double(nt_cat_Class$cat)

nt_Class_jn<- right_join(as.data.frame(Class_full), 
            nt_cat_Class, 
            by="cat") 

nt_Class_bind<-bind_cols(nt_Class_jn, nt_bt_Class) %>%
     dplyr::rename(cov=1,cat=2)

#----------create data frame for remaining Class not in NT
ntleg_wrd_dif<-setdiff(Class_full_leg, nt_Class_jn$cov)
ntcod_wrd_dif<-setdiff(Class_full_cod,  nt_Class_jn$cat)

#----------merge data for Class in NT and not in NT
# nt_Class_diff<-data.frame(cov=ntleg_wrd_dif,
#                          cat=ntcod_wrd_dif, 
#                          type=ntcod_wrd_dif,
#                          mean=NA,
#                          ci_low=NA,
#                          ci_high=NA)
  
l_df_nt_Class<-nt_Class_bind %>% 
                        arrange(cat)  %>% 
                        select(cov, cat, mean, ci_low, ci_high)

gg_plot(l_df_nt_Class) + scale_x_discrete(labels=rev(l_df_nt_Class[,1]))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@-----OF

#----------Boosting for mean and ci
of_li_Class<-split(l_df_of$effectSize,l_df_of$Class)
of_li_Class<-list_drop_empty(of_li_Class)

of_bt_Class<-as.data.frame(t(map_df(of_li_Class, es_boot))) %>%
   rownames_to_column(var = "Type")%>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

#----------create data frame for Class in OF
of_cat_Class<-data.frame(cat=of_bt_Class$Type) 
of_cat_Class$cat<-as.double(of_cat_Class$cat)

of_Class_jn<- right_join(as.data.frame(Class_full), 
            of_cat_Class, 
            by="cat") 

of_Class_bind<-bind_cols(of_Class_jn, of_bt_Class) %>%
     dplyr::rename(cov=1,cat=2)

#----------create data frame for remaining Class not in OF
ofleg_wrd_dif<-setdiff(Class_full_leg, of_Class_jn$cov)
ofcod_wrd_dif<-setdiff(Class_full_cod,  of_Class_jn$cat)

#----------merge data for Class in OF and not in OF
of_Class_diff<-data.frame(cov=ofleg_wrd_dif,
                         cat=ofcod_wrd_dif, 
                         type=ofcod_wrd_dif,
                         mean=NA,
                         ci_low=NA,
                         ci_high=NA)
  
l_df_of_Class<-bind_rows(of_Class_bind,of_Class_diff) %>% 
                        arrange(cat)  %>% 
                        select(cov, cat, mean, ci_low, ci_high)

# make the plots--------wrb
df_af_wrb1<- df_af_wrb %>% add_column(Data_type="wrb_af") %>% select(1:6)
df_cc_wrb1<- df_cc_wrb %>% add_column(Data_type="wrb_cc") %>% select(1:6)
df_nt_wrb1<- df_nt_wrb %>% add_column(Data_type="wrb_nt") %>% select(1:6)
df_of_wrb1<- df_of_wrb %>% add_column(Data_type="wrb_of") %>% select(1:6)

wrb_af<-gg_plot(df_af_wrb) + scale_x_discrete(labels=rev(df_af_wrb[,1]))
wrb_cc<-gg_plot(df_cc_wrb) + scale_x_discrete(labels=rev(df_cc_wrb[,1])) +theme(axis.text.y=element_blank())
wrb_nt<-gg_plot(df_nt_wrb) + scale_x_discrete(labels=rev(df_nt_wrb[,1]))+theme(axis.text.y=element_blank())
wrb_of<-gg_plot(df_of_wrb) + scale_x_discrete(labels=rev(df_of_wrb[,1]))+theme(axis.text.y=element_blank())

# make the plots--------LANDFORM
lf_af1<- l_df_af_Class %>% add_column(Data_type="landform_af")
lf_cc1<- l_df_cc_Class %>% add_column(Data_type="landform_cc")
lf_nt1<- l_df_nt_Class %>% add_column(Data_type="landform_nt")
lf_of1<- l_df_of_Class %>% add_column(Data_type="landform_of")

lf_af<-gg_plot(l_df_af_Class) + scale_x_discrete(labels=rev(l_df_af_Class[,1]))
lf_cc<-gg_plot(l_df_cc_Class) + scale_x_discrete(labels=rev(l_df_cc_Class[,1]))+theme(axis.text.y=element_blank())
lf_nt<-gg_plot(l_df_nt_Class) + scale_x_discrete(labels=rev(l_df_nt_Class[,1]))+theme(axis.text.y=element_blank())
lf_of<-gg_plot(l_df_of_Class) + scale_x_discrete(labels=rev(l_df_of_Class[,1]))+theme(axis.text.y=element_blank())

# bind all data
all<-bind_rows(df_af_wrb1, df_cc_wrb1, df_nt_wrb1, df_of_wrb1, lf_af1, lf_cc1, lf_nt1, lf_of1)
write_xlsx(all, "./input/wrb_landform/wrb_ldf.xlsx")
