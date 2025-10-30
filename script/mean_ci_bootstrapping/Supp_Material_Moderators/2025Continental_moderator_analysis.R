
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

# Get data per key
af1<-read_xlsx("./output/mean_ci/AF_clim/arng_AF_Continental_mean_ci_sliced.xlsx") %>% 
  dplyr::select(cov,cat,cat1,mean,ci_low,ci_high) %>%
  add_column(key="AF")

cc1<-read_xlsx("./output/mean_ci/CC_clim/arng_CC_Continental_mean_ci_sliced.xlsx") %>%
  dplyr::select(cov,cat,cat1,mean,ci_low,ci_high)%>%
  add_column(key="CC")

tail(cc1)

nt1<-read_xlsx("./output/mean_ci/NT_clim/arng_NT_Continental_mean_ci_sliced.xlsx") %>% 
  dplyr::select(cov,cat,cat1,mean,ci_low,ci_high)%>%
  add_column(key="NT")

of1<-read_xlsx("./output/mean_ci/OF_clim/arng_OF_Continental_mean_ci_sliced.xlsx") %>%
  dplyr::select(cov,cat,cat1,mean,ci_low,ci_high)%>%
  add_column(key="OF")

all_df1<-bind_rows(af1,cc1,nt1,of1) %>% as.data.frame() %>%
   dplyr::mutate(cat = ifelse(cat== "Veg&Fruit and others",
      "V_F_others",cat))

#data$cat[data$cat == "GDD_maize5"] <- "6-10"

# Create vector wrb and landform

wrb_full_leg<-c("Acrisols","Albeluvisols","Alisols","Andosols","Arenosols",
                 "Calcisols","Cambisols","Chernozems","Ferralsols","Fluvisols",	
                 "Gleysols","Gypsisols","Histosols","Kastanozems","Leptosols",
                 "Lixisols","Luvisols","Phaeozems","Plinthosols","Podzols",
                 "Regosols","Solonetz","Vertisols")

landform_full_leg<-c("Mtn_sumt","Cliff_sl","Lwhi_mtn","Shills_dcsl","Lhgsl_steep",
                 "Lhgsl_mod","Mtn_vs","Mod_hills","Tfphi_dis","Tfphi_surf",	
                 "Val_sl","Tfplw_dis","Tfplw_surf","Hi_plain","Lw_plain")

# Change GDD, wrb, landform values in rows
all_df2<- all_df1 %>% dplyr::mutate(cat=case_when(cat== "GDD_maize1" ~ "<0.8",
                                          cat== "GDD_maize2" ~ "0.8-2.7",
                                          cat== "GDD_maize3" ~ "2.7-4",
                                          cat== "GDD_maize4" ~ "4-6",
                                          cat== "GDD_maize5" ~ "6-10",
                                          cat== "GDD_rice1" ~ "<0.8",
                                          cat== "GDD_rice2" ~ "0.8-2.7",
                                          cat== "GDD_rice3" ~ "2.7-4",
                                          cat== "GDD_rice4" ~ "4-6",
                                          cat== "GDD_rice5" ~ "6-10",
                                          cat== "GDD_soybean1" ~ "<0.8",
                                          cat== "GDD_soybean2" ~ "0.8-2.7",
                                          cat== "GDD_soybean3" ~ "2.7-4",
                                          cat== "GDD_soybean4" ~ "4-6",
                                          cat== "GDD_soybean5" ~ "6-10",
                                          cat== "GDD_wheat1" ~ "<0.8",
                                          cat== "GDD_wheat2" ~ "0.8-2.7",
                                          cat== "GDD_wheat3" ~ "2.7-4",
                                          cat== "GDD_wheat4" ~ "4-6",
                                          cat== "GDD_wheat5" ~ "6-10",
                                          cov==wrb_full_leg[1 ]~ cov,
                                          cov==wrb_full_leg[2 ]~ cov,
                                          cov==wrb_full_leg[3 ]~ cov,
                                          cov==wrb_full_leg[4 ]~ cov,
                                          cov==wrb_full_leg[5 ]~ cov,
                                          cov==wrb_full_leg[6 ]~ cov,
                                          cov==wrb_full_leg[7 ]~ cov,
                                          cov==wrb_full_leg[8 ]~ cov,
                                          cov==wrb_full_leg[9 ]~ cov,
                                          cov==wrb_full_leg[10 ]~ cov,
                                          cov==wrb_full_leg[11 ]~ cov,
                                          cov==wrb_full_leg[12 ]~ cov,
                                          cov==wrb_full_leg[13 ]~ cov,
                                          cov==wrb_full_leg[14 ]~ cov,
                                          cov==wrb_full_leg[15 ]~ cov,
                                          cov==wrb_full_leg[16 ]~ cov,
                                          cov==wrb_full_leg[17 ]~ cov,
                                          cov==wrb_full_leg[18 ]~ cov,
                                          cov==wrb_full_leg[19 ]~ cov,
                                          cov==wrb_full_leg[20 ]~ cov,
                                          cov==wrb_full_leg[21 ]~ cov,
                                          cov==wrb_full_leg[22 ]~ cov,
                                          cov==wrb_full_leg[23 ]~ cov,
                                          cov==landform_full_leg[1 ]~ cov,
                                          cov==landform_full_leg[2 ]~ cov,
                                          cov==landform_full_leg[3 ]~ cov,
                                          cov==landform_full_leg[4 ]~ cov,
                                          cov==landform_full_leg[5 ]~ cov,
                                          cov==landform_full_leg[6 ]~ cov,
                                          cov==landform_full_leg[7 ]~ cov,
                                          cov==landform_full_leg[8 ]~ cov,
                                          cov==landform_full_leg[9 ]~ cov,
                                          cov==landform_full_leg[10 ]~ cov,
                                          cov==landform_full_leg[11 ]~ cov,
                                          cov==landform_full_leg[12 ]~ cov,
                                          cov==landform_full_leg[13 ]~ cov,
                                          cov==landform_full_leg[14 ]~ cov,
                                          cov==landform_full_leg[15 ]~ cov,
                                          TRUE ~ cat  # Keep other values unchanged
                                              ))

# Finds the first non-missing value at each position
all_df<-all_df2 %>% dplyr::select(key,cov,cat,mean,ci_low,ci_high) 
af<-all_df %>% filter(key %in% c("AF")) %>% dplyr::select(cov:ci_high)
cc<-all_df %>% filter(key %in% c("CC")) %>% dplyr::select(cov:ci_high)
nt<-all_df %>% filter(key %in% c("NT")) %>% dplyr::select(cov:ci_high)
of<-all_df %>% filter(key %in% c("OF")) %>% dplyr::select(cov:ci_high)

# Create the empty moderator data frame

cat_crop = c("V_F_others","Cash crop","Other cereal","Wheat",
              "Soybean","Rice","Maize","overall")
df_crops0<-data.frame(cov=cat_crop, cat_crop, mean = NA, ci_low=NA,ci_high=NA) %>%
                    dplyr::rename(cat=cat_crop)

cat_aridity<-c("aridity1","aridity2","aridity3","aridity4","aridity5")
df_aridity0<-data.frame(cov=cat_aridity, 
                        cat = c("<0.05","0.05-0.20","0.20-0.50","0.50-0.65",">0.65"), 
                        cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

cat_gdd<-c("gdd1","gdd2","gdd3","gdd4","gdd5")
df_gdd0<-data.frame(cov=cat_gdd,
                    cat = c("<0.8","0.8-2.7","2.7-4","4-6","6-10"), 
                    cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

cat_ph<-c("alkaline","neutral","acidic")
df_pH0<-data.frame(cov=cat_ph,
                   cat = c("alkaline","neutral","acidic"),
                   cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

cat_soc<-c("soc1","soc2","soc3")
df_soc0<-data.frame(cov=cat_soc,
                    cat = c("<5","5-10",">10"),
                    cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

cat_p<-c("p1","p2","p3")
df_p0<-data.frame(cov=cat_p,
                  cat = c("<10.9","10.9-21.4",">21.4"),
                  cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

cat_bd<-c("bd1","bd2","bd3")
df_bd0<-data.frame(cov=cat_bd,
                   cat = c("<1.20","1.20-1.47",">1.47"),
                   cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

cat_texture<-c("texture1","texture2","texture3")
df_tex0<-data.frame(cov=cat_texture,
                    cat = c("fine","medium","coarse"),
                    cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

cat_dem<-c("dem1","dem2","dem3")
df_dem0<-data.frame(cov=cat_dem,
                    cat = c("<250","250-1000",">1000"),
                    cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

cat_slope<-c("slope1","slope2","slope3","slope4","slope5")
df_slope0<-data.frame(cov=cat_slope,
                      cat = c("<0.20","0.2-1","1-5","5-15",">15"),
                      cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)


df_landform0<-data.frame(cov=landform_full_leg,
                      cat = landform_full_leg,
                      cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_wrb0<-data.frame(cov=wrb_full_leg,
                      cat = wrb_full_leg,
                      cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

# Extract crops data from df
df_af_crops1<- af %>% filter(cat %in% c("overall","Maize","Rice","Soybean",
                                         "Wheat","Other cereal","Cash crop",
                                         "V_F_others")) %>% as.data.frame()

df_cc_crops1<- cc %>% filter(cat %in% c("overall","Maize","Rice","Soybean",
                                         "Wheat","Other cereal","Cash crop",
                                         "V_F_others")) %>% as.data.frame()

df_nt_crops1<- nt %>% filter(cat %in% c("overall","Maize","Rice","Soybean",
                                         "Wheat","Other cereal","Cash crop",
                                         "V_F_others")) %>% as.data.frame()

df_of_crops1<- of %>% filter(cat %in% c("overall","Maize","Rice","Soybean",
                                         "Wheat","Other cereal","Cash crop",
                                         "V_F_others")) %>% as.data.frame()

df_af_crops<-full_join(df_af_crops1,df_crops0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="AF") 

df_cc_crops<-full_join(df_cc_crops1,df_crops0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="CC") 

df_nt_crops<-full_join(df_nt_crops1,df_crops0) %>%
             distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="NT") 

df_of_crops<-full_join(df_of_crops1,df_crops0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(1,8,7,2,3,4,5,6)%>%
            add_column(mgt="OF") 

comb_crops<-bind_rows(df_af_crops,df_cc_crops,df_of_crops,df_nt_crops)

# Extract aridity data from df
df_af_aridity1<- af %>% filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50","0.50-0.65",">0.65")) %>% as.data.frame()
df_cc_aridity1<- cc %>% filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50","0.50-0.65",">0.65")) %>% as.data.frame()
df_nt_aridity1<- nt %>% filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50","0.50-0.65",">0.65")) %>% as.data.frame()
df_of_aridity1<- of %>% filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50","0.50-0.65",">0.65")) %>% as.data.frame()

df_af_aridity<-full_join(df_af_aridity1,df_aridity0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="AF") 

df_cc_aridity<-full_join(df_cc_aridity1,df_aridity0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="CC") 

df_nt_aridity<-full_join(df_nt_aridity1,df_aridity0) %>%
             distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="NT") 

df_of_aridity<-full_join(df_of_aridity1,df_aridity0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(1,8,7,2,3,4,5,6)%>%
            add_column(mgt="OF") 

comb_aridity<-bind_rows(df_af_aridity,df_cc_aridity,df_of_aridity,df_nt_aridity)
comb_aridity

# Extract gdd data from df
df_af_gdd1<- af %>% filter(cat %in% c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10")) %>% as.data.frame()

df_cc_gdd1<- cc %>% filter(cat %in% c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10")) %>% as.data.frame()

df_nt_gdd1<- nt %>% filter(cat %in% c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10")) %>% as.data.frame()

df_of_gdd1<- of %>% filter(cat %in% c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10")) %>% as.data.frame()

df_af_gdd<-full_join(df_af_gdd1,df_gdd0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(1,5,4,3,2) %>%
            add_column(mgt="AF") 

df_cc_gdd<-full_join(df_cc_gdd1,df_gdd0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(4,3,2,1,5) %>%
            add_column(mgt="CC") 

df_nt_gdd<-full_join(df_nt_gdd1,df_gdd0) %>%
             distinct(cat,.keep_all = TRUE) %>%
             slice(1,5,4,3,2) %>%
            add_column(mgt="NT") 

df_of_gdd<-full_join(df_of_gdd1,df_gdd0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(1,2,3,5,4)%>%
            add_column(mgt="OF") 

comb_gdd<-bind_rows(df_af_gdd,df_cc_gdd,df_nt_gdd,df_of_gdd,)


# Extract pH data from df
df_af_pH1<- af %>% filter(cat %in% c("acidic","neutral","alkaline")) %>% as.data.frame()
df_cc_pH1<- cc %>% filter(cat %in% c("acidic","neutral","alkaline")) %>% as.data.frame()
df_nt_pH1<- nt %>% filter(cat %in% c("acidic","neutral","alkaline")) %>% as.data.frame()
df_of_pH1<- of %>% filter(cat %in% c("acidic","neutral","alkaline")) %>% as.data.frame()

df_af_pH<-full_join(df_af_pH1,df_pH0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(2,3,1) %>%
            add_column(mgt="AF") 

df_cc_pH<-full_join(df_cc_pH1,df_pH0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(2,3,1) %>%
            add_column(mgt="CC") 

df_nt_pH<-full_join(df_nt_pH1,df_pH0) %>%
             distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="NT") 

df_of_pH<-full_join(df_of_pH1,df_pH0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(2,3,1)%>%
            add_column(mgt="OF") 

comb_pH<-bind_rows(df_af_pH,df_cc_pH,df_of_pH,df_nt_pH)
comb_pH

# Extract soc data from df
df_af_soc1<- af %>% filter(cat %in% c("<5","5-10",">10")) %>% as.data.frame()
df_cc_soc1<- cc %>% filter(cat %in% c("<5","5-10",">10")) %>% as.data.frame()
df_nt_soc1<- nt %>% filter(cat %in% c("<5","5-10",">10")) %>% as.data.frame()
df_of_soc1<- of %>% filter(cat %in% c("<5","5-10",">10")) %>% as.data.frame()

df_af_soc<-full_join(df_af_soc1,df_soc0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(3,1,2) %>%
            add_column(mgt="AF") 

df_cc_soc<-full_join(df_cc_soc1,df_soc0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(1,3,2) %>%
            add_column(mgt="CC") 

df_nt_soc<-full_join(df_nt_soc1,df_soc0) %>%
             distinct(cat,.keep_all = TRUE) %>%
             #slice(1,3,2) %>%
             add_column(mgt="NT") 

df_of_soc<-full_join(df_of_soc1,df_soc0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(3,1,2)%>%
            add_column(mgt="OF") 

comb_soc<-bind_rows(df_af_soc,df_cc_soc,df_of_soc,df_nt_soc)
comb_soc


# Extract P data from df
df_af_p1<- af %>% filter(cat %in% c("<10.9","10.9-21.4",">21.4")) %>% as.data.frame()
df_cc_p1<- cc %>% filter(cat %in% c("<10.9","10.9-21.4",">21.4")) %>% as.data.frame()
df_nt_p1<- nt %>% filter(cat %in% c("<10.9","10.9-21.4",">21.4")) %>% as.data.frame()
df_of_p1<- of %>% filter(cat %in% c("<10.9","10.9-21.4",">21.4")) %>% as.data.frame()

df_af_p<-full_join(df_af_p1,df_p0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="AF") 

df_cc_p<-full_join(df_cc_p1,df_p0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(3,1,2) %>%
            add_column(mgt="CC") 

df_nt_p<-full_join(df_nt_p1,df_p0) %>%
             distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="NT") 

df_of_p<-full_join(df_of_p1,df_p0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(1,8,7,2,3,4,5,6)%>%
            add_column(mgt="OF") 

comb_p<-bind_rows(df_af_p,df_cc_p,df_of_p,df_nt_p)
comb_p


# Extract BD data from df
df_af_bd1<- af %>% filter(cat %in% c("<1.20","1.20-1.47",">1.47")) %>% as.data.frame()
df_cc_bd1<- cc %>% filter(cat %in% c("<1.20","1.20-1.47",">1.47")) %>% as.data.frame()
df_nt_bd1<- nt %>% filter(cat %in% c("<1.20","1.20-1.47",">1.47")) %>% as.data.frame()
df_of_bd1<- of %>% filter(cat %in% c("<1.20","1.20-1.47",">1.47")) %>% as.data.frame()

df_af_bd<-full_join(df_af_bd1,df_bd0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="AF") 

df_cc_bd<-full_join(df_cc_bd1,df_bd0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="CC") 

df_nt_bd<-full_join(df_nt_bd1,df_bd0) %>%
             distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="NT") 

df_of_bd<-full_join(df_of_bd1,df_bd0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(3,1,2)%>%
            add_column(mgt="OF") 

comb_bd<-bind_rows(df_af_bd,df_cc_bd,df_of_bd,df_nt_bd)
comb_bd

# Extract texture data from df
df_af_tex1<- af %>% filter(cat %in% c("fine","medium","coarse")) %>% as.data.frame()
df_cc_tex1<- cc %>% filter(cat %in% c("fine","medium","coarse")) %>% as.data.frame()
df_nt_tex1<- nt %>% filter(cat %in% c("fine","medium","coarse")) %>% as.data.frame()
df_of_tex1<- of %>% filter(cat %in% c("fine","medium","coarse")) %>% as.data.frame()

df_af_tex<-full_join(df_af_tex1,df_tex0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(3,1,2) %>%
            add_column(mgt="AF") 

df_cc_tex<-full_join(df_cc_tex1,df_tex0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="CC") 

df_nt_tex<-full_join(df_nt_tex1,df_tex0) %>%
             distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="NT") 

df_of_tex<-full_join(df_of_tex1,df_tex0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(3,2,1)%>%
            add_column(mgt="OF") 

comb_tex<-bind_rows(df_af_tex,df_cc_tex,df_of_tex,df_nt_tex)
comb_tex


# Extract slope data from df
df_af_dem1<- af %>% filter(cat %in% c("<250","250-1000",">1000")) %>% as.data.frame()
df_cc_dem1<- cc %>% filter(cat %in% c("<250","250-1000",">1000")) %>% as.data.frame()
df_nt_dem1<- nt %>% filter(cat %in% c("<250","250-1000",">1000")) %>% as.data.frame()
df_of_dem1<- of %>% filter(cat %in% c("<250","250-1000",">1000")) %>% as.data.frame()

df_af_dem<-full_join(df_af_dem1,df_dem0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(3,1,2) %>%
            add_column(mgt="AF") 

df_cc_dem<-full_join(df_cc_dem1,df_dem0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="CC") 

df_nt_dem<-full_join(df_nt_dem1,df_dem0) %>%
             distinct(cat,.keep_all = TRUE) %>%
            add_column(mgt="NT") 

df_of_dem<-full_join(df_of_dem1,df_dem0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(1,8,7,2,3,4,5,6)%>%
            add_column(mgt="OF") 

comb_dem<-bind_rows(df_af_dem,df_cc_dem,df_of_dem,df_nt_dem)
comb_dem


# Extract slope data from df
df_af_slope1<- af %>% filter(cat %in% c("<0.20","0.2-1","1-5",
                                           "5-15",">15")) %>% as.data.frame()
df_cc_slope1<- cc %>% filter(cat %in% c("<0.20","0.2-1","1-5",
                                           "5-15",">15")) %>% as.data.frame()
df_nt_slope1<- nt %>% filter(cat %in% c("<0.20","0.2-1","1-5",
                                           "5-15",">15")) %>% as.data.frame()
df_of_slope1<- of %>% filter(cat %in% c("<0.20","0.2-1","1-5",
                                           "5-15",">15")) %>% as.data.frame()

df_af_slope<-full_join(df_af_slope1,df_slope0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(5,4,1,2,3) %>%
            add_column(mgt="AF") 

df_cc_slope<-full_join(df_cc_slope1,df_slope0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(5,4,1,2,3) %>%
            add_column(mgt="CC") 

df_nt_slope<-full_join(df_nt_slope1,df_slope0) %>%
             distinct(cat,.keep_all = TRUE) %>%
             slice(5,1,2,3,4) %>%
            add_column(mgt="NT") 

df_of_slope<-full_join(df_of_slope1,df_slope0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(5,1,2,3,4) %>%
            add_column(mgt="OF") 

comb_slope<-bind_rows(df_af_slope,df_cc_slope,df_of_slope,df_nt_slope)
comb_slope

# Extract wrb data from df
df_af_wrb1<- af %>% filter(cat %in% wrb_full_leg) %>% as.data.frame()
df_cc_wrb1<- cc %>% filter(cat %in%  wrb_full_leg) %>% as.data.frame()
df_nt_wrb1<- nt %>% filter(cat %in%  wrb_full_leg) %>% as.data.frame()
df_of_wrb1<- of %>% filter(cat %in%  wrb_full_leg) %>% as.data.frame()

df_af_wrb<-full_join(df_af_wrb1,df_wrb0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(5,4,1,2,3) %>%
            add_column(mgt="AF") 

df_cc_wrb<-full_join(df_cc_wrb1,df_wrb0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(5,4,1,2,3) %>%
            add_column(mgt="CC") 

df_nt_wrb<-full_join(df_nt_wrb1,df_wrb0) %>%
             distinct(cat,.keep_all = TRUE) %>%
             slice(5,1,2,3,4) %>%
            add_column(mgt="NT") 

df_of_wrb<-full_join(df_of_wrb1,df_wrb0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(5,1,2,3,4) %>%
            add_column(mgt="OF") 

comb_wrb<-bind_rows(df_af_wrb,df_cc_wrb,df_of_wrb,df_nt_wrb)
comb_wrb

# Extract landform data from df
df_af_landform1<- af %>% filter(cat %in% landform_full_leg) %>% as.data.frame()
df_cc_landform1<- cc %>% filter(cat %in%  landform_full_leg) %>% as.data.frame()
df_nt_landform1<- nt %>% filter(cat %in%  landform_full_leg) %>% as.data.frame()
df_of_landform1<- of %>% filter(cat %in%  landform_full_leg) %>% as.data.frame()

df_af_landform<-full_join(df_af_landform1,df_landform0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(5,4,1,2,3) %>%
            add_column(mgt="AF") 

df_cc_landform<-full_join(df_cc_landform1,df_landform0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(5,4,1,2,3) %>%
            add_column(mgt="CC") 

df_nt_landform<-full_join(df_nt_landform1,df_landform0) %>%
             distinct(cat,.keep_all = TRUE) %>%
             slice(5,1,2,3,4) %>%
            add_column(mgt="NT") 

df_of_landform<-full_join(df_of_landform1,df_landform0) %>%
            distinct(cat,.keep_all = TRUE) %>%
            slice(5,1,2,3,4) %>%
            add_column(mgt="OF") 

comb_landform<-bind_rows(df_af_landform,df_cc_landform,df_of_landform,df_nt_landform)
comb_landform


############## Combine all
comb_all<-bind_rows(comb_crops,comb_aridity,comb_pH,comb_soc,
                    comb_p,comb_bd,comb_tex,comb_dem,comb_slope,
                    comb_wrb,comb_landform) %>%
                    as.data.frame()
write_xlsx(comb_all,"./output/mean_ci/df_supp_mat2.xlsx")

#comb_all<-comb_all %>% filter(!cat %in% c("Cereal"))

# # The palette with black:
# cbbPalette <- c("#009E73", "#E69F00","#56B4E9","#999999",  "#F0E442", 
#                 "#0072B2", "#D55E00", "#CC79A7","#000000")

tr_theme1<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = "none",
         axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1),
         axis.text = element_text(size = 27),
         axis.title=element_blank(),
         strip.text = element_text(size=25, color = 'black'),
         strip.background = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
       )

tr_theme2<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.key.height= unit(2, 'cm'),
         legend.key.width= unit(4, 'cm'),
         legend.title = element_text(size=30),
         legend.text = element_text(size=30),
         axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1),
         axis.text = element_text(size = 27),
         axis.title=element_blank(),
         theme(strip.text = element_text(size = 30)),
         strip.text = element_text(size=25, color = 'black'),
         strip.background = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
       )

plt<-comb_all %>% dplyr::arrange(mean) %>%
  mutate(cat = factor(cat, levels= c(
wrb_full_leg[23],
wrb_full_leg[22],
wrb_full_leg[21],
wrb_full_leg[20],
wrb_full_leg[19],
wrb_full_leg[18],
wrb_full_leg[17],
wrb_full_leg[16],
wrb_full_leg[15],
wrb_full_leg[14],
wrb_full_leg[13],
wrb_full_leg[12],
wrb_full_leg[11],
wrb_full_leg[10],
wrb_full_leg[9],
wrb_full_leg[8],
wrb_full_leg[7],
wrb_full_leg[6],
wrb_full_leg[5],
wrb_full_leg[4],
wrb_full_leg[3],
wrb_full_leg[2],
wrb_full_leg[1],
landform_full_leg[1],
landform_full_leg[2],
landform_full_leg[3],
landform_full_leg[4],
landform_full_leg[5],
landform_full_leg[6],
landform_full_leg[7],
landform_full_leg[8],
landform_full_leg[9],
landform_full_leg[10],
landform_full_leg[11],
landform_full_leg[12],
landform_full_leg[13],
landform_full_leg[14],
landform_full_leg[15],
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
">10",        
"5-10",       
"<5",
">21.4",      
"10.9-21.4",  
"<10.9", 
"alkaline",   
"neutral",    
"acidic",
">1.47",      
"1.20-1.47",  
"<1.20", 
">0.65",  
"0.50-0.65",
"0.20-0.50",  
"0.05-0.20",  
"<0.05",
"V_F_others",
"Cash crop",  
"Other cereal",
"Wheat",     
"Soybean",    
"Rice",       
"Maize",      
"overall"
  ))) %>%
  ggplot(aes(x=cat,y= mean,group=mgt)) +
  geom_point( size=2) +
  geom_errorbar(aes(x = cat, ymin = ci_low, ymax = ci_high),
                width=0.3,size=0.80) +
      coord_flip(clip = "off") +
       facet_wrap(~mgt, ncol = 4) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=0.90) +
   #scale_color_manual(values="#000000")+
   scale_y_continuous(limits = c(-50, 90))+
   facet_wrap(~mgt, ncol = 4)
  
plt
plt_continental<- plt + tr_theme1 
plt_continental

# save the maps
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
ggsave(plt_continental,filename = "./output/graphs/supp_mat_2.png",
       width = 45,height = 75,
       dpi = 300, units = "cm")
 
ggsave(plt_continental,filename = "./output/graphs/supp_mat_2.pdf",
       width = 45,height = 65,
       dpi = 300, units = "cm")


