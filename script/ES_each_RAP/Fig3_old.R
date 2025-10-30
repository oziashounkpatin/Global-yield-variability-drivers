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

#https://patchwork.data-imaginist.com/reference/wrap_plots.html
# https://bioinformatics.ccr.cancer.gov/docs/data-visualization-with-r/Lesson6_V2/

tr_theme1<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.background = element_rect(fill='transparent',color=NA),
         legend.key=element_blank(),
         legend.box.background = element_rect(fill='transparent'),
         axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1),
         axis.text = element_text(size = 20),
         axis.title=element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
       )

# Building function to plot each covariate
gg_plot <- function(data){
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  data %>% dplyr::arrange(data[,2]) %>%
  mutate(cat = factor(data[,1], levels= unique(data[,1]))) %>%
  ggplot(aes(x=cat,y= data[,2])) +
  geom_point(fill= "#000000", color= "#000000", size=1.5) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
  geom_errorbar(aes(x = cat, ymin = data[,3], ymax = data[,4]),
                color= "#000000",width=0.1,size=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=0.75)+
  scale_y_continuous(limits = c(-40,80))+
  scale_x_discrete(labels=data[,1])+
  tr_theme1 +
  #facet_wrap(~cov)+
  coord_flip()
}

# load data
af<-read_xlsx("./output/mean_ci/AF_clim/AF_mean_ci_plot_data.xlsx")
cc<-read_xlsx("./output/mean_ci/CC_clim/CC_mean_ci_plot_data.xlsx")
nt<-read_xlsx("./output/mean_ci/NT_clim/NT_mean_ci_plot_data.xlsx")
of<-read_xlsx("./output/mean_ci/OF_clim/OF_mean_ci_plot_data.xlsx")
wrb_ldf<-as.data.frame(read_xlsx("./input/wrb_landform/wrb_ldf.xlsx"))
hist(wrb_ldf$mean)

min(af$mean);max(af$mean)
min(cc$mean);max(cc$mean)
min(nt$mean);max(nt$mean)
min(of$mean);max(of$mean)

min(af$ci_low);max(af$ci_low)
min(cc$ci_low);max(cc$ci_low)
min(nt$ci_low);max(nt$ci_low)
min(of$ci_low);max(of$ci_low)

min(af$ci_high);max(af$ci_high)
min(cc$ci_high);max(cc$ci_high)
min(nt$ci_high);max(nt$ci_high)
min(of$ci_high);max(of$ci_high)

# wrb_ldf$cat<-as.character(wrb_ldf$cat)
# wrb_ldf_perc<- wrb_ldf %>% mutate(mean_perc= 100*(exp(mean) - 1)) %>% select(cov, cat, mean,ci_low, ci_high, mean_perc,Data_type)
# af_perc<- af %>% mutate(mean_perc= 100*(exp(mean) - 1)) %>% add_column(Data_type="AF") %>% select(cov, cat, mean,ci_low, ci_high, mean_perc, Data_type)
# cc_perc<- cc %>% mutate(mean_perc= 100*(exp(mean) - 1)) %>% add_column(Data_type="CC") %>% select(cov, cat, mean,ci_low, ci_high, mean_perc, Data_type)
# nt_perc<- nt %>% mutate(mean_perc= 100*(exp(mean) - 1)) %>% add_column(Data_type="NT") %>% select(cov, cat, mean,ci_low, ci_high, mean_perc, Data_type)
# of_perc<- of %>% mutate(mean_perc= 100*(exp(mean) - 1)) %>% add_column(Data_type="OF") %>% select(cov, cat, mean,ci_low, ci_high, mean_perc, Data_type)
# all_perc<-bind_rows(af_perc, cc_perc, nt_perc, of_perc, wrb_ldf_perc)
# write_xlsx(all_perc, "C:/Users/hounkpk1/RAP_Drivers/input/data/rap_all_perc.xlsx")

df_af_wrb<-wrb_ldf %>% filter(Data_type %in% c("wrb_af"))
df_cc_wrb<-wrb_ldf %>% filter(Data_type %in% c("wrb_cc"))
df_nt_wrb<-wrb_ldf %>% filter(Data_type %in% c("wrb_nt"))
df_of_wrb<-wrb_ldf %>% filter(Data_type %in% c("wrb_of"))

l_df_af_Class<-wrb_ldf %>% filter(Data_type %in% c("landform_af"))
l_df_cc_Class<-wrb_ldf %>% filter(Data_type %in% c("landform_cc"))
l_df_nt_Class<-wrb_ldf %>% filter(Data_type %in% c("landform_nt"))
l_df_of_Class<-wrb_ldf %>% filter(Data_type %in% c("landform_of"))

# crops data

df_af_crops1<- af %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("overall","Maize","Rice","Soybean",
                                           "Wheat","Cash crop","Other Other cereal",
                                           "V_F_others")) %>% as.data.frame()

df_cc_crops1<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("overall","Maize","Rice","Soybean",
                                           "Wheat","Other cereal","Cash crop",
                                           "V_F_others")) %>% as.data.frame() %>%
                                            slice(1,3,7,4,5,6,2,8)
df_cc_crops1

df_nt_crops1<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("overall","Maize","Rice","Soybean",
                                           "Wheat","Other cereal","Cash crop",
                                           "V_F_others")) %>% as.data.frame() %>%
                                            slice(1,3,2,4,5,6,7,8)

df_of_crops1<- of %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("overall","Maize","Rice","Soybean",
                                           "Wheat","Cash crop","Other cereal",
                                           "V_F_others")) %>% as.data.frame()

df_crops<-data.frame(cat = c("V_F_others","Other cereal","Cash crop","Wheat",
                             "Soybean","Rice","Maize","overall"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)


df_af_crops<-full_join(df_af_crops1,df_crops) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,5,6,7,8,3,4)
df_af_crops
  
df_of_crops<-full_join(df_of_crops1,df_crops) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,8,7,2,3,4,5,6)

df_nt_crops<-full_join(df_nt_crops1,df_crops) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,3,8,2,4,5,6,7)

df_cc_crops<-full_join(df_cc_crops1,df_crops) %>%
distinct(cat,.keep_all = TRUE) 
#slice(1,3,8,2,4,5,6,7)

df_of_crops

df_nt_crops
df_of_crops


# # Save each one of the covariates
# ggsave(plt_crops,filename = "./Next_Article/output/graphs/all_crops/cov_crops.png",
#        width = 35, height = 7, dpi = 300, units = "cm")


# climate

#-------------------AF
df_af_clim1<- af %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("Arid","Continental","Temperate",
                                           "Tropical")) %>% as.data.frame()

df_af_clim2<-data.frame(cat = c("Arid","Continental","Temperate","Tropical"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_af_clim<-full_join(df_af_clim1,df_af_clim2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4)
df_af_clim

df_af_ar1<- af %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50",
                                           "0.50-0.65",">0.65")) %>% as.data.frame()

df_af_ar2<-data.frame(cat = c("<0.05","0.05-0.20","0.20-0.50",
                              "0.50-0.65",">0.65"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_af_ar<-full_join(df_af_ar1,df_af_ar2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_af_ar

# gdd maize
df_af_gdd_maize1<- af %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_maize1","GDD_maize2","GDD_maize3",
                                        "GDD_maize4","GDD_maize5")) %>% as.data.frame()
df_af_gdd_maize1<- df_af_gdd_maize1 %>% mutate(cat=case_when(
                                                cat== "GDD_maize1" ~ "<0.8",
                                                cat== "GDD_maize2" ~ "0.8-2.7",
                                                cat== "GDD_maize3" ~ "2.7-4",
                                                cat== "GDD_maize4" ~ "4-6",
                                                cat== "GDD_maize5" ~ "6-10"
                                              ))
df_af_gdd_maize2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_af_gdd_maize<-full_join(df_af_gdd_maize1,df_af_gdd_maize2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,5,3,4)
df_af_gdd_maize

# # save maps
# ggsave(af_plt_clim,filename = "./Next_Article/output/graphs/all_clim/cov_clim.png",
#        width = 8, height = 6, dpi = 300, units = "cm")

#-------------------CC
df_cc_clim1<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("Arid","Continental","Temperate",
                                           "Tropical")) %>% as.data.frame()

df_cc_clim2<-data.frame(cat = c("Arid","Continental","Temperate","Tropical"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_cc_clim<-full_join(df_cc_clim1,df_cc_clim2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4)
df_cc_clim

df_cc_ar1<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50",
                                           "0.50-0.65",">0.65")) %>% as.data.frame()

df_cc_ar2<-data.frame(cat = c("<0.05","0.05-0.20","0.20-0.50",
                              "0.50-0.65",">0.65"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_cc_ar<-full_join(df_cc_ar1,df_cc_ar2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,5,4)
df_cc_ar


# gdd maize
df_cc_gdd_maize1<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_maize1","GDD_maize2","GDD_maize3",
                                        "GDD_maize4","GDD_maize5")) %>% as.data.frame()

df_cc_gdd_maize1<- df_cc_gdd_maize1 %>% mutate(cat=case_when(
                                                cat== "GDD_maize1" ~ "<0.8",
                                                cat== "GDD_maize2" ~ "0.8-2.7",
                                                cat== "GDD_maize3" ~ "2.7-4",
                                                cat== "GDD_maize4" ~ "4-6",
                                                cat== "GDD_maize5" ~ "6-10"
                                              ))
df_cc_gdd_maize2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_cc_gdd_maize<-full_join(df_cc_gdd_maize1,df_cc_gdd_maize2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_cc_gdd_maize

# gdd rice
df_cc_gdd_rice1<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_rice1","GDD_rice2","GDD_rice3",
                                        "GDD_rice4","GDD_rice5")) %>% as.data.frame()

df_cc_gdd_rice1<- df_cc_gdd_rice1 %>% mutate(cat=case_when(
                                                cat== "GDD_rice1" ~ "<0.8",
                                                cat== "GDD_rice2" ~ "0.8-2.7",
                                                cat== "GDD_rice3" ~ "2.7-4",
                                                cat== "GDD_rice4" ~ "4-6",
                                                cat== "GDD_rice5" ~ "6-10"
                                              ))
df_cc_gdd_rice2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_cc_gdd_rice<-full_join(df_cc_gdd_rice1,df_cc_gdd_rice2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_cc_gdd_rice

# gdd soy
df_cc_gdd_soy1<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_soybean1","GDD_soybean2","GDD_soybean3",
                                        "GDD_soybean4","GDD_soybean5")) %>% as.data.frame()

df_cc_gdd_soy1<- df_cc_gdd_soy1 %>% mutate(cat=case_when(
                                                cat== "GDD_soybean1" ~ "<0.8",
                                                cat== "GDD_soybean2" ~ "0.8-2.7",
                                                cat== "GDD_soybean3" ~ "2.7-4",
                                                cat== "GDD_soybean4" ~ "4-6",
                                                cat== "GDD_soybean5" ~ "6-10"
                                              ))
df_cc_gdd_soy2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_cc_gdd_soy<-full_join(df_cc_gdd_soy1,df_cc_gdd_soy2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_cc_gdd_soy

# gdd wheat
df_cc_gdd_wheat1<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_wheat1","GDD_wheat2","GDD_wheat3",
                                        "GDD_wheat4","GDD_wheat5")) %>% as.data.frame()

df_cc_gdd_wheat1<- df_cc_gdd_wheat1 %>% mutate(cat=case_when(
                                                cat== "GDD_wheat1" ~ "<0.8",
                                                cat== "GDD_wheat2" ~ "0.8-2.7",
                                                cat== "GDD_wheat3" ~ "2.7-4",
                                                cat== "GDD_wheat4" ~ "4-6",
                                                cat== "GDD_wheat5" ~ "6-10"
                                              ))
df_cc_gdd_wheat2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_cc_gdd_wheat<-full_join(df_cc_gdd_wheat1,df_cc_gdd_wheat2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_cc_gdd_wheat

#-------------------NT

df_nt_clim1<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("Arid","Continental","Temperate",
                                           "Tropical")) %>% as.data.frame()

df_nt_clim2<-data.frame(cat = c("Arid","Continental","Temperate","Tropical"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_nt_clim<-full_join(df_nt_clim1,df_nt_clim2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4)
df_nt_clim

df_nt_ar1<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50",
                                           "0.50-0.65",">0.65")) %>% as.data.frame()

df_nt_ar2<-data.frame(cat = c("<0.05","0.05-0.20","0.20-0.50",
                              "0.50-0.65",">0.65"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_nt_ar<-full_join(df_nt_ar1,df_nt_ar2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_nt_ar

df_nt_gdd_maize1<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_maize1","GDD_maize2","GDD_maize3",
                                        "GDD_maize4","GDD_maize5")) %>% as.data.frame()

df_nt_gdd_maize1<- df_nt_gdd_maize1 %>% mutate(cat=case_when(
                                                cat== "GDD_maize1" ~ "<0.8",
                                                cat== "GDD_maize2" ~ "0.8-2.7",
                                                cat== "GDD_maize3" ~ "2.7-4",
                                                cat== "GDD_maize4" ~ "4-6",
                                                cat== "GDD_maize5" ~ "6-10"
                                              ))
df_nt_gdd_maize2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_nt_gdd_maize<-full_join(df_nt_gdd_maize1,df_nt_gdd_maize2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,4)
df_nt_gdd_maize


# gdd rice
df_nt_gdd_rice1<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_rice1","GDD_rice2","GDD_rice3",
                                        "GDD_rice4","GDD_rice5")) %>% as.data.frame()

df_nt_gdd_rice1<- df_nt_gdd_rice1 %>% mutate(cat=case_when(
                                                cat== "GDD_rice1" ~ "<0.8",
                                                cat== "GDD_rice2" ~ "0.8-2.7",
                                                cat== "GDD_rice3" ~ "2.7-4",
                                                cat== "GDD_rice4" ~ "4-6",
                                                cat== "GDD_rice5" ~ "6-10"
                                              ))
df_nt_gdd_rice2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_nt_gdd_rice<-full_join(df_nt_gdd_rice1,df_nt_gdd_rice2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_nt_gdd_rice

# gdd soy
df_nt_gdd_soy1<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_soybean1","GDD_soybean2","GDD_soybean3",
                                        "GDD_soybean4","GDD_soybean5")) %>% as.data.frame()

df_nt_gdd_soy1<- df_nt_gdd_soy1 %>% mutate(cat=case_when(
                                                cat== "GDD_soybean1" ~ "<0.8",
                                                cat== "GDD_soybean2" ~ "0.8-2.7",
                                                cat== "GDD_soybean3" ~ "2.7-4",
                                                cat== "GDD_soybean4" ~ "4-6",
                                                cat== "GDD_soybean5" ~ "6-10"
                                              ))
df_nt_gdd_soy2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_nt_gdd_soy<-full_join(df_nt_gdd_soy1,df_nt_gdd_soy2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_nt_gdd_soy

# gdd wheat
df_nt_gdd_wheat1<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_wheat1","GDD_wheat2","GDD_wheat3",
                                        "GDD_wheat4","GDD_wheat5")) %>% as.data.frame()

df_nt_gdd_wheat1<- df_nt_gdd_wheat1 %>% mutate(cat=case_when(
                                                cat== "GDD_wheat1" ~ "<0.8",
                                                cat== "GDD_wheat2" ~ "0.8-2.7",
                                                cat== "GDD_wheat3" ~ "2.7-4",
                                                cat== "GDD_wheat4" ~ "4-6",
                                                cat== "GDD_wheat5" ~ "6-10"
                                              ))
df_nt_gdd_wheat2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_nt_gdd_wheat<-full_join(df_nt_gdd_wheat1,df_nt_gdd_wheat2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_nt_gdd_wheat

#-------------------OF
df_of_clim1<- of %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("Arid","Continental","Temperate",
                                           "Tropical")) %>% as.data.frame()

df_of_clim2<-data.frame(cat = c("Arid","Continental","Temperate","Tropical"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_of_clim<-full_join(df_of_clim1,df_of_clim2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4)
df_of_clim

df_of_ar1<- of %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.05","0.05-0.20","0.20-0.50",
                                           "0.50-0.65",">0.65")) %>% as.data.frame()

df_of_ar2<-data.frame(cat = c("<0.05","0.05-0.20","0.20-0.50",
                              "0.50-0.65",">0.65"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_of_ar<-full_join(df_of_ar1,df_of_ar2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_of_ar

df_of_gdd_maize1<- of %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_maize1","GDD_maize2","GDD_maize3",
                                        "GDD_maize4","GDD_maize5")) %>% as.data.frame()

df_of_gdd_maize1<- df_of_gdd_maize1 %>% mutate(cat=case_when(
                                                cat== "GDD_maize1" ~ "<0.8",
                                                cat== "GDD_maize2" ~ "0.8-2.7",
                                                cat== "GDD_maize3" ~ "2.7-4",
                                                cat== "GDD_maize4" ~ "4-6",
                                                cat== "GDD_maize5" ~ "6-10"
                                              ))
df_of_gdd_maize2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_of_gdd_maize<-full_join(df_of_gdd_maize1,df_of_gdd_maize2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_of_gdd_maize



# gdd rice
df_of_gdd_rice1<- of %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_rice1","GDD_rice2","GDD_rice3",
                                        "GDD_rice4","GDD_rice5")) %>% as.data.frame()

df_of_gdd_rice1<- df_of_gdd_rice1 %>% mutate(cat=case_when(
                                                cat== "GDD_rice1" ~ "<0.8",
                                                cat== "GDD_rice2" ~ "0.8-2.7",
                                                cat== "GDD_rice3" ~ "2.7-4",
                                                cat== "GDD_rice4" ~ "4-6",
                                                cat== "GDD_rice5" ~ "6-10"
                                              ))
df_of_gdd_rice2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_of_gdd_rice<-full_join(df_of_gdd_rice1,df_of_gdd_rice2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_of_gdd_rice

# gdd soy
df_of_gdd_soy1<- of %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_soybean1","GDD_soybean2","GDD_soybean3",
                                        "GDD_soybean4","GDD_soybean5")) %>% as.data.frame()

df_of_gdd_soy1<- df_of_gdd_soy1 %>% mutate(cat=case_when(
                                                cat== "GDD_soybean1" ~ "<0.8",
                                                cat== "GDD_soybean2" ~ "0.8-2.7",
                                                cat== "GDD_soybean3" ~ "2.7-4",
                                                cat== "GDD_soybean4" ~ "4-6",
                                                cat== "GDD_soybean5" ~ "6-10"
                                              ))
df_of_gdd_soy2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_of_gdd_soy<-full_join(df_of_gdd_soy1,df_of_gdd_soy2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_of_gdd_soy

# gdd wheat
df_of_gdd_wheat1<- of %>% select(cat,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_wheat1","GDD_wheat2","GDD_wheat3",
                                        "GDD_wheat4","GDD_wheat5")) %>% as.data.frame()

df_of_gdd_wheat1<- df_of_gdd_wheat1 %>% mutate(cat=case_when(
                                                cat== "GDD_wheat1" ~ "<0.8",
                                                cat== "GDD_wheat2" ~ "0.8-2.7",
                                                cat== "GDD_wheat3" ~ "2.7-4",
                                                cat== "GDD_wheat4" ~ "4-6",
                                                cat== "GDD_wheat5" ~ "6-10"
                                              ))
df_of_gdd_wheat2<-data.frame(cat = c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10"), 
                           cat2 = NA, mean = NA, ci_low=NA,ci_high=NA)
df_of_gdd_wheat<-full_join(df_of_gdd_wheat1,df_of_gdd_wheat2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(1,2,3,4,5)
df_of_gdd_wheat


# crops maps
p1<-gg_plot(df_af_crops) + theme(axis.text.x=element_blank()) 
p2<-gg_plot(df_cc_crops) + theme(axis.text=element_blank())
p3<-gg_plot(df_nt_crops) + theme(axis.text=element_blank())
p4<-gg_plot(df_of_crops) + theme(axis.text=element_blank())

p1+p2+p3

# Climate regions
p5<-gg_plot(df_af_clim)+ theme(axis.text.x=element_blank())
p6<-gg_plot(df_cc_clim)+ theme(axis.text=element_blank())
p7<-gg_plot(df_nt_clim)+ theme(axis.text=element_blank())
p8<-gg_plot(df_of_clim)+ theme(axis.text=element_blank())
df_af_clim
df_cc_clim
df_nt_clim
df_of_clim

# Aridity index
p9<-gg_plot(df_af_ar)+theme(axis.text.x=element_blank())
p10<-gg_plot(df_cc_ar)+ theme(axis.text=element_blank())
p11<-gg_plot(df_nt_ar)+ theme(axis.text=element_blank())
p12<-gg_plot(df_of_ar)+ theme(axis.text=element_blank())

# GDD Maize
p13<-gg_plot(df_af_gdd_maize) + theme(axis.text.x=element_blank())
p14<-gg_plot(df_cc_gdd_maize)+ theme(axis.text=element_blank())
p15<-gg_plot(df_nt_gdd_maize)+ theme(axis.text=element_blank())
p16<-gg_plot(df_of_gdd_maize)+ theme(axis.text=element_blank())

# GDD rice
p17<-gg_plot(df_cc_gdd_rice)+ theme(axis.text.x=element_blank())
p17bis<-gg_plot(df_cc_gdd_rice)+ theme(axis.text=element_blank())
p18<-gg_plot(df_nt_gdd_rice)+ theme(axis.text=element_blank())
# p18bis<-gg_plot(df_nt_gdd_rice)+ theme(axis.text=element_blank())
p17_1<-gg_plot(df_of_gdd_rice)+ theme(axis.text=element_blank())

# GDD soybean
p19<-gg_plot(df_cc_gdd_soy)+theme(axis.text.x=element_blank())
p19bis<-gg_plot(df_cc_gdd_soy)+theme(axis.text=element_blank())
p20<-gg_plot(df_nt_gdd_soy)+theme(axis.text=element_blank())
p20_1<-gg_plot(df_of_gdd_soy)+theme(axis.text=element_blank())

# GDD wheat
p21<-gg_plot(df_cc_gdd_wheat)
p21bis<-gg_plot(df_cc_gdd_wheat)+theme(axis.text.y=element_blank())
p22<-gg_plot(df_nt_gdd_wheat)+theme(axis.text.y=element_blank())
p22_1<-gg_plot(df_of_gdd_wheat)+theme(axis.text.y=element_blank())

# make the plots--------wrd
wrb_af<-gg_plot(df_af_wrb) + scale_x_discrete(labels=rev(df_af_wrb[,1])) +theme(axis.text.x=element_blank())
wrb_cc<-gg_plot(df_cc_wrb) + scale_x_discrete(labels=rev(df_cc_wrb[,1])) +theme(axis.text=element_blank())
wrb_nt<-gg_plot(df_nt_wrb) + scale_x_discrete(labels=rev(df_nt_wrb[,1]))+theme(axis.text=element_blank())
wrb_of<-gg_plot(df_of_wrb) + scale_x_discrete(labels=rev(df_of_wrb[,1]))+theme(axis.text=element_blank())


# make the plots--------LANDFORM
lf_af<-gg_plot(l_df_af_Class) + scale_x_discrete(labels=rev(l_df_af_Class[,1]))
lf_cc<-gg_plot(l_df_cc_Class) + scale_x_discrete(labels=rev(l_df_cc_Class[,1]))+theme(axis.text.y=element_blank())
lf_nt<-gg_plot(l_df_nt_Class) + scale_x_discrete(labels=rev(l_df_nt_Class[,1]))+theme(axis.text.y=element_blank())
lf_of<-gg_plot(l_df_of_Class) + scale_x_discrete(labels=rev(l_df_of_Class[,1]))+theme(axis.text.y=element_blank())

# Agroforestry
red_af<-af %>% dplyr::select(cov,cat,mean,ci_low,ci_high) 
red_df_af_wrb<- df_af_wrb %>% select(cov,cat,mean,ci_low,ci_high) %>% mutate(cat = as.character(cat))
red_l_df_af_Class<-l_df_af_Class %>% select(cov,cat,mean,ci_low,ci_high) %>% mutate(cat = as.character(cat))
af_full<-bind_rows(red_af,red_df_af_wrb,red_l_df_af_Class)
write_xlsx(af_full, "./output/Figure_data/af_fig_3.xlsx")

as.data.frame(af_full)

# Cover crop
red_cc<-cc %>% dplyr::select(cov,cat,mean,ci_low,ci_high) 
red_df_cc_wrb<- df_cc_wrb %>% select(cov,cat,mean,ci_low,ci_high) %>% mutate(cat = as.character(cat))
red_l_df_cc_Class<-l_df_cc_Class %>% select(cov,cat,mean,ci_low,ci_high) %>% mutate(cat = as.character(cat))
cc_full<-bind_rows(red_cc,red_df_cc_wrb,red_l_df_cc_Class)
write_xlsx(cc_full, "./output/Figure_data/cc_fig_3.xlsx")

# No-tillage
red_nt<-nt %>% dplyr::select(cov,cat,mean,ci_low,ci_high) 
red_df_nt_wrb<- df_nt_wrb %>% select(cov,cat,mean,ci_low,ci_high) %>% mutate(cat = as.character(cat))
red_l_df_nt_Class<-l_df_nt_Class %>% select(cov,cat,mean,ci_low,ci_high) %>% mutate(cat = as.character(cat))
nt_full<-bind_rows(red_nt,red_df_nt_wrb,red_l_df_nt_Class)
write_xlsx(nt_full, "./output/Figure_data/nt_fig_3.xlsx")

# Organic Farming
red_of<-of %>% dplyr::select(cov,cat,mean,ci_low,ci_high) 
red_df_of_wrb<- df_of_wrb %>% select(cov,cat,mean,ci_low,ci_high) %>% mutate(cat = as.character(cat))
red_l_df_of_Class<-l_df_of_Class %>% select(cov,cat,mean,ci_low,ci_high) %>% mutate(cat = as.character(cat))
of_full<-bind_rows(red_of,red_df_of_wrb,red_l_df_of_Class)
write_xlsx(of_full, "./output/Figure_data/of_fig_3.xlsx")

# https://github.com/thomasp85/patchwork/blob/main/R/plot_layout.R

         # wrb_af+wrb_cc+wrb_nt+wrb_of+ # soil type
        lf_af+lf_cc+lf_nt+lf_of# landform
         #   
gdd <-         p13+p14+p15+p16+ # GDD maize
               p17+p17bis+p18+ # Rice
               p19+p19bis+p20+ # Soybean
               p21+p21bis+p22+ # Wheat
      plot_layout(design = "ABCD
                            EFG#
                            HIJ#
                            KLM#",
                           heights = c(
                  0.20,
                  0.20,
                  0.20,
                  0.20))

driv<-   p1+p2+p3+p4+ # crops
         p5+p6+p7+p8+ # climate regions
         p9+p10+p11+p12+ # aridity
         p13+p14+p15+p16+ # GDD maize
         p17+p17bis+p18+p17_1+# Rice
         p19+p19bis+p20+p20_1+# Soybean
         p21+p21bis+p22+p22_1+ # Wheat
      plot_layout(#design = "ABCD
      #                       EFGH
      #                       IJKL
      #                       MNOP
      #                       QRS
      #                       TUV#
      #                       WXY#",
                           heights = c(
                  0.20,
                  0.10,
                  0.15,
                  0.20,
                  0.20,
                  0.20,
                  0.20),ncol=4)
driv

# 
# ggsave(gdd,filename = "./output/graphs/all_clim/gdd_crops.pdf",
#         width = 31, height=17,dpi = 300, units = "cm")

ggsave(driv,filename = "./output/graphs/Fig_3.png",
       width = 33, height=36,dpi = 300, units = "cm")

ggsave(driv,filename = "./output/graphs/Fig_3.pdf",
       width = 33, height=36,dpi = 300, units = "cm")

# ggsave(a,filename = "./output/graphs/all_clim/all_drivers_gdd3.pdf",
#        width = 33, height=55,dpi = 300, units = "cm")
