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

tr_theme<-theme(
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

plt<-function(data,drivers){
  
  data %>% dplyr::arrange(mean) %>%
  mutate(cat = factor(cat, levels= c(
  drivers
  ))) %>%
  ggplot(aes(x=cat,y= mean,group=mgt)) +
  geom_point( size=2) +
  geom_errorbar(aes(x = cat, ymin = ci_low, ymax = ci_high),
                width=0.3,size=0.80) +
      coord_flip(clip = "off")+
       facet_wrap(~mgt, ncol = 4)+
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=0.90) +
   #scale_color_manual(values="#000000")+
   scale_y_continuous(limits = c(-50, 90))+
   facet_wrap(~mgt, ncol = 4)+
   tr_theme
}
  
arid<-read_xlsx("./output/mean_ci/df_supp_mat1.xlsx",guess_max = 1000)
cont<-read_xlsx("./output/mean_ci/df_supp_mat2.xlsx",guess_max = 1000)
temp<-read_xlsx("./output/mean_ci/df_supp_mat3.xlsx",guess_max = 1000)
trop<-read_xlsx("./output/mean_ci/df_supp_mat4.xlsx",guess_max = 1000)

a_crops<- arid %>% filter(cat %in% c("overall","Maize","Rice","Soybean",
                                         "Wheat","Other cereal","Cash crop",
                                         "V_F_others")) %>% as.data.frame()

c_crops<- cont %>% filter(cat %in% c("overall","Maize","Rice","Soybean",
                                         "Wheat","Other cereal","Cash crop",
                                         "V_F_others")) %>% as.data.frame()

tm_crops<- temp %>% filter(cat %in% c("overall","Maize","Rice","Soybean",
                                         "Wheat","Other cereal","Cash crop",
                                         "V_F_others")) %>% as.data.frame()

tr_crops<- trop %>% filter(cat %in% c("overall","Maize","Rice","Soybean",
                                         "Wheat","Other cereal","Cash crop",
                                         "V_F_others")) %>% as.data.frame()

df_crops<-bind_rows(a_crops,c_crops,tm_crops,tr_crops)
dim(df_crops)
head(df_crops)

head(a_crops); head(c_crops);head(tm_crops);head(tr_crops)
list_crops<-c("overall","Maize","Rice","Soybean",
              "Wheat","Other cereal","Cash crop",
              "V_F_others")
plt(a_crops,list_crops)
plt(c_crops,list_crops)

