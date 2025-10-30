
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
library(readxl)

library(plyr)
library(fuzzyjoin)
library(janitor)
library(readr)
library(tidyr)
library(reshape2)

library(ggh4x)
library(showtext)
library(egg)
library(patchwork)

# Load all mangement data mean_ci
arid<-read_xlsx("./output/mean_ci/NT_clim/NT_arid_management.xlsx")  %>% 
                       as.data.frame() %>%
                       slice(7,6,5,4,3,2,1)
cont<-read_xlsx("./output/mean_ci/NT_clim/NT_Continental_management.xlsx")
temp<-read_xlsx("./output/mean_ci/NT_clim/NT_Temperate_management.xlsx")
trop<-read_xlsx("./output/mean_ci/NT_clim/NT_Tropical_management.xlsx")

all_mgt<-bind_rows(arid,cont,temp,trop)
hist(all_mgt$mean)


# save the data and plots
#write_xlsx(all_mgt,"./output/mean_ci/NT_clim/NT_all_mgt_mean_ci.xlsx")   

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
          axis.text.y = element_text(size = 18),
         axis.text.x = element_blank(),
          axis.title=element_blank(),
         # axis.title= element_text(size=18),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
       )


tr_theme3<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.background = element_rect(fill='transparent',color=NA),
         legend.key=element_blank(),
         legend.box.background = element_rect(fill='transparent'),
         axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1),
         axis.text = element_text(size = 18),
          axis.x.title=element_blank(),
         axis.title= element_text(size=18),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
       )


#all_mgt1<-read_xlsx("./output/mean_ci/NT_clim/NT_all_mgt_mean_ci.xlsx")

df_bind <- all_mgt %>% 
          mutate(cat2 = paste0(ID, " (",  total_count, ")"))

#df_bind1<- df_bind %>% filter(!total_count %in% c("1","2"))

df_bind1<-df_bind

# save the data and plots
#write_xlsx(df_bind1,"./output/mean_ci/NT_clim/red_NT_all_mgt_mean_ci.xlsx") 

min(df_bind1$mean1);max(df_bind1$mean1)
min(df_bind1$cil);max(df_bind1$cil)
min(df_bind1$cih);max(df_bind1$cih)

#all_mgt1$ID<-as.factor(all_mgt1$ID) 

plot_data<- df_bind1 %>% 
  dplyr::select(ID,cat2,mean1,cil,cih) %>%
  dplyr::mutate(no = row_number()) %>%
  arrange(desc(no))


# save the data and plots
write_xlsx(plot_data,"./output/mean_ci/NT_clim/NT_all_mgt_plot_data2.xlsx") 


arid_plot_data<-plot_data %>% slice(29:23)
cont_plot_data<-plot_data %>% slice(22:15)
temp_plot_data<-plot_data %>% slice(14:6)
trop_plot_data<-plot_data %>% slice(5:1)
unique(arid_plot_data$ID)
p1<- arid_plot_data %>%
  #arrange(mean) %>%
  mutate(ID = factor(ID,levels=c(
"Arid & only weed_control",                             
"Arid & soil_cover & weed_control",                     
"Arid & N_input & weed_control",                        
"Arid & N_input & soil_cover",                          
"Arid & N_input & soil_cover & weed_control",           
"Arid & N_input & soil_cover & weed_control & rotation",
"Arid & bt_all")
  )) %>%
  ggplot(aes(x=ID, y=mean1)) +
  geom_point(fill= "#000000", color= "#000000", size=1.5) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=3) +
    tr_theme2 +
  coord_flip() +
  geom_errorbar(aes(x = ID, ymin = cil, ymax = cih),
                color= "#000000",width=0.1,size=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  #geom_vline(xintercept=c(5.5,13.5,21.55),size=0.25)+
  scale_y_continuous(limits = c(-34, 75))+
  scale_x_discrete(labels=c(
"only weed control",
"soil cover + weed control",
"N input + weed control",
"N input + soil cover",
"N input + soil cover + weed control",
"N input + soil cover + weed control + rotation",
"Overall"
  ))
  p1

p2<- cont_plot_data %>%
  #arrange(mean1) %>%
  mutate(ID = factor(ID,levels=c(
"Continental & only soil_cover",
"Continental & soil_cover & weed_control",
"Continental & N_input & weed_control",
"Continental & soil_cover & weed_control & rotation",
"Continental & N_input & weed_control & rotation",
"Continental & N_input & soil_cover & weed_control",
"Continental & N_input & soil_cover & weed_control & rotation",
"Temperate & bt_all")
  )) %>%
  ggplot(aes(x=ID, y=mean1)) +
  geom_point(fill= "#000000", color= "#000000", size=1.5) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=3) +
  geom_errorbar(aes(x = ID, ymin = cil, ymax = cih),
                color= "#000000",width=0.1,size=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  #geom_vline(xintercept=c(5.5,13.5,21.55),size=0.25)+
  scale_y_continuous(limits = c(-34, 75))+
  scale_x_discrete(labels=c(
"only soil cover",
"soil cover  + weed control"	,
"N input  + weed control",
"soil cover  + weed control  + rotation"	,
"N input  + weed control  + rotation"	,
"N input  + soil cover  + weed control"	,
"N input  + soil cover  + weed control  + rotation",
"Overall"
#"overall"	
  ))+
  tr_theme2 +
  coord_flip()

p2


unique(temp_plot_data$ID)

p3<- temp_plot_data %>%
  #arrange(mean1) %>%
  mutate(ID = factor(ID,levels=c(
"Temperate & only weed_control",                             
"Temperate & soil_cover & weed_control", 
"Temperate & soil_cover & weed_control & rotation", 
"Temperate & N_input & soil_cover",
"Temperate & N_input & weed_control",  
"Temperate & N_input & weed_control & rotation", 
"Temperate & N_input & soil_cover & weed_control",  
"Temperate & N_input & soil_cover & weed_control & rotation",
"Temperate & bt_all")
  )) %>%
  ggplot(aes(x=ID, y=mean1)) +
  geom_point(fill= "#000000", color= "#000000", size=1.5) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=3) +
  geom_errorbar(aes(x = ID, ymin = cil, ymax = cih),
                color= "#000000",width=0.1,size=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  #geom_vline(xintercept=c(5.5,13.5,21.55),size=0.25)+
  scale_y_continuous(limits = c(-34,75))+
  scale_x_discrete(labels=c(
"only weed control",                             
"soil cover + weed control", 
"soil cover + weed control + rotation", 
"N input + soil cover",
"N input + weed control",  
"N input + weed control + rotation", 
"N input + soil cover + weed control",  
"N input + soil cover + weed control + rotation",
"Overall"
  ))+
  tr_theme2 +
  coord_flip()

p3

unique(trop_plot_data$ID)

p4<- trop_plot_data %>%
  #arrange(mean1) %>%
  mutate(ID = factor(ID,levels=c(
"Tropical & N_input & weed_control",
"Tropical & N_input & weed_control & rotation",
"Tropical & N_input & soil_cover & weed_control",
"Tropical & N_input & soil_cover & weed_control & rotation",
"Tropical & bt_all")
  )) %>%
  ggplot(aes(x=ID, y=mean1)) +
  geom_point(fill= "#000000", color= "#000000", size=1.5) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=3) +
  geom_errorbar(aes(x = ID, ymin = cil, ymax = cih),
                color= "#000000",width=0.1,size=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=1)+
  #geom_vline(xintercept=c(5.5,13.5,21.55),size=0.25)+
  scale_y_continuous(limits = c(-34, 75))+
   ylab(" % Change (Crop Yield)")+ xlab("")+
  scale_x_discrete(labels=c(
"N input  + weed control"	,
"N input  + weed control  + rotation"	,
"N input  + soil cover  + weed control"	,
"N input  + soil cover  + weed control  + rotation"	,
"Overall"
  ))+
  tr_theme3 +
  coord_flip()

p4

mgt_plot<-p1+p2+p3+p4+plot_layout(ncol=1)
mgt_plot

ggsave(mgt_plot,filename = "./output/graphs/Fig_5.pdf",
       width = 25,height = 20,
       dpi = 300, units = "cm")


ggsave(mgt_plot,filename = "./output/graphs/Fig_5.png",
       width = 26,height = 25,
       dpi = 300, units = "cm")
















# 
# fig1<- plot_data %>%
#   #arrange(mean1) %>%
#   mutate(ID = factor(ID,levels=c(
#   "Tropical & N_input & weed_control",
# "Tropical & N_input & weed_control & rotation",
# "Tropical & N_input & soil_cover & weed_control",
# "Tropical & N_input & soil_cover & weed_control & rotation",
# "Tropical & bt_all",
# "Temperate & only weed_control",
# "Temperate & soil_cover & weed_control",
# "Temperate & N_input & weed_control",
# "Temperate & N_input & soil_cover",
# "Temperate & N_input & weed_control & rotation",
# "Temperate & N_input & soil_cover & weed_control",
# "Temperate & N_input & soil_cover & weed_control & rotation",
# "Temperate & bt_all",
# "Continental & only soil_cover",
# "Continental & soil_cover & weed_control",
# "Continental & N_input & weed_control",
# "Continental & soil_cover & weed_control & rotation",
# "Continental & N_input & weed_control & rotation",
# "Continental & N_input & soil_cover & weed_control",
# "Continental & N_input & soil_cover & weed_control & rotation",
# "Continental & bt_all",
# "Arid & only weed_control",
# "Arid & soil_cover & weed_control",
# "Arid & N_input & weed_control",
# "Arid & N_input & soil_cover & weed_control",
# "Arid & N_input & soil_cover & weed_control & rotation",
# "Arid & bt_all")
#   )) %>%
#   ggplot(aes(x=ID, y=mean1)) +
#   geom_point(fill= "#000000", color= "#000000", size=1.5) +
#   #geom_line(aes(group = 1), color="lightblue",linewidth=3) +
#   geom_errorbar(aes(x = ID, ymin = cil, ymax = cih),
#                 color= "#000000",width=0.1,size=0.5) +
#   geom_hline(yintercept = 0, linetype="dotted", 
#                 color = "red", linewidth=1)+
#   geom_vline(xintercept=c(5.5,13.5,21.55),size=0.25)+
#   scale_x_discrete(labels=c(
# "Tropical & N_input & weed_control"	=	"N input  + weed control (14)"	,
# "Tropical & N_input & weed_control & rotation"	=	"N input  + weed control  + rotation (9)"	,
# "Tropical & N_input & soil_cover & weed_control"	=	"N input  + soil cover  + weed control (43)"	,
# "Tropical & N_input & soil_cover & weed_control & rotation"	=	"N input  + soil cover  + weed control  + rotation (25)"	,
# "Tropical & bt_all"	=	"overall (463)"	,
# "Temperate & only weed_control"	=	"only weed control (11)"	,
# "Temperate & soil_cover & weed_control"	=	"soil cover  + weed control (55)"	,
# "Temperate & N_input & weed_control"	=	"N input  + weed control (77)"	,
# "Temperate & N_input & soil_cover"	=	"N input  + soil cover (6)"	,
# "Temperate & N_input & weed_control & rotation"	=	"N input  + weed control  + rotation (13)"	,
# "Temperate & N_input & soil_cover & weed_control"	=	"N input  + soil cover  + weed control (223)"	,
# "Temperate & N_input & soil_cover & weed_control & rotation"	=	"N input  + soil cover  + weed control  + rotation (76)"	,
# "Temperate & bt_all"	=	"overall (463)"	,
# "Continental & only soil_cover"	=	"only soil cover (4)"	,
# "Continental & soil_cover & weed_control"	=	"soil cover  + weed control (18)"	,
# "Continental & N_input & weed_control"	=	"N input  + weed control (28)"	,
# "Continental & soil_cover & weed_control & rotation"	=	"soil cover  + weed control  + rotation (11)"	,
# "Continental & N_input & weed_control & rotation"	=	"N input  + weed control  + rotation (5)"	,
# "Continental & N_input & soil_cover & weed_control"	=	"N input  + soil cover  + weed control (189)"	,
# "Continental & N_input & soil_cover & weed_control & rotation"	=	"N input  + soil cover  + weed control  + rotation (81)"	,
# "Continental & bt_all"	=	"overall (337)"	,
# "Arid & only weed_control"	=	"only weed control (15)"	,
# "Arid & soil_cover & weed_control"	=	"soil cover  + weed control (13)"	,
# "Arid & N_input & weed_control"	=	"N input  + weed control (45)"	,
# "Arid & N_input & soil_cover & weed_control"	=	"N input  + soil cover  + weed control (147)"	,
# "Arid & N_input & soil_cover & weed_control & rotation"	=	"N input  + soil cover  + weed control  + rotation (6)"	,
# "Arid & bt_all"	=	"overall (228)"	
#   ))+
#   tr_theme2 +
#   coord_flip()
# 
# fig1
# 
# # Save the figures
# ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
# ggsave(fig1, filename = "./output/graphs/NT_all_ClimReg_figure.png", 
#        width = 15, height = 20, dpi = 300, units = "cm")
