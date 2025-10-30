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
         axis.text = element_text(size = 16),
         axis.title=element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
       )

# Building function to plot each covariate
gg_plot <- function(data){
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  data %>% dplyr::arrange(data[,3]) %>%
  mutate(cat = factor(data[,1], levels= unique(data[,1]))) %>%
  ggplot(aes(x=cat,y= data[,3])) +
  geom_point(fill= "#000000", color= "#000000", size=1.5) +
  #geom_line(aes(group = 1), color="lightblue",linewidth=2) +
  geom_errorbar(aes(x = cat, ymin = data[,4], ymax = data[,5]),
                color= "#000000",width=0.1,size=0.5) +
  geom_hline(yintercept = 0, linetype="dotted", 
                color = "red", linewidth=0.80)+
  scale_y_continuous(limits = c(-40, 55))+
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

hist(cc$mean)

tail(nt)
min(af$mean);max(af$mean);min(af$ci_low);max(af$ci_high);
min(cc$mean);max(cc$mean);min(cc$ci_low);max(cc$ci_high);
min(nt$mean);max(nt$mean);min(nt$ci_low);max(nt$ci_high);
min(of$mean);max(of$mean);min(of$ci_low);max(of$ci_high);

wrb_ldf<-as.data.frame(read_xlsx("./input/wrb_landform/wrb_ldf.xlsx"))
# wrb_ldf$cat<-as.character(wrb_ldf$cat)
# wrb_ldf_perc<- wrb_ldf %>% mutate(mean_perc= 100*(exp(mean) - 1)) %>% select(cov, cat, mean,ci_low, ci_high, mean_perc,Data_type)
# af_perc<- af %>% mutate(mean_perc= 100*(exp(mean) - 1)) %>% add_column(Data_type="AF") %>% select(cov, cat, mean,ci_low, ci_high, mean_perc, Data_type)
# cc_perc<- cc %>% mutate(mean_perc= 100*(exp(mean) - 1)) %>% add_column(Data_type="CC") %>% select(cov, cat, mean,ci_low, ci_high, mean_perc, Data_type)
# nt_perc<- nt %>% mutate(mean_perc= 100*(exp(mean) - 1)) %>% add_column(Data_type="NT") %>% select(cov, cat, mean,ci_low, ci_high, mean_perc, Data_type)
# of_perc<- of %>% mutate(mean_perc= 100*(exp(mean) - 1)) %>% add_column(Data_type="OF") %>% select(cov, cat, mean,ci_low, ci_high, mean_perc, Data_type)
# all_perc<-bind_rows(af_perc, cc_perc, nt_perc, of_perc, wrb_ldf_perc)
# write_xlsx(all_perc, "C:/RAP_Drivers/input/data/rap_all_perc.xlsx")

df_af_wrb<-wrb_ldf %>% filter(Data_type %in% c("wrb_af"))
df_cc_wrb<-wrb_ldf %>% filter(Data_type %in% c("wrb_cc"))
df_nt_wrb<-wrb_ldf %>% filter(Data_type %in% c("wrb_nt"))
df_of_wrb<-wrb_ldf %>% filter(Data_type %in% c("wrb_of"))

l_df_af_Class<-wrb_ldf %>% filter(Data_type %in% c("landform_af"))
l_df_cc_Class<-wrb_ldf %>% filter(Data_type %in% c("landform_cc"))
l_df_nt_Class<-wrb_ldf %>% filter(Data_type %in% c("landform_nt"))
l_df_of_Class<-wrb_ldf %>% filter(Data_type %in% c("landform_of"))

#------------------Soil properties---------------------------------------------#

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


# # load data
# af<-read_xlsx("./Next_Article/output/mean_ci/AF_clim/AF_mean_ci_plot_data.xlsx")%>%
#                          as.data.frame()
# cc<-read_xlsx("./Next_Article/output/mean_ci/CC_clim/CC_mean_ci_plot_data.xlsx") %>%
#                          as.data.frame()
# nt<-read_xlsx("./Next_Article/output/mean_ci/NT_clim/NT_mean_ci_plot_data.xlsx") %>%
#                          as.data.frame()
# of<-read_xlsx("./Next_Article/output/mean_ci/OF_clim/OF_mean_ci_plot_data.xlsx") %>%
#                          as.data.frame()

# Get soil properties data

#--------@pH
df_af_pH1 <- af %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("acidic","neutral","alkaline")) %>%
                         as.data.frame()

df_af_pH2<-data.frame(cat = c("alkaline","neutral","acidic"),
                           cat2 = NA, mean = NA, 
                           ci_low=NA,ci_high=NA)

df_af_pH<-full_join(df_af_pH1,df_af_pH2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(3,2,1)
df_af_pH

df_cc_pH <- cc %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("acidic","neutral","alkaline")) %>%
                         as.data.frame() 

df_nt_pH <- nt %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("acidic","neutral","alkaline")) %>%
                         as.data.frame()

df_of_pH <- of %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("acidic","neutral","alkaline")) %>%
                         as.data.frame()
df_cc_pH
df_nt_pH

#--------@ SOC
df_af_soc<- af %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<5","5-10",">10")) %>%
                         as.data.frame()

df_cc_soc<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<5","5-10",">10")) %>%
                         as.data.frame()

df_nt_soc<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<5","5-10",">10")) %>%
                         as.data.frame()

df_of_soc<- of %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<5","5-10",">10")) %>%
                         as.data.frame()

#--------@ p
  
df_af_p<- af %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<10.9","10.9-21.4",">21.4")) %>%
                         as.data.frame()

df_cc_p<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<10.9","10.9-21.4",">21.4")) %>%
                         as.data.frame()

df_nt_p<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<10.9","10.9-21.4",">21.4")) %>%
                         as.data.frame()

df_of_p<- of %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<10.9","10.9-21.4",">21.4")) %>%
                         as.data.frame()

#--------@ BD
df_af_bd<- af %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<1.20","1.20-1.47",">1.47")) %>%
                         as.data.frame()

df_cc_bd<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<1.20","1.20-1.47",">1.47")) %>%
                         as.data.frame()

df_nt_bd<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<1.20","1.20-1.47",">1.47")) %>%
                         as.data.frame()

df_of_bd<- of %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<1.20","1.20-1.47",">1.47")) %>%
                         as.data.frame()

#--------@ Texture
df_af_tex<- af %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("fine","medium","coarse")) %>%
                         as.data.frame()

df_cc_tex<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("fine","medium","coarse")) %>%
                         as.data.frame()

df_nt_tex<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("fine","medium","coarse")) %>%
                         as.data.frame()

df_of_tex<- of %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("fine","medium","coarse")) %>%
                         as.data.frame()

#--------@ DEM 
df_af_dem<- af %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<250","250-1000",">1000")) %>%
                         as.data.frame()

df_cc_dem<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<250","250-1000",">1000")) %>%
                         as.data.frame()

df_nt_dem<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<250","250-1000",">1000")) %>%
                         as.data.frame()

df_of_dem<- of %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<250","250-1000",">1000")) %>%
                         as.data.frame()

#--------@ Slope  
df_af_slope1<- af %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.20","0.2-1","1-5",
                                           "5-15",">15"))

df_af_slope2<-data.frame(cat = c("<0.20","0.2-1","1-5",
                                           "5-15",">15"),
                           cat2 = NA, mean = NA, 
                           ci_low=NA,ci_high=NA)

df_af_slope<-full_join(df_af_slope1,df_af_slope2) %>%
distinct(cat,.keep_all = TRUE) %>%
slice(5,1,2,3,4)
df_af_slope

df_cc_slope<- cc %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.20","0.2-1","1-5",
                                           "5-15",">15"))  %>% as.data.frame()

df_nt_slope<- nt %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.20","0.2-1","1-5",
                                           "5-15",">15"))  %>% as.data.frame()

df_of_slope<- of %>% select(cat,mean,ci_low,ci_high) %>%
                         filter(cat %in% c("<0.20","0.2-1","1-5",
                                           "5-15",">15"))  %>% as.data.frame()

# Plotting to get the figures pH soc p bd tex dem slope

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
  scale_y_continuous(limits = c(-40, 80))+
  scale_x_discrete(labels=data[,1])+
  tr_theme1 +
  #facet_wrap(~cov)+
  coord_flip()
}

p1<-gg_plot(df_af_pH)+ theme(axis.text.x=element_blank())
p2<-gg_plot(df_cc_pH)+ theme(axis.text=element_blank())
p3<-gg_plot(df_nt_pH)+ theme(axis.text=element_blank())
p4<-gg_plot(df_of_pH)+ theme(axis.text=element_blank())

p5<-gg_plot(df_af_soc)+ theme(axis.text.x=element_blank())
p6<-gg_plot(df_cc_soc)+ theme(axis.text=element_blank())
p7<-gg_plot(df_nt_soc)+ theme(axis.text=element_blank())
p8<-gg_plot(df_of_soc)+ theme(axis.text=element_blank())

p9<-gg_plot(df_af_p)+ theme(axis.text.x=element_blank())
p10<-gg_plot(df_cc_p)+ theme(axis.text=element_blank())
p11<-gg_plot(df_nt_p)+ theme(axis.text=element_blank())
p12<-gg_plot(df_of_p)+ theme(axis.text=element_blank())

p13<-gg_plot(df_af_bd)+ theme(axis.text.x=element_blank())
p14<-gg_plot(df_cc_bd)+ theme(axis.text=element_blank())
p15<-gg_plot(df_nt_bd)+ theme(axis.text=element_blank())
p16<-gg_plot(df_of_bd)+ theme(axis.text=element_blank())

p17<-gg_plot(df_af_tex)+ theme(axis.text.x=element_blank())
p18<-gg_plot(df_cc_tex)+ theme(axis.text=element_blank())
p19<-gg_plot(df_nt_tex)+ theme(axis.text=element_blank())
p20<-gg_plot(df_of_tex)+ theme(axis.text=element_blank())

p21<-gg_plot(df_af_dem)+ theme(axis.text.x=element_blank())
p22<-gg_plot(df_cc_dem)+ theme(axis.text=element_blank())
p23<-gg_plot(df_nt_dem)+ theme(axis.text=element_blank())
p24<-gg_plot(df_of_dem)+ theme(axis.text=element_blank())

df_af_slope<-as.data.frame(df_af_slope)
p25<-gg_plot(df_af_slope)+ theme(axis.text.x =element_blank())
p26<-gg_plot(df_cc_slope)+ theme(axis.text=element_blank())
p27<-gg_plot(df_nt_slope)+ theme(axis.text=element_blank())
p28<-gg_plot(df_of_slope)+ theme(axis.text=element_blank())

# make the plots--------wrd
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
  scale_y_continuous(limits = c(-20, 30))+
  scale_x_discrete(labels=rev(data[,1]))+
  tr_theme1 +
  #facet_wrap(~cov)+
  coord_flip()
}

wrb_af<-gg_plot1(df_af_wrb) + scale_x_discrete(labels=rev(df_af_wrb[,1])) +theme(axis.text.x=element_blank())
wrb_cc<-gg_plot1(df_cc_wrb) + scale_x_discrete(labels=rev(df_cc_wrb[,1])) +theme(axis.text=element_blank())
wrb_nt<-gg_plot1(df_nt_wrb) + scale_x_discrete(labels=rev(df_nt_wrb[,1]))+theme(axis.text=element_blank())
wrb_of<-gg_plot1(df_of_wrb) + scale_x_discrete(labels=rev(df_of_wrb[,1]))+theme(axis.text=element_blank())


# make the plots--------LANDFORM
lf_af<-gg_plot1(l_df_af_Class) + scale_x_discrete(labels=rev(l_df_af_Class[,1]))
lf_cc<-gg_plot1(l_df_cc_Class) + scale_x_discrete(labels=rev(l_df_cc_Class[,1]))+theme(axis.text.y=element_blank())
lf_nt<-gg_plot1(l_df_nt_Class) + scale_x_discrete(labels=rev(l_df_nt_Class[,1]))+theme(axis.text.y=element_blank())
lf_of<-gg_plot1(l_df_of_Class) + scale_x_discrete(labels=rev(l_df_of_Class[,1]))+theme(axis.text.y=element_blank())


# climate maps

new_b<- p13+p14+p15+p16+
        p9+p10+p11+p12 + 
        p1+p2+p3+p4+
        p5+p6+p7+p8+
        p17+p18+p19+p20+
        p21+p22+p23+p24+
        p25+p26+p27+p28+
        wrb_af+wrb_cc+wrb_nt+wrb_of+ # soil type
        lf_af+lf_cc+lf_nt+lf_of+ # landform
        plot_layout(
     heights = c(
          0.20,
          0.20,
          0.20,
          0.20,
          0.20,
          0.20,
          0.25,
          1,
          0.90),
          ncol=4)
new_b

ggsave(new_b,filename = "./output/graphs/Fig_4.png",
       width = 30,height = 50,
       dpi = 300, units = "cm")

ggsave(new_b,filename = "./output/graphs/Fig_4.pdf",
       width = 30,height = 50,
       dpi = 300, units = "cm")

