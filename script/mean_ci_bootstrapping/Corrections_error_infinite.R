summary(factor(df$soc_class))
df_soc<-df %>% filter(!soc_class %in% c("2"))
summary(factor(df_soc$soc_class))
li_soc_class<-split(df$effectSize,df_soc$soc_class)


n_all <- df %>% count() %>%
  dplyr::summarise(numbers=n()) %>%
  add_column(Type_Cat="overall") %>%
  dplyr::select(2,1) %>%
  as.data.frame()

df_count<- df %>% select(3,9:20)

n_df<- as.data.frame(unlist(apply(df_count, 2 , table))) %>% 
  rownames_to_column(var = "Type_Cat") %>%
  dplyr::rename(numbers=2) 


df_gdd_maize<- final_df %>% select(cat,cat2,mean,ci_low,ci_high) %>%
                      filter(cat %in% c("GDD_maize1","GDD_maize2","GDD_maize3",
                                        "GDD_maize4","GDD_maize5",
                                        "GDD_rice1","GDD_rice2","GDD_rice3",
                                        "GDD_rice4","GDD_rice5",
                                        "GDD_soybean1","GDD_soybean2","GDD_soybean3",
                                        "GDD_soybean4","GDD_soybean5",
                                        "GDD_wheat1","GDD_wheat2","GDD_wheat3",
                                        "GDD_wheat4","GDD_wheat5"))
a<-p1+p2+p3+p4+p44+p7+p13+p19+p5+p8+p14+p20+ #12 plots
  plot_layout(widths = c(1, 1),  ncol = 2)
a

b<-p6+p9+p15+p21+p10+p16+p11+p17+p12+p18+
  plot_layout(widths = c(1, 1),  ncol = 2,nrow = 6)
b

ggsave(a,filename = "./Next_Article/output/graphs/all_clim/aplot.png",
       width = 17, height = 25,dpi = 300, units = "cm")

ggsave(b,filename = "./Next_Article/output/graphs/all_clim/bplot.png",
       width = 17, height = 25,dpi = 300, units = "cm")

c<-p1+p44+p5+p6+
   plot_layout(widths = c(1, 1),  ncol = 1,nrow = 6)
c
ggsave(c,filename = "./Next_Article/output/graphs/all_clim/cplot.png",
       width = 12, height = 25,dpi = 300, units = "cm")

d<-p2+p7+p8+p9+p10+p11+p12+
   plot_layout(widths = c(1, 1),  ncol = 1,nrow = 7)
d

ggsave(d,filename = "./Next_Article/output/graphs/all_clim/dplot.png",
       width = 17, height = 25,dpi = 300, units = "cm")


# af_of_clim<-p4 + p19 + p5 + p20 + p6 + p21+
#   plot_layout(widths = c(1, 1),  nrow = 3)

# 1-4 crops
# 44-6
# 7-12
# 13-18
# 19-20

nt_cc_clim1<-p7+p13+p8+p14+p9+p15+
  plot_layout(widths = c(1, 1),  nrow = 3)

nt_cc_clim1

nt_cc_clim2<-p10+p16+p11+p17+p12+p18+
 plot_layout(widths = c(1, 1),  nrow = 3)

nt_cc_clim2

# save maps
ggsave(af_of_clim,filename = "./Next_Article/output/graphs/all_clim/af_of_cov_clim.png",
       width = 13, height = 7, dpi = 300, units = "cm")

ggsave(nt_cc_clim1,filename = "./Next_Article/output/graphs/all_clim/nt_cc_cov_clim1.png",
       width = 13, height = 7, dpi = 300, units = "cm")

ggsave(nt_cc_clim2,filename = "./Next_Article/output/graphs/all_clim/nt_cc_cov_clim2.png",
       width = 13, height = 7, dpi = 300, units = "cm")


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


# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# VIP: https://www.w3online.net/article/learn_method__plotting_the_confidence_intervals_using_plotci()_function_2

df<-read_xlsx("./Next_Article/input/all_dis_cov2.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"),kg_clim %in% c("Arid")) %>%
    dplyr::select(effectSize,key,Crop_Group,N_input,soil_cover,weed_control,rotation,
                 kg_clim,aridity_class,ph_class,soc_class,
                  p_class, bd_class, texture,gdd_maize_class, gdd_wheat_class,gdd_rice_class,
                  gdd_soybean_class, dem_class,slope_class) %>%
                  drop_na(Crop_Group) %>%
    dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others",
      "V_F_others",Crop_Group))


# Get data per key
af<-df %>% filter(key %in% c("AF")) %>% as.data.frame()
cc<-df %>% filter(key %in% c("CC")) %>% as.data.frame()
nt<-df %>% filter(key %in% c("NT")) %>% as.data.frame()
of<-df %>% filter(key %in% c("OF")) %>% as.data.frame()

#

# soil properties

# soil property maps

# soil topograpgy

# soil topography











# # Plot with ggplot
# 
# tr_theme2<-theme(
#          panel.background = element_rect(fill='transparent'),
#          plot.background = element_rect(fill='transparent', color=NA),
#          panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank(),
#          legend.background = element_rect(fill='transparent',color=NA),
#          legend.key=element_blank(),
#          legend.box.background = element_rect(fill='transparent'),
#          axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1),
#          #axis.ticks = element_blank(),
#          #axis.text = element_blank(),
#          #axis.line = element_blank(),
#          axis.title=element_blank(),
#          panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
#        )
# 
# fig1<-df_all %>%
#   arrange(mean) %>%
#   mutate(cat = factor(cat, levels=c(
# "GDD_soybean5",
# "GDD_soybean4",
# "GDD_soybean3",
# "GDD_soybean2",
# "GDD_soybean1",
# "GDD_rice5",
# "GDD_rice4",
# "GDD_rice3",
# "GDD_rice2",
# "GDD_rice1",
# "GDD_wheat5",
# "GDD_wheat4",
# "GDD_wheat3",
# "GDD_wheat2",
# "GDD_maize5",
# "GDD_maize4",
# "GDD_maize3",
# "GDD_maize2",
# "GDD_maize1",
# ">0.65",
# "0.50-0.65",
# "0.20-0.50",
# "0.05-0.20",
# "<0.05",
# "Tropical",
# "Temperate",
# "Continental",
# "Arid",
# ">15",
# "5-15",
# "1-5",
# "0.2-1",
# "<0.20",
# ">1000",
# "250-1000",
# "<250",
# "coarse",
# "medium",
# "fine",
# ">21.4",
# "10.9-21.4",
# "<10.9",
# ">1.47",
# "1.20-1.47",
# "<1.20",
# ">10",
# "5-10",
# "<5",
# "Veg&Fruit and others",
# "Cereal",
# "Cash crop",
# "Wheat",
# "Soybean",
# "Rice",
# "Maize",
# "OF",
# "NT",
# "CC",
# "AF",
# "overall"))) %>%
#   ggplot( aes(x=cat, y=mean)) +
#   geom_point(fill= "#0C7BDC", color= "#0C7BDC", size=1) +
#   #geom_line(aes(group = 1), color="lightblue",linewidth=3) +
#   geom_errorbar(aes(x = cat, ymin = ci_low, ymax = ci_high),
#                 color= "#0C7BDC",width=0.1,size=0.5) +
#   geom_hline(yintercept = 0, linetype="dotted", 
#                 color = "red", linewidth=1)+
#   geom_vline(xintercept=c(5.5,10.5,14.5,19.5,24.5,28.5,
#                           33.5,36.5,39.5,42.5,45.5,48.5,
#                           55.5,59.5),size=0.25)+
#   tr_theme2 +
#   coord_flip()
# 
# fig1