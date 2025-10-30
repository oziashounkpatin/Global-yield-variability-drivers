
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
af1<-read_xlsx("./output/mean_ci/AF_clim/arng_AF_Arid_mean_ci.xlsx") %>% 
  dplyr::select(cat,cat1,mean,ci_low,ci_high) %>%
  add_column(key="AF")

cc1<-read_xlsx("./output/mean_ci/CC_clim/arng_CC_Arid_mean_ci.xlsx") %>%
  dplyr::select(cat,cat1,mean,ci_low,ci_high)%>%
  add_column(key="CC")

nt1<-read_xlsx("./output/mean_ci/NT_clim/arng_NT_Arid_mean_ci.xlsx") %>% 
  dplyr::select(cat,cat1,mean,ci_low,ci_high)%>%
  add_column(key="NT")

of1<-read_xlsx("./output/mean_ci/OF_clim/arng_OF_Arid_mean_ci.xlsx") %>%
  dplyr::select(cat,cat1,mean,ci_low,ci_high)%>%
  add_column(key="OF")

all_df1<-bind_rows(af1,cc1,nt1,of1)

all_df2<- all_df1 %>% dplyr::mutate(cat3=case_when(cat== "GDD_maize1" ~ "<0.8",
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
                                          cat== "GDD_wheat5" ~ "6-10"
                                              ))

# Finds the first non-missing value at each position
all_df<-all_df2 %>% mutate(newClass = coalesce(cat,cat3)) %>%
                    dplyr::select(key,cat,newClass,cat1,mean,ci_low,ci_high) 
                    #rename(cat=2) %>% as.data.frame() %>%)

af<-all_df %>% filter(key %in% c("AF")) %>% dplyr::select(key:ci_high)
cc<-all_df %>% filter(key %in% c("CC")) %>% dplyr::select(key:ci_high)
nt<-all_df %>% filter(key %in% c("NT")) %>% dplyr::select(key:ci_high)
of<-all_df %>% filter(key %in% c("OF")) %>% dplyr::select(key:ci_high)

# Create the empty moderator data frame
df_gdd0<-data.frame(cat1 = c("<0.8","0.8-2.7","2.7-4","4-6","6-10"), 
                           cat11 = NA, mean = NA, ci_low=NA,ci_high=NA)

# Extract gdd data from df
df_af_gdd1<- af %>% filter(cat1 %in% c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10")) %>% as.data.frame()

df_cc_gdd1<- cc %>% filter(cat1 %in% c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10")) %>% as.data.frame()

df_nt_gdd1<- nt %>% filter(cat1 %in% c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10")) %>% as.data.frame()

df_of_gdd1<- of %>% filter(cat1 %in% c("<0.8","0.8-2.7",
                                     "2.7-4","4-6",
                                     "6-10")) %>% as.data.frame()

# df_af_gdd<-full_join(df_af_gdd1,df_gdd0) %>%
#             #distinct(cat1,.keep_all = TRUE) %>%
#             #slice(1,5,4,3,2) %>%
#             add_column(mgt="AF") 
# 
# df_cc_gdd<-full_join(df_cc_gdd1,df_gdd0) %>%
#             #distinct(cat1,.keep_all = TRUE) %>%
#             #slice(4,3,2,1,5) %>%
#             add_column(mgt="CC") 
# 
# df_nt_gdd<-full_join(df_nt_gdd1,df_gdd0) %>%
#              #distinct(cat1,.keep_all = TRUE) %>%
#              #slice(1,5,4,3,2) %>%
#             add_column(mgt="NT") 
# 
# df_of_gdd<-full_join(df_of_gdd1,df_gdd0) %>%
#             #distinct(cat1,.keep_all = TRUE) %>%
#             #slice(1,2,3,5,4)%>%
#             add_column(mgt="OF") 

# make plots

# The palette with black:
cbbPalette <- c("#009E73", "#E69F00","#56B4E9","#999999",  "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7","#000000")

tr_theme1<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = "none",
         axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1),
         axis.text = element_text(size = 27),
         axis.x.title=element_blank(),
         axis.title= element_text(size=27),
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
         axis.x.title=element_blank(),
         axis.title= element_text(size=27),
         theme(strip.text = element_text(size = 30)),
         strip.text = element_text(size=25, color = 'black'),
         strip.background = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
       )

comb_gdd<-bind_rows(df_af_gdd1,df_cc_gdd1,df_nt_gdd1,df_of_gdd1)%>%
          add_column(crops=c("Maize", 
                             "Wheat","Wheat","Wheat",
                             "Maize","Maize",
                              "Wheat","Wheat","Wheat","Wheat",
                             "Soybean","Soybean","Soybean","Soybean","Soybean",
                             "Rice","Rice","Rice","Rice",
                             "Maize","Maize","Maize","Maize","Maize","Wheat",
                             "Maize","Maize")) %>%
        dplyr::select(crops, everything())

comb_gdd_id<- comb_gdd %>% mutate(ID=paste(key,crops, sep=":"))

  

plt<-comb_gdd_id %>% dplyr::arrange(mean) %>%
  mutate(cat1 = factor(cat1, levels= c(
"6-10",     
"4-6",       
"2.7-4",      
"0.8-2.7",    
"<0.8"
  ))) %>%
  ggplot(aes(x=cat1,y= mean,group=key)) +
  geom_point( size=2) +
    coord_flip()+
  geom_errorbar(aes(x = cat1, ymin = ci_low, ymax = ci_high),
                width=0.3,size=0.80) +
  geom_hline(yintercept = 0, linetype="dotted",
                color = "red", linewidth=0.90) +
   #scale_color_manual(values=cbbPalette)+
   scale_y_continuous(limits = c(-0.3, 1))+
   ylab("Effect size")+ xlab("")+
   facet_wrap(~ID, ncol = 3)+
  #scale_x_discrete(labels=cat1)+
  coord_flip()+
  labs(color="RAP") #+
   # geom_vline(xintercept=c(3.5,8.5,11.5,14.5,17.5,20.5,23.5,
   #                         28.5,33.5,40.5),linewidth=0.25)

plt

plt
plt_arid<- plt + tr_theme1 
plt_arid_legend<- plt + tr_theme2


plt_arid
plt_arid_legend
unique(comb_gdd_id$ID)


# save the maps
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
ggsave(plt_arid,filename = "./output/graphs/recap/arid/arid_gdd.png",
       width = 30,height = 18.5,
       dpi = 300, units = "cm")
 
ggsave(plt_arid,filename = "./output/graphs/recap/arid/arid_gdd.pdf",
       width = 45,height = 55,
       dpi = 300, units = "cm")
 