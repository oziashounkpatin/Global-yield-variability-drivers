
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


# Get data per key
af1<-read_xlsx("./output/mean_ci/AF_clim/arng_AF_Continental_mean_ci_sliced.xlsx") %>% 
  dplyr::select(cov,cat,cat1,mean,ci_low,ci_high) %>%
  add_column(key="AF")

cc1<-read_xlsx("./output/mean_ci/CC_clim/arng_CC_Continental_mean_ci_sliced.xlsx") %>%
  dplyr::select(cov,cat,cat1,mean,ci_low,ci_high)%>%
  add_column(key="CC")

nt1<-read_xlsx("./output/mean_ci/NT_clim/arng_NT_Continental_mean_ci_sliced.xlsx") %>% 
  dplyr::select(cov,cat,cat1,mean,ci_low,ci_high)%>%
  add_column(key="NT")

of1<-read_xlsx("./output/mean_ci/OF_clim/arng_OF_Continental_mean_ci_sliced.xlsx") %>%
  dplyr::select(cov,cat,cat1,mean,ci_low,ci_high)%>%
  add_column(key="OF")

all_df1<-bind_rows(af1,cc1,nt1,of1) %>% filter(str_detect(cov, 'GDD')) %>% as.data.frame()
all_df1

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
                                          TRUE ~ cat )) # Keep other values unchanged

# Finds the first non-missing value at each position
all_df<-all_df2 %>% dplyr::select(key,cov,cat,mean,ci_low,ci_high) 
af<-all_df %>% filter(key %in% c("AF"))
cc<-all_df %>% filter(key %in% c("CC"))
nt<-all_df %>% filter(key %in% c("NT")) 
of<-all_df %>% filter(key %in% c("OF"))

# Create the empty moderator data frame

#------------@ Prepare data vector for all gdd
cat_maize<-c("maize","maize","maize","maize","maize")
cat_rice<-c("rice","rice","rice","rice","rice")
cat_soybean<-c("soybean","soybean","soybean","soybean","soybean")
cat_wheat<-c("wheat","wheat","wheat","wheat","wheat")

df_gdd0_maize<-data.frame(cov=cat_maize,
                    cat = c("<0.8","0.8-2.7","2.7-4","4-6","6-10"), 
                    cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_gdd0_rice<-data.frame(cov=cat_rice,
                    cat = c("<0.8","0.8-2.7","2.7-4","4-6","6-10"), 
                    cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_gdd0_soybean<-data.frame(cov=cat_soybean,
                    cat = c("<0.8","0.8-2.7","2.7-4","4-6","6-10"), 
                    cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

df_gdd0_wheat<-data.frame(cov=cat_wheat,
                    cat = c("<0.8","0.8-2.7","2.7-4","4-6","6-10"), 
                    cat1 = NA, mean = NA, ci_low=NA,ci_high=NA)

#-----------------@ maize @----------------------------------------------------#
df_af_maize<-af %>% filter(str_detect(cov, 'maize'))
af_gdd_maize<-full_join(df_af_maize,df_gdd0_maize) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="AF") 

df_cc_maize<-cc %>% filter(str_detect(cov, 'maize'))
cc_gdd_maize<-full_join(df_cc_maize,df_gdd0_maize) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="CC") 

df_nt_maize<-nt %>% filter(str_detect(cov, 'maize'))
nt_gdd_maize<-full_join(df_nt_maize,df_gdd0_maize) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="NT") 

df_of_maize<-of %>% filter(str_detect(cov, 'maize'))
of_gdd_maize<-full_join(df_of_maize,df_gdd0_maize) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="OF") 

comb_gdd_maize<-bind_rows(af_gdd_maize,cc_gdd_maize,
                          nt_gdd_maize,of_gdd_maize)



#-----------------@ rice @----------------------------------------------------#
df_af_rice<-af %>% filter(str_detect(cov, 'rice'))
af_gdd_rice<-full_join(df_af_rice,df_gdd0_rice) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="AF") 

df_cc_rice<-cc %>% filter(str_detect(cov, 'rice'))
cc_gdd_rice<-full_join(df_cc_rice,df_gdd0_rice) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="CC") 

df_nt_rice<-nt %>% filter(str_detect(cov, 'rice'))
nt_gdd_rice<-full_join(df_nt_rice,df_gdd0_rice) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="NT") 

df_of_rice<-of %>% filter(str_detect(cov, 'rice'))
of_gdd_rice<-full_join(df_of_rice,df_gdd0_rice) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="OF") 

comb_gdd_rice<-bind_rows(af_gdd_rice,cc_gdd_rice,
                          nt_gdd_rice,of_gdd_rice)

#-----------------@ soybean @----------------------------------------------------#
df_af_soybean<-af %>% filter(str_detect(cov, 'soybean'))
af_gdd_soybean<-full_join(df_af_soybean,df_gdd0_soybean) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="AF") 

df_cc_soybean<-cc %>% filter(str_detect(cov, 'soybean'))
cc_gdd_soybean<-full_join(df_cc_soybean,df_gdd0_soybean) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="CC") 

df_nt_soybean<-nt %>% filter(str_detect(cov, 'soybean'))
nt_gdd_soybean<-full_join(df_nt_soybean,df_gdd0_soybean) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="NT") 

df_of_soybean<-of %>% filter(str_detect(cov, 'soybean'))
of_gdd_soybean<-full_join(df_of_soybean,df_gdd0_soybean) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="OF") 

comb_gdd_soybean<-bind_rows(af_gdd_soybean,cc_gdd_soybean,
                          nt_gdd_soybean,of_gdd_soybean)

#-----------------@ wheat @----------------------------------------------------#
df_af_wheat<-af %>% filter(str_detect(cov, 'wheat'))
af_gdd_wheat<-full_join(df_af_wheat,df_gdd0_wheat) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="AF") 

df_cc_wheat<-cc %>% filter(str_detect(cov, 'wheat'))
cc_gdd_wheat<-full_join(df_cc_wheat,df_gdd0_wheat) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="CC") 

df_nt_wheat<-nt %>% filter(str_detect(cov, 'wheat'))
nt_gdd_wheat<-full_join(df_nt_wheat,df_gdd0_wheat) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="NT") 

df_of_wheat<-of %>% filter(str_detect(cov, 'wheat'))
of_gdd_wheat<-full_join(df_of_wheat,df_gdd0_wheat) %>%
            distinct(cat,.keep_all = TRUE) %>%
            #slice(5,1,2,6,7,8,3,4) %>%
            add_column(mgt="OF") 

comb_gdd_wheat<-bind_rows(af_gdd_wheat,cc_gdd_wheat,
                          nt_gdd_wheat,of_gdd_wheat)

# bind all
gdd_Continental<-bind_rows(comb_gdd_maize,comb_gdd_rice,
                    comb_gdd_soybean,comb_gdd_wheat)

gdd_Continental <- gdd_Continental %>% dplyr::mutate(cov=case_when(cov== "GDD_maize" ~ "maize",
                                                     cov== "GDD_rice" ~ "rice",
                                                     cov== "GDD_soybean" ~ "soybean",
                                                     cov== "GDD_wheat" ~ "wheat",
                                                     TRUE ~ cov))

# Plot the GDD
plt1<-gdd_Continental %>% dplyr::arrange(mean) %>%
  mutate(cat = factor(cat, levels= c(
"6-10",
"4-6",
"2.7-4",
"0.8-2.7",
"<0.8"
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
   #scale_y_continuous(limits = c(-100, 100))+
   facet_wrap(cov~mgt, ncol = 4)
  
plt1
plt_gdd_Continental<- plt1 + tr_theme1 +theme(strip.text.x = element_blank())
plt_gdd_Continental

# save the maps
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
ggsave(plt_gdd_Continental,filename = "./output/graphs/recap/Continental/gdd_Continental_all_RAP1.png",
       width = 35,height = 25,
       dpi = 300, units = "cm")
 
ggsave(plt_gdd_Continental,filename = "./output/graphs/recap/Continental/gdd_Continental_all_RAP.pdf",
       width = 35,height = 25,
       dpi = 300, units = "cm")