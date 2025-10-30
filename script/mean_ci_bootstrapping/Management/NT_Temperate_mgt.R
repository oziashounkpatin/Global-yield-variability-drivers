
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

# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# VIP: https://www.w3online.net/article/learn_method__plotting_the_confidence_intervals_using_plotci()_function_2

# Convert ES to perc
perc<- function(data){
  data_conv=100*(exp(data) - 1)
  return(data_conv)
}

df_Temperate<-read_xlsx("./input/data/all_dis_cov2.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"),kg_clim %in% c("Temperate"))#%>%
    #dplyr::select(effectSize,key,Crop_Group)
# 
# write_xlsx(df_joh,"./input/dataset_Ozias.xlsx")

df<-read_xlsx("./input/data/all_dis_cov2.xlsx",guess_max = 1000) %>%
    filter(key %in% c("NT"), !Crop_Group %in% c("Grass"),kg_clim %in% c("Temperate"))%>%
    dplyr::select(effectSize,key,Crop_Group,N_input,soil_cover,
                  weed_control,rotation) %>%
                  drop_na(Crop_Group) #%>%
     # mutate(ES=perc(effectSize)) %>%
     # filter(!ES > 100) %>%
     # select(!effectSize)  %>%
     # dplyr::rename(effectSize=ES)

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
         #axis.ticks = element_blank(),
         #axis.text = element_blank(),
         #axis.line = element_blank(),
         #axis.title=element_blank()
         #panel.border = element_rect(colour = "black", fill=NA, size=5)
       )

# draw plot 
hist_nt<-ggplot(df, aes(x = effectSize)) + 
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


soil_man <- df %>% mutate_at(c("N_input", "soil_cover", "weed_control", 
                                   "rotation"),as.factor) %>% 
            mutate(N_input=recode(N_input, No="no", Yes="yes")) %>% 
            mutate(soil_cover=recode(soil_cover, No="no", Yes="yes")) %>% 
            mutate(weed_control=recode(weed_control, No="no", Yes="yes")) %>% 
            mutate(rotation=recode(rotation, No="no", Yes="yes"))


levels(soil_man$N_input)
levels(soil_man$soil_cover)
levels(soil_man$weed_control)
levels(soil_man$rotation)
levels(soil_man$key)

# Create overall treatment ID

soil_man_id <- soil_man %>% 
  mutate(ID = case_when(N_input=="yes" & soil_cover=="no" &
                              weed_control=="no" & rotation=="no" ~ "only N_input",
                            
                        N_input=="no" & soil_cover=="yes" &
                          weed_control=="no" & rotation=="no" ~ "only soil_cover",
                        
                        N_input=="no" & soil_cover=="no" &
                          weed_control=="yes" & rotation=="no" ~ "only weed_control",
                        
                        N_input=="no" & soil_cover=="no" &
                          weed_control=="no" & rotation=="yes" ~ "only rotation",
                        
                        N_input=="yes" & soil_cover=="yes" &
                        weed_control=="no" & rotation=="no" ~ "N_input & soil_cover",
                        
                        N_input=="yes" & soil_cover=="no" &
                       weed_control=="yes" & rotation=="no" ~ "N_input & weed_control",
                        
                        N_input=="yes" & soil_cover=="no" &
                        weed_control=="no" & rotation=="yes" ~ "N_input & rotation",
                      
                        N_input=="no" & soil_cover=="yes" &
                        weed_control=="yes" & rotation=="no" ~ "soil_cover & weed_control",
                        
                        N_input=="no" & soil_cover=="yes" &
                        weed_control=="no" & rotation=="yes" ~ "soil_cover & rotation",
                     
                        N_input=="no" & soil_cover=="no" &
                        weed_control=="yes" & rotation=="yes" ~ "weed_control & rotation",
              
                        N_input=="yes" & soil_cover=="yes" &
                        weed_control=="yes" & rotation=="no" ~ "N_input & soil_cover & weed_control",
                        
                        
                        N_input=="yes" & soil_cover=="no" &
                        weed_control=="yes" & rotation=="yes" ~ "N_input & weed_control & rotation",
                        
                        
                        N_input=="no" & soil_cover=="yes" &
                        weed_control=="yes" & rotation=="yes" ~ "soil_cover & weed_control & rotation",

                        
                        N_input=="yes" & soil_cover=="yes" &
                          weed_control=="yes" & rotation=="yes" ~ "N_input & soil_cover & weed_control & rotation"
                        )) %>%
                       drop_na(ID)

# Get long format data
soil_long <- melt(soil_man_id, id.vars = c("ID","key"),
                measure.vars = c("effectSize"))

soil_long<-soil_long %>% filter(value <= 2.5)

##########################################################
#                    NT                          
#---------------------------------------------------------

soil_long_nt <- soil_long %>% filter(key %in% c("NT")) %>% 
                filter(!ID %in% c("only N_input","only rotation",
                                  "soil_cover & rotation",
                                  "weed_control & rotation"))
head(soil_long_nt)

#---@ overall
bt_all<-es_boot(soil_long_nt$value)
bt_all<-as.data.frame(rbind(bt_all,bt_all))
# colnames(bt_all)<-c("ID", "mean","ci_low","ci_high")
bt_all<-as.data.frame(bt_all) %>%
        rownames_to_column(var = "ID") %>%
        dplyr::rename(mean=2, ci_low=3,ci_high=4) %>%
        slice_head()

#---@ for management
li_Temperate_class<-split(soil_long_nt$value,soil_long_nt$ID)

bt_Temperate_class<-as.data.frame(t(map_df(li_Temperate_class, es_boot))) %>%
   rownames_to_column(var = "ID") %>%
   dplyr::rename(mean=2, ci_low=3,ci_high=4)

df_Temperate<-bind_rows(bt_all,bt_Temperate_class) 
df_Temperate$cat<-"Temperate"

df_Temperate$ID<-paste(df_Temperate$cat,"&",df_Temperate$ID)
df_Temperate_arg<- df_Temperate %>% slice(1,4,3,6,9,2,5,8,7)

# Compute percent increase
df_Temperate_perc<-df_Temperate_arg %>% 
     mutate(mean1=perc(mean), cil=perc(ci_low),cih=perc(ci_high))

# count all obs 
all_n<-soil_long_nt %>% dplyr::summarise(total_count=n())
all_n$ID1<-"overall"
all_n<- all_n %>% dplyr::select(ID1,total_count)

# count per management
df_grp<-soil_long_nt %>% group_by(ID) %>% 
              dplyr::summarise(total_count=n()) %>%
              slice(3,2,5,8,1,4,7,6) %>% 
              dplyr::rename(ID1=ID)
df_Temperate
df_grp

# bind count
count_n<-bind_rows(all_n,df_grp)

#bind all
df_Temperate_n<-bind_cols(count_n,df_Temperate_perc)

df_Temperate_n

# save the data and plots
write_xlsx(df_Temperate_n,"./output/mean_ci/NT_clim/NT_Temperate_management.xlsx")                 
