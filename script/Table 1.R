
library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(magrittr)
library(stringr)

#load data
df1<-read_xlsx("./Next_Article/input/all_dis_cov.xlsx",guess_max = 1000) %>%
    #filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))
    filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))%>%
    dplyr::select(key,Country,Author,effectSize,Crop_Group,kg_clim,aridity,
                  pH_class,aridity_class,Cat)%>%
                  drop_na(Crop_Group)

df<-read_xlsx("./Next_Article/input/all_dis_cov.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))%>%
    dplyr::select(key,Country,Author,effectSize,Crop_Group,kg_clim,aridity,
                  pH_class,aridity_class,Cat)%>%
    drop_na(Crop_Group)


obs<-df1 %>% dplyr::select(Crop_Group) %>% 
          group_by(Crop_Group) %>% 
          summarise(n = n()) %>% 
          drop_na()

ari<-df %>% dplyr::select(Crop_Group,aridity) %>% 
          group_by(Crop_Group) %>% 
          dplyr::summarize(Aridity_index= mean(aridity),sd_ar= sd(aridity)) %>% 
          drop_na()

# ari<-df %>% dplyr::select(aridity) %>% 
#           #group_by(Crop_Group) %>% 
#           dplyr::summarize(Aridity_index= mean(aridity),sd_ar= sd(aridity)) %>% 
#           drop_na()

studies<-df1 %>% dplyr::select(Crop_Group,Author) %>% 
          group_by(Crop_Group) %>% 
          summarise_all(list(distinct=~length(unique(.))))%>% 
          dplyr::rename(Studies=distinct) %>% 
          drop_na()

# a<-unique(df1$Author)
# length(a)
# 
# b<-as.factor(df1$kg_clim)
# summary(b)

magt<-  df1 %>% dplyr::select(Crop_Group,key) %>%
          rename(mag=key) %>%
        gather(mag, value,-Crop_Group) %>%
        group_by(Crop_Group,value) %>%
        tally %>% 
        spread(value, n, fill = 0) %>% 
        drop_na()


zone_clim<-df1 %>% dplyr::select(Crop_Group,kg_clim) %>%
          #rename(mag=key) %>%
         gather(kg_clim, value,-Crop_Group) %>%
         group_by(Crop_Group,value) %>%
         tally %>% 
         spread(value, n, fill = 0) %>% 
        drop_na()

# pH_cl<-df1 %>% dplyr::select(Crop_Group,pH_class) %>%
#           #rename(mag=key) %>%
#          gather(pH_class, value,-Crop_Group) %>%
#          group_by(Crop_Group,value) %>%
#          tally %>% 
#          spread(value, n, fill = 0) %>% 
#          drop_na()
# 
# aridity_cl<-df1 %>% dplyr::select(Crop_Group,aridity_class) %>%
#           #rename(mag=key) %>%
#          gather(aridity_class, value,-Crop_Group) %>%
#          group_by(Crop_Group,value) %>%
#          tally %>% 
#          spread(value, n, fill = 0) %>% 
#          drop_na()
# 
# cat_cl<-df1 %>% dplyr::select(Crop_Group,Cat) %>%
#           #rename(mag=key) %>%
#          gather(Cat, value,-Crop_Group) %>%
#          group_by(Crop_Group,value) %>%
#          tally %>% 
#          spread(value, n, fill = 0)
#          drop_na()

tab_all<-bind_cols(obs,studies[,2],magt[,2:5],zone_clim[,2:5],ari[,2:3])

write_xlsx(tab_all,"./Next_Article/output/table/1_table.xlsx")


summary_table %>% 
  mutate(delay = str_glue("{delay_mean} ({delay_sd})")) %>%  # combine and format other values
  select(-c(delay_mean, delay_sd)) %>%                       # remove two old columns   
  adorn_totals(where = "row") %>%                            # add total row
  select(                                                    # order and rename cols
    "Hospital Name"   = hospital,
    "Cases"           = cases,
    "Max delay"       = delay_max,
    "Mean (sd)"       = delay,
    "Delay 3+ days"   = delay_3,
    "% delay 3+ days" = pct_delay_3
    )

linelist %>% 
  group_by(outcome) %>% 
  summarise(across(.cols = c(age_years, temp, wt_kg, ht_cm),  # columns
                   .fns = mean,                               # function
                   na.rm=T))                                  # extra arguments
