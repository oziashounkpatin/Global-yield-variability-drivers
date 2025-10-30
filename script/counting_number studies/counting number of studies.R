library(tidyverse)
library(metafor)
library(meta)
library(readxl)
library(writexl

        
# Upload data
dat<-read_xlsx("./input/management/all_dis_cov.xlsx") %>%
            filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% c("Grass"))%>%
            filter(!is.na(effectSize))

nt<-read_xlsx("./input/management/all_dis_cov1.xlsx") %>%
  filter(key %in% c("NT"), !Crop_Group %in% c("Grass"))

dim(nt)

# Number of unique publications
names(dat)
n_pub<-dat %>%  distinct(Location)
dim(n_pub)
