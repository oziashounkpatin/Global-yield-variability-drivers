#rm(list = ls())

## -----------------------------------------------------------------------------
## Libraries
## -----------------------------------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)
library(fuzzyjoin)
library(janitor)
library(sf)
library(terra)
library(soiltexture)
library(mice)
library(stringr)
library(tidyr)
library(purrr)

## -----------------------------------------------------------------------------
## Helper functions
## -----------------------------------------------------------------------------

# Remove spaces in column names
spaceless <- function(x) {
  colnames(x) <- gsub(" ", "_", colnames(x))
  x
}

# Vector of crop labels to drop
crops_to_drop <- c(
  "Average of Durun wheat-sunflower-maize-lupine","Average of potato-wheat-soybean-maize",
  "Average of soybean-wheat-sugarbeet","Average of wheat-maize-soybean","Mixed",
  "Average of wheat-soybean","Average of wheat-sunflower","Average pf wheat-sunflower-maize",
  "Broccoli-cauliflower","Timothy","Winter wheat - silage maize","Average of corn-soybean",
  "Average of grassland, potatto, onion, cereal, or sugar beet",
  "Average of Pea – Pisum sativum L.; durum wheat – Triticum durum Desf.; tomato – Licopersicum esculentum",
  "Average of wheat-oilseed-sugarbeet","Corn/Wheat/Broccoli","Corn-soybean",
  "liquorice and isatis root",
  "Mixed Cropping of beans or soybeans, maize or sorghum, sweet potatoes and cassava",
  "Winter wheat - spring pea - barly","Arable","Beans/Maize","Chickpea alfalfa",
  "Tea and citrus orchard","Wheat alfalfa","Winter wheat - sugar beet - pea",
  "Average of oat Barley","Bean-beet-carrot-chard-cucumber-marrow","Black oat-maize",
  "Snap_bean/Sweetcorn","Corn/soybean/wheat",
  "rotation wheat - maize (6 years); rotation wheat - maize - wheat - sunflower (4 years)",
  "Annual Crop","Average of wheat-barley-sugarbeet-potato",
  "Average of Wheat-sunflower-pigeon pea-Maize",
  "continuous maize (5 years);  rotation wheat - maize (6 years); rotation wheat - maize - wheat - sunflower (4 years)",
  "Rotation Potatoes - Winter rye - Silo maize - Winter rye","Sweetcorn/broccoli","AVG",
  " Average of tomato, safflower, corn, and bean","Average of wheat-barley-pea",
  "Winter_wheat/Sorghum","Average","Bean/Beet/Corn/Bean",
  "rotation corn (Zea Mays L.) - soybean (Glycine max L. Merr.)",
  "Pea/Bean/Tomatto/SweetCorn","Radix glycyrrhizar and Isatis tinctoria",
  " Average of cotton, garlic, melon, tomatto, and onion","Winter_wheat/Crop",
  "Average of barley-wheat-potatto","Corn/soybean","Average of Soybean and corn",
  "continuous tomato (Lycopersicum esculentum Mill) [3 years]; continuous silage corn (Zea mays L.) [2 years]",
  "Tomato/Safflower/Corn/Bean","Wheat and Liquorice","Soybean_Maize","Cereal",
  "Wheat-barley-white clover-oat-turnip-rape","Not_available","Vegetable",
  "Corn/Soybean","Indigowoad, Liquorice",
  "Average of cotton, garlic, melon, tomatto, and onion",
  "Average of tomato, safflower, corn, and bean"
)

# Crop recoding (exact matches only)
crop_recode <- c(
  "Spring barley" = "Barley",
  "continuous spring barley (Hordeum vulgare L.)" = "Barley",
  "barley.winter" = "Barley",
  "barley.spring" = "Barley",
  "Broad bean" = "Bean",
  "Dry_bean" = "Bean",
  "Beans" = "Bean",
  "Phaseolus vulgaris" = "Bean",
  "Organic_Dairy_Corn" = "Corn",
  "Corn (Zea mays L.) [2 years]" = "Corn",
  "continuous corn (Zea mays L.)" = "Corn",
  "Conventional_Dairy_Corn" = "Corn",
  "Sweetcorn" = "Corn",
  "continuous cotton" = "Cotton",
  "Continuous cotton (Gossypium hirsutum L.)" = "Cotton",
  "continuous cotton (G. hirsutum L.)" = "Cotton",
  "continuous shepherd's-purse (Captella bursa pastoris (L.) Medikus)" = "Shepherd's-purse",
  "continuous maize" = "Maize",
  "Silage maize" = "Maize",
  "Oat (Avena strigosa)+Maize (Zea mays L.)" = "Oat",
  "Spring pea" = "Pea",
  "Apple tree" = "Apple",
  "continuous soybean (Glycine max L. Merr.)" = "Soybean",
  "Soybean_Bragg" = "Soybean",
  "Soybean_Coker" = "Soybean",
  "Soybean_Ransom" = "Soybean",
  "continuous tomato (Lycopersicum esculentum Mill) [3 years]" = "Tomato",
  "continuous tomato (Lycopersicum esculentum Mill)" = "Tomato",
  "continuous wheat (Triticum aestivum L.) * 2 each year" = "Wheat",
  "Summer_wheat" = "Wheat",
  "Wheat -summer wheat - summer barley" = "Wheat",
  "Continuous_Wheat" = "Wheat",
  "Wheat, Liquorice" = "Wheat",
  "Spring wheat" = "Wheat",
  "continuous wheat (Triticum aestivum L.)" = "Wheat",
  "Winter_wheat" = "Wheat",
  "Spring_wheat" = "Wheat",
  "wheat.spring" = "Wheat",
  "wheat.winter" = "Wheat",
  "Winter wheat" = "Wheat",
  "durum wheat" = "Wheat",
  "spelt wheat" = "Wheat"
)

## -----------------------------------------------------------------------------
## Load raw data
## -----------------------------------------------------------------------------
su_dat        <- read_xlsx("./input/Su.xlsx")
jian_dat      <- read_xlsx("./input/Jian.xlsx")
farmgeek_dat  <- read_xlsx("./input/farmgeek.xlsx")

su_ref_dat    <- read_xlsx("./input/references_Su.xlsx")
jian_ref_dat  <- read_xlsx("./input/ref_Jian.xlsx")
farmgeek_ref  <- read_xlsx("./input/farmgeek_ref.xlsx")

## -----------------------------------------------------------------------------
## SU ET AL
## -----------------------------------------------------------------------------

su_dat <- su_dat %>%
  dplyr::mutate(source = "Su et al.") %>%
  dplyr::rename(Author1 = Author)

su_rep <- su_dat %>% 
  dplyr::mutate(
    Author  = gsub("(\\s|1|\\set.*|\\s[and].*)", "", Author1),
    newYear = gsub("(a|b)", "", Year)
  ) %>%
  tidyr::unite("refID", Author, newYear, remove = FALSE) %>%
  dplyr::mutate(no = dplyr::row_number())


id_ref <- su_ref_dat %>% 
  dplyr::mutate(
    Author2 = gsub(",.*", "", References_Su),
    year    = str_sub(References_Su, -6, -3)
  ) %>% 
  unite("ID", Author2, year, remove = FALSE) %>%
  group_by(Author2, year) %>%
  dplyr::mutate(duplicateID = row_number()) %>%
  ungroup() %>%
  unite("FID", ID, duplicateID, remove = FALSE)

dup <- id_ref %>% 
  janitor::get_dupes(ID) %>% 
  arrange(ID) %>%
  select(FID, ID, StudyID_Su, References_Su)

uniq_ref <- anti_join(id_ref, dup, by = "ID")

dup_red    <- dup      %>% select(FID, StudyID_Su, References_Su)
id_ref_red <- uniq_ref %>% select(FID, StudyID_Su, References_Su)

ref <- bind_rows(id_ref_red, dup_red)

# Special cases for ambiguous refID + Journal pairs
df_su <- su_rep %>%
  dplyr::mutate(class = case_when(
    (refID== "Almaraz_2009" & Journal=="Soil Science Society of America Journal") ~"Almaraz_2009_2",
    (refID== "Almaraz_2009" & Journal=="Soil & Tillage Research") ~"Almaraz_2009_1",
    (refID== "Balkcom_2010" & Journal== "Journal of Sustainable Agriculture") ~ "Balkcom_2010_1",
    (refID== "Balkcom_2010" & Journal== "Field Crops Research") ~ "Balkcom_2010_2",
    (refID== "Cociu_2019" & Journal== "Romanian Agricultural Research") ~ "Cociu_2019_1",
    (refID== "Cociu_2019" & Journal== "Rom. Agric. Res") ~ "Cociu_2019_2",
    (refID== "Fischer_2002" & Journal== "Field Crops Research") ~ "Fischer_2002_1",
    (refID== "Fischer_2002" & Journal== "F. Crop. Res.") ~ "Fischer_2002_2",
    (refID== "Galvez_2001" & Journal== "Plant Soil") ~ "Galvez_2001_1",
    (refID== "Galvez_2001" & Journal== "American Journal of Alternative Agriculture") ~ "Galvez_2001_2",
    (refID== "Halvorson_1999" & Journal== "Agronomy Journal") ~ "Halvorson_1999_1",
    (refID== "Halvorson_1999" & Journal== "Agron. J.") ~ "Halvorson_1999_2",
    (refID== "Huang_2008" & Journal== "Field Crops Research") ~ "Huang_2008_1",
    (refID== "Huang_2008" & Journal== "Agronomy Journal") ~ "Huang_2008_2",
    (refID== "Lal_1997" & Journal== "Soil & Tillage Research") ~ "Lal_1997_1",
    (refID== "Lal_1997" & Journal== "Land Degradation & Development") ~ "Lal_1997_2",
    (refID== "Mutsamba_2019" & Journal== "NJAS - Wageningen Journal of Life Sciences") ~ "Mutsamba_2019_1",
    (refID== "Mutsamba_2019" & Journal== "NJAS - Wageningen J. Life Sci.") ~ "Mutsamba_2019_2",
    (refID== "Ogunremi_1986" & Journal== "Soil & Tillage Research") ~ "Ogunremi_1986_1",
    (refID== "Ogunremi_1986" & Journal== "Soil Tillage Res.") ~ "Ogunremi_1986_2",
    (refID== "Parihar_2019" & Journal== "Field Crops Research") ~ "Parihar_2019_1",
    (refID== "Parihar_2019" & Journal== "Indian Journal of Agricultural Sciences") ~ "Parihar_2019_2",
    (refID== "Ram_2010" & Journal== "Indian Journal of Agronomy") ~ "Ram_2010_1",
    (refID== "Ram_2010" & Journal== "Ecology, Environment & Conservation") ~ "Ram_2010_2",
    (refID== "Silva_2019" & Journal== "Field Crops Research") ~ "Silva_2019_1",
    (refID== "Silva_2019" & Journal== "International Journal of Plant Production") ~ "Silva_2019_2",
    (refID== "Sun_2018" & Journal== "Agriculture, Ecosystems, and Environment") ~ "Sun_2018_1",
    (refID== "Sun_2018" & Journal== "Field Crops Research") ~ "Sun_2018_2",
    (refID== "Verhulst_2011" & Journal== "Plant S.") ~ "Verhulst_2011_1",
    (refID== "Verhulst_2011" & Journal== "Field Crops Research") ~ "Verhulst_2011_2",
    (refID== "Verhulst_2011" & Journal== "Plant Soil") ~ "Verhulst_2011_3",
    (refID== "Wang_2018" & Journal== "Agricultural Water Management") ~ "Wang_2018_1",
    (refID== "Wang_2018" & Journal== "Applied Ecology and Environmental Research") ~ "Wang_2018_2",
    (refID== "West_1996" & Journal== "Journal of Production Agriculture") ~ "West_1996_1",
    (refID== "West_1996" & Journal== "J. Prod. Agric.") ~ "West_1996_2",
    (refID== "Yadav_2018" & Journal== "Archives of Agronomy and Soil Science") ~ "Yadav_2018_1",
    (refID== "Yadav_2018" & Journal== "Agricultural Research") ~ "Yadav_2018_2",
    (refID== "Yadav_2019" & Journal== "Agriculture, Ecosystems, and Environment") ~ "Yadav_2019_1",
    (refID== "Yadav_2019" & Journal== "Carbon Management") ~ "Yadav_2019_2",
    (refID== "Yadav_2019" & Journal== "Ecological Indicators") ~ "Yadav_2019_3",
    (refID== "Yang_2018" & Journal== "Agricultural Systems") ~ "Yang_2018_1",
    (refID== "Yang_2018" & Journal== "Agricultural Water Management") ~ "Yang_2018_2",
    (refID== "Zhang_2018" & Journal== "Field Crops Research") ~ "Zhang_2018_1",
    (refID== "Zhang_2018" & Journal== "Agricultural and Forest Meteorology") ~ "Zhang_2018_2"
    # NOTE: original code had a duplicated condition for Zhang_2018 + FCR (3rd variant);
    # if you truly need a 3rd class, you'll want to distinguish it with a different Journal string.
  )) %>%
  dplyr::mutate(newClass = coalesce(class, refID))

df_su_fuzz <- fuzzy_right_join(
  ref, df_su,
  by = c("FID" = "newClass"),
  match_fun = str_detect
)

df_su_all <- df_su_fuzz %>%
  spaceless() %>%
  dplyr::mutate(
    Cons_Type = "NT",
    newCrop   = gsub("(\\.winter|\\.spring)", "", Crop)
  ) %>%
  dplyr::mutate(effectSize = ((Yield_of_NT - Yield_of_CT) / Yield_of_CT) * 100)

fin_sel_su <- df_su_all %>%
  select(
    no, source, References_Su, Author1, Year, Journal, Site_country, Location,
    Longitude, Latitude, Yield_of_CT, Yield_of_NT, effectSize,
    Cons_Type, newCrop, Replications_in_experiment, Harvest_year, N_input,
    Soil_cover_in_NT, Weed_and_pest_control_NT, Crop_rotation_in_NT
  ) %>%
  dplyr::rename(
    x          = Longitude,
    y          = Latitude,
    control    = Yield_of_CT,
    managed    = Yield_of_NT,
    Author     = Author1,
    sampleSize = Replications_in_experiment,
    country    = Site_country,
    key        = Cons_Type,
    soil_cover = Soil_cover_in_NT,
    references = References_Su,
    Crop       = newCrop,
    Year_harvest = Harvest_year,
    weed_control = Weed_and_pest_control_NT,
    rotation   = Crop_rotation_in_NT
  )

## -----------------------------------------------------------------------------
## JIAN ET AL
## -----------------------------------------------------------------------------

# Make sure IDs are character in both tables
jian_ref_dat <- jian_ref_dat %>%
  dplyr::mutate(ID = as.character(ID))

jian_dat <- jian_dat %>%
 dplyr::rename(ID = StudyID) %>%
  dplyr::mutate(ID = as.character(ID))

jian_df <- jian_ref_dat %>%
  dplyr::left_join(jian_dat, by = "ID") %>%
  dplyr::mutate(
    source     = "Jian et al",
    effectSize = ((Yield_T - Yield_C) / Yield_C) * 100,
    N_input    = "NA"
  )

sel_jian <- jian_df %>%
  select(
    no, source, References, Author_F, YearPublication,
    Journal, Country, SiteInfor, Longitude, Latitude,
    Yield_C, Yield_T, effectSize, Conservation_Type,
    GrainCrop, NoSubsample, SamplingYear, N_input,
    soil_cover, weed_control, rotation
  )

fin_sel_jian <- sel_jian %>%
  dplyr::rename(
    x           = Longitude,
    y           = Latitude,
    control     = Yield_C,
    managed     = Yield_T,
    sampleSize  = NoSubsample,
    country     = Country,
    key         = Conservation_Type,
    references  = References,
    Year        = YearPublication,
    Crop        = GrainCrop,
    Year_harvest = SamplingYear,
    Location    = SiteInfor,
    Author      = Author_F
  )

## -----------------------------------------------------------------------------
## FARMGEEK
## -----------------------------------------------------------------------------

# Turn key to factor
farmgeek_dat$key<-as.factor(farmgeek_dat$key)

farmgeek_dat<- farmgeek_dat %>% dplyr::mutate(no = row_number()) %>%
  add_column(source="farmgeek")

farmgeek_dat<-farmgeek_dat %>%
  mutate(key=ifelse(key=="1","AF", key))%>% 
  mutate(key=ifelse(key=="2","BioC", key))%>% 
  mutate(key=ifelse(key=="3","BioF", key))%>% 
  mutate(key=ifelse(key=="4","GenDiv", key))%>% 
  mutate(key=ifelse(key=="5","GMO", key))%>% 
  mutate(key=ifelse(key=="6","GM", key))%>% 
  mutate(key=ifelse(key=="7","IN", key))%>% 
  mutate(key=ifelse(key=="8","NT", key))%>% 
  mutate(key=ifelse(key=="9","OF", key))%>% 
  mutate(key=ifelse(key=="10","OSF", key))%>% 
  mutate(key=ifelse(key=="11","SRF", key))%>% 
  mutate(key=ifelse(key=="12","IN_w", key))%>% 
  mutate(key=ifelse(key=="13","WA", key))%>%
  mutate(effectSize=effectSize*100) %>%
  select(no, source, x, y, effectSize, sampleSize, 
  country,location,key,author,crop)

farmgeek_dat <- farmgeek_dat %>%
  add_column(control= NA, managed=NA,Year_harvest="NA",Author=NA,Year=NA,Journal=NA,
            N_input ="NA",soil_cover="NA",weed_control="NA",rotation="NA")%>%
            select(no,source,author,Author,Year,Journal,country,location,x,y,control,managed,effectSize, key,crop,
            sampleSize,Year_harvest,N_input,soil_cover,weed_control,rotation) 

# Load look up tables 
farmgeek_df_fuzz<-fuzzy_right_join(farmgeek_ref, farmgeek_dat,
                                   by = c("doi"="author"), 
                                   match_fun = str_detect) 

farmgeek_dat_df<-farmgeek_df_fuzz %>% select(no,source,references,Author, Year, 
                                             Journal,country,location,x,y,control,
                                             managed,effectSize, key,crop,
                                             sampleSize,Year_harvest,N_input,
                                             soil_cover,weed_control,rotation) %>%
                                dplyr::rename(Location=location,Crop=crop)

## -----------------------------------------------------------------------------
## MERGE SU + JIAN + FARMGEEK
## -----------------------------------------------------------------------------

# Make sure all three have the same types for shared columns
# Harmonise column types before binding ------------------------------

standardise_types <- function(df) {
  df %>%
    dplyr::mutate(
      # numeric-ish columns
      no         = as.numeric(no),
      x          = as.numeric(x),
      y          = as.numeric(y),
      control    = as.numeric(control),
      managed    = as.numeric(managed),
      effectSize = as.numeric(effectSize),
      sampleSize = as.numeric(sampleSize),

      # everything else as character
      source       = as.character(source),
      references   = as.character(references),
      Author       = as.character(Author),
      Year         = as.character(Year),
      Journal      = as.character(Journal),
      country      = as.character(country),
      Location     = as.character(Location),
      key          = as.character(key),
      Crop         = as.character(Crop),
      Year_harvest = as.character(Year_harvest),
      N_input      = as.character(N_input),
      soil_cover   = as.character(soil_cover),
      weed_control = as.character(weed_control),
      rotation     = as.character(rotation)
    )
}

# Clean and numeric-ise sampleSize in Su using the minimum mentioned value
fin_sel_su <- fin_sel_su %>%
  dplyr::mutate(
    sampleSize = if_else(
      is.na(sampleSize),
      NA_real_,
      map_dbl(str_extract_all(sampleSize, "\\d+"), function(v) {
        if (length(v) == 0) {
          NA_real_
        } else {
          min(as.numeric(v))  # use minimum instead of mean
        }
      })
    )
  )

fin_sel_su_std      <- standardise_types(fin_sel_su)
fin_sel_jian_std    <- standardise_types(fin_sel_jian)
farmgeek_dat_df_std <- standardise_types(farmgeek_dat_df)

df_all1 <- dplyr::bind_rows(
  fin_sel_su_std,
  fin_sel_jian_std,
  farmgeek_dat_df_std
)

unique(df_all1$key)


# Clean crop labels
df_all_up_crop1 <- df_all1 %>%
  filter(!Crop %in% crops_to_drop) %>%
  dplyr::mutate(Crop = recode(Crop, !!!crop_recode))

dim(df_all1)
dim(df_all_up_crop1)

# Keep only AF, CC, NT, OF
df_all_up_crop_target1<- df_all_up_crop1[is.element(df_all_up_crop1$key, c("AF","CC","NT","OF")),]
dim(df_all_up_crop_target1)

df_all_up_crop_target1 %>%
  dplyr::filter(source == "farmgeek") %>%
  dplyr::count(references, key, sort = TRUE)


write_xlsx(df_all_up_crop_target1, "df_all_up_crop_target1.xlsx")
write_xlsx(df_all_up_crop_target, "df_all_up_crop_target.xlsx")


# sample sizes
final_data <- df_all_up_crop_target %>%
  dplyr::mutate(
    sampleSize1 = sampleSize,
    sampleSize_control   = sampleSize,
    sampleSize_treatment = sampleSize1
  ) %>%
  select(
    no, source, references, Author, Year, Journal,
    country, Location, x, y, control, managed, effectSize, key, Crop,
    sampleSize_control, sampleSize_treatment, Year_harvest,
    N_input, soil_cover, weed_control, rotation
  )

# Author without initials after first dot
final_data <- final_data %>%
  dplyr::mutate(Author1 = sub("\\..*", "", Author))

all_dat_sel <- final_data %>% dplyr::select(source,references,Author1,Year,Journal,
                        country,Location,x, y, control,managed, effectSize,key,Crop,
                        sampleSize_control,sampleSize_treatment,Year_harvest,
                        N_input,soil_cover,weed_control,rotation) %>%
                        dplyr::rename(Author=Author1) %>%
                        dplyr::mutate(no = row_number())%>%
                        filter(effectSize <= 100) %>%
                        drop_na(effectSize,Crop,x,y)

# Remove duplicates
all_dis <- all_dat_sel %>%
  distinct(
    Author, Journal, Location, x, y, Year,
    effectSize, key, Crop,
    .keep_all = TRUE
  )

# Convert to spatial
sp_dat  <- st_as_sf(all_dis, coords = c("x", "y"), crs = 4326)
ter_data <- vect(sp_dat)

write_xlsx(all_dis, "./input/new_df.xlsx")

## -----------------------------------------------------------------------------
## RASTERS: soil, climate, topography
## -----------------------------------------------------------------------------

r_soil <- rast("D:/covariates/soil_grid.tif")
wrb_soil <- rast("C:/Users/hounkpk1/Food_System/input/covariates/wrb/wrb.tif")
phos <- rast("C:/Users/hounkpk1/Food_System/input/covariates/phosphorus/phosphorus_proj.tif")
names(phos) <- "phosphorus"

ari <- rast("C:/Users/hounkpk1/Food_System/input/covariates/aridity/aridity.tif")
clim_kb <- rast("C:/Users/hounkpk1/Food_System/input/covariates/Climate Zone/Beck_KG_V1_present_0p0083.tif")
names(clim_kb) <- "kg_clim"

r_gdd <- rast(list.files(
  path       = "C:/Users/hounkpk1/Food_System/input/covariates/gdd",
  pattern    = ".tif",
  all.files  = TRUE,
  full.names = TRUE
))

names(r_gdd) <- c(
  "GDD_wheat", "barley", "potato", "sugar_beet", "GDD_rice", "cassava",
  "groundnut", "millet", "GDD_maize", "GDD_sorghum", "GDD_soybean", "sugarcane"
)

r_gdd <- subset(r_gdd, c("GDD_maize", "GDD_wheat", "GDD_rice", "GDD_soybean"))

slope <- rast("C:/Users/hounkpk1/OneDrive - Aalto University/Thesis_data/misc/slope.tif")
dem   <- rast("C:/Users/hounkpk1/Food_System/input/covariates/topography/dem.tif")
landform <- rast("D:/covariates/land_form.tif")

## -----------------------------------------------------------------------------
## Extract covariates
## -----------------------------------------------------------------------------

# Extract soil data
y <- terra::project(ter_data, crs(r_soil))
ext_soil <- as.data.frame(terra::extract(r_soil, y, na.rm = TRUE,bind=T,ID=F)) 
ext_phos<- as.data.frame(terra::extract(phos, y, na.rm = TRUE,ID=F))  # y coord. ref homolosine
ext_wrb<- as.data.frame(terra::extract(wrb_soil, ter_data, na.rm = T,ID=F))

# Extract climate data
ext_gdd <- as.data.frame(terra::extract(r_gdd, ter_data, xy=T,na.rm = TRUE,ID=F))
ext_ar<- as.data.frame(terra::extract(ari, ter_data, na.rm = TRUE,ID=F)) 
ext_clim<- as.data.frame(terra::extract(clim_kb, ter_data, na.rm = TRUE,ID=F))

# Extract topography data
ext_lf <-as.data.frame(terra::extract(landform, ter_data, na.rm = TRUE,ID=F))
ext_dem <- as.data.frame(terra::extract(dem, ter_data, na.rm = TRUE,ID=F))
ext_sl <- as.data.frame(terra::extract(slope, ter_data, na.rm = TRUE,ID=F))

ext_data <- bind_cols(
  ext_gdd, ext_ar, ext_clim,
  ext_soil, ext_phos, ext_wrb,
  ext_dem, ext_sl, ext_lf
)

names(ext_data)

## -----------------------------------------------------------------------------
## Look-up tables + joins
## -----------------------------------------------------------------------------

wrb_looktab <- read_xlsx("C:/Users/hounkpk1/Food_System/input/covariates/others/soil_world/WRB_Legend.xlsx")
kg_looktab  <- read_xlsx("C:/Users/hounkpk1/Food_System/input/covariates/Climate Zone/kg_classes.xlsx")
crop_grp    <- read_xlsx("C:/Users/hounkpk1/Food_System/Lookup_Table/crop_groups.xlsx")

# NOTE: numeric positions here are fragile; if possible replace with names.
ext_data <- ext_data %>%
  dplyr::rename(
    sand     = 29,
    silt     = 30,
    clay     = 31,
    pH       = 32,
    cec      = 33,
    nit      = 34,
    soc      = 35,
    bd       = 36,
    aridity  = ai_et0,
    ID_wrb   = RSG,
    ID_kg    = kg_clim,
    landform = land_form
  )

df1 <- ext_data %>%
  left_join(kg_looktab, by = "ID_kg")

df2 <- df1 %>%
  left_join(wrb_looktab, by = "ID_wrb")

df <- df2 %>%
  left_join(crop_grp, by = "Crop")

## -----------------------------------------------------------------------------
## Texture harmonisation + imputation
## -----------------------------------------------------------------------------

my.text_nor2 <- df %>%
  transmute(
    CLAY = clay,
    SILT = silt,
    SAND = sand
  )

# mice setup
emptyModel     <- mice(my.text_nor2, maxit = 100)
method         <- emptyModel$method
predictorMatrix <- emptyModel$predictorMatrix

imputedData <- mice(my.text_nor2, method = method, predictorMatrix = predictorMatrix, m = 5)
df_text <- complete(imputedData)

# Check missing
df_text %>% dplyr::summarise(across(everything(), ~ sum(is.na(.))))

SSCP_norm <- TT.normalise.sum(tri.data = df_text) %>%
  dplyr::rename(clay = 1, silt = 2, sand = 3)

df_final1 <- df %>%
  select(-clay, -sand, -silt)

df_final2 <- bind_cols(df_final1, SSCP_norm) %>%
  relocate(sand, .after = Crop_Group) %>%
  relocate(silt, .after = sand) %>%
  relocate(clay, .after = silt) %>%
  dplyr::rename(
    kg_clim = kg_class2,
    wrb     = ID_wrb
  )

# Save dataext_data
write_xlsx(df_final2, "./input/new_data2.xlsx")

dim(df_final2)
