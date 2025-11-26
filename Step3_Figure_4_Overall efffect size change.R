# =========================================================
# Figure 2 (fixed): includes crop-by-practice panel (AF only has its crops)
# =========================================================

# ---------------- Packages ----------------
library(dplyr)
library(readxl)
library(tidyr)
library(purrr)
library(boot)
library(stringr)
library(writexl)
library(soiltexture)
library(ggplot2)
library(future)
library(furrr)
library(forcats)
library(patchwork)
library(ggh4x)
library(grid)

set.seed(7)

# ---------------- I/O ----------------
in_path  <- "./input/scps_data.xlsx"     # <- change if needed
out_dir  <- "./output/Figure_4/"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------------- Read ----------------
df_raw0 <- read_xlsx(in_path, guess_max = 1000)

# ---------------- Helpers ----------------
normalize_reference <- function(reference) {
  reference %>%
    tolower() %>%
    gsub("\\s+", " ", .) %>%          # collapse multiple spaces
    gsub("[[:punct:]]", "", .) %>%    # remove punctuation
    trimws()
}

extract_year_from_ref <- function(reference) {
  if (is.na(reference)) return(NA_integer_)
  yrs <- stringr::str_extract_all(reference, "(?<!\\d)(19|20)\\d{2}(?!\\d)")[[1]]
  if (length(yrs) == 0) return(NA_integer_)
  suppressWarnings(as.integer(tail(yrs, 1)))
}

# Percent conversion: ES(%) = 100 * (exp(lnRR) - 1)
perc <- function(x) 100 * (exp(x) - 1)

# Cluster (study-level) bootstrap of the mean
boot_mean_ci_study <- function(data, study_col = "study_id",
                               value_col = "effectSize",
                               R = 1000, conf = 0.95) {
  data <- data %>% filter(is.finite(.data[[value_col]]), !is.na(.data[[study_col]]))
  studs <- unique(data[[study_col]])
  k <- length(studs)
  if (k < 3) return(list(mean = NA_real_, ci_low = NA_real_, ci_high = NA_real_, k = k))

  re_mean <- function(study_ids) {
    idx <- unlist(lapply(study_ids, function(s) which(data[[study_col]] == s)), use.names = FALSE)
    mean(data[[value_col]][idx])
  }

  set.seed(7)
  boot_vals <- replicate(R, {
    s_star <- sample(studs, k, replace = TRUE)
    re_mean(s_star)
  })

  est <- mean(data[[value_col]])
  qs  <- quantile(boot_vals, probs = c((1 - conf)/2, 1 - (1 - conf)/2), na.rm = TRUE)

  list(mean = as.numeric(est),
       ci_low = as.numeric(qs[1]),
       ci_high = as.numeric(qs[2]),
       k = k)
}

# ---------------- Base filtering + ES conversion ----------------
df_raw <- df_raw0 %>%
  filter(key %in% c("AF","CC","NT","OF"),
         !Crop_Group %in% c("Grass")) %>%
  mutate(
    Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others", "V_F_others", Crop_Group)
    #effectSize = perc(effectSize)
  ) %>%
  filter(is.finite(effectSize), effectSize <= 100)

# ---------------- References -> study_id ----------------
df_raw <- df_raw %>%
  mutate(
    Year_from_ref   = purrr::map_int(references, extract_year_from_ref),
    Year_final      = dplyr::coalesce(Year_from_ref,
                                      suppressWarnings(as.integer(Year))),
    references_norm = normalize_reference(references),
    Crop_key        = dplyr::coalesce(as.character(Crop), as.character(Crop)),
    # include location (x, y) in study_id
    study_id        = paste(
      references_norm,
      Crop_key,
      Year_final,
      round(x, 5),   # adjust rounding as you like
      round(y, 5),
      sep = "_"
    )
  )

dim(df_raw)
sum(is.na(df_raw$references))  

df_unique <- df_raw %>%
  dplyr::select(references,references_norm, Author,Year_final,Journal, country) %>%
  dplyr::distinct(references_norm, .keep_all = TRUE) %>%
  drop_na(references)
  
df_unique

# ---------------- Classes ----------------
# SOC
soc_rcl <- matrix(c(0,5,1, 5,10,2, 10,180,3), ncol=3, byrow=TRUE)
# Available P
p_rcl   <- matrix(c(0,10.9,1, 10.9,21.4,2, 21.4,185,3), ncol=3, byrow=TRUE)
# Bulk density
bd_rcl  <- matrix(c(0,1.2,1, 1.2,1.47,2, 1.47,1.7,3), ncol=3, byrow=TRUE)
# DEM
dem_rcl <- matrix(c(0,250,1, 250,1000,2, 1000,4000,3), ncol=3, byrow=TRUE)
# Slope
slope_rcl <- matrix(c(0,0.20,1, 0.20,1.00,2, 1.00,5.00,3, 5.00,15.00,4, 15.00,80.00,5), ncol=3, byrow=TRUE)
# GDD bands (same cutoffs, per-crop columns)
gdd_rcl <- matrix(c(0,800,1, 800,2700,2, 2700,4000,3, 4000,6000,4, 6000,10000,5), ncol=3, byrow=TRUE)
# Aridity (AI)
aridity_rcl <- matrix(c(0,0.05,1, 0.05,0.20,2, 0.20,0.50,3, 0.50,0.65,4, 0.65,1.00,5), ncol=3, byrow=TRUE)

# Texture (HYPRES -> 3 groups)
mk_texture_class <- function(clay, silt, sand) {
  ok <- is.finite(clay) & is.finite(silt) & is.finite(sand)
  clay[!ok] <- NA_real_; silt[!ok] <- NA_real_; sand[!ok] <- NA_real_
  tri <- data.frame(CLAY = clay, SILT = silt, SAND = sand)
  tex <- TT.points.in.classes(tri.data = tri, class.sys = "HYPRES.TT") %>% as.data.frame()
  texture <- dplyr::case_when(
    tex$VF==1 & tex$F==1  ~ "1",
    tex$F==1              ~ "1",
    tex$VF==1             ~ "1",
    tex$M==1 & tex$MF==1  ~ "2",
    tex$M==1              ~ "2",
    tex$MF==1             ~ "2",
    tex$C==1              ~ "3",
    TRUE ~ NA_character_
  )
  factor(texture, levels=c("1","2","3"), labels=c("fine","medium","coarse"))
}

# Landform helpers (optional)
landform_names <- c(`1`="Mtn_sumt","2"="Cliff_sl","3"="Lwhi_mtn","4"="Shills_dcsl","5"="Lhgsl_steep",
                    "6"="Lhgsl_mod","7"="Mtn_vs","8"="Mod_hills","9"="Tfphi_dis","11"="Tfphi_surf",
                    "13"="Val_sl","15"="Tfplw_dis","17"="Tfplw_surf","19"="Hi_plain","21"="Lw_plain")
collapse_landform <- function(x) {
  xi <- suppressWarnings(as.integer(x))
  dplyr::recode(
    xi, `0`=NA_integer_, `1`=1L,`2`=2L,`3`=3L,`4`=4L,`5`=5L,`6`=6L,`7`=7L,`8`=8L,
    `9`=9L,`10`=9L,`11`=11L,`12`=11L,`13`=13L,`14`=13L,`15`=15L,`16`=15L,
    `17`=17L,`18`=17L,`19`=19L,`20`=19L,`21`=21L,`22`=21L, .default = NA_integer_
  )
}

# WRB mapping
wrb_full_cod <- c(0,1,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,20,22,23,24,26,29)
wrb_full_leg <- c("Acrisols","Albeluvisols","Alisols","Andosols","Arenosols",
                  "Calcisols","Cambisols","Chernozems","Ferralsols","Fluvisols",
                  "Gleysols","Gypsisols","Histosols","Kastanozems","Leptosols",
                  "Lixisols","Luvisols","Phaeozems","Plinthosols","Podzols",
                  "Regosols","Solonetz","Vertisols")

# Detect pH column
ph_col <- intersect(c("ph","pH","soil_pH"), names(df_raw))
if (length(ph_col) == 0) stop("No pH column found (looked for 'ph', 'pH', 'soil_pH').")
ph_col <- ph_col[1]

# Build classes
df_classed <- df_raw %>%
  mutate(
    # pH
    ph_class_num = case_when(
      .data[[ph_col]] < 6.3 ~ 1,
      .data[[ph_col]] >= 6.3 & .data[[ph_col]] < 7.4 ~ 2,
      .data[[ph_col]] >= 7.4 ~ 3,
      TRUE ~ NA_real_
    ),
    ph_class = factor(as.integer(ph_class_num), levels=c(1,2,3),
                      labels=c("acidic","neutral","alkaline")),
    # SOC
    soc_class_num = case_when(
      soc >= soc_rcl[1,1] & soc < soc_rcl[1,2] ~ soc_rcl[1,3],
      soc >= soc_rcl[2,1] & soc < soc_rcl[2,2] ~ soc_rcl[2,3],
      soc >= soc_rcl[3,1] & soc < soc_rcl[3,2] ~ soc_rcl[3,3],
      TRUE ~ NA_real_
    ),
    soc_class = factor(as.integer(soc_class_num), levels=c(1,2,3),
                       labels=c("<5","5-10",">10")),
    # P
    p_class_num = case_when(
      phosphorus >= p_rcl[1,1] & phosphorus < p_rcl[1,2] ~ p_rcl[1,3],
      phosphorus >= p_rcl[2,1] & phosphorus < p_rcl[2,2] ~ p_rcl[2,3],
      phosphorus >= p_rcl[3,1] & phosphorus < p_rcl[3,2] ~ p_rcl[3,3],
      TRUE ~ NA_real_
    ),
    p_class = factor(as.integer(p_class_num), levels=c(1,2,3),
                     labels=c("<10.9","10.9-21.4",">21.4")),
    # BD
    bd_class_num = case_when(
      bd >= bd_rcl[1,1] & bd < bd_rcl[1,2] ~ bd_rcl[1,3],
      bd >= bd_rcl[2,1] & bd < bd_rcl[2,2] ~ bd_rcl[2,3],
      bd >= bd_rcl[3,1] & bd < bd_rcl[3,2] ~ bd_rcl[3,3],
      TRUE ~ NA_real_
    ),
    bd_class = factor(as.integer(bd_class_num), levels=c(1,2,3),
                      labels=c("<1.20","1.20-1.47",">1.47")),
    # DEM
    dem_class_num = case_when(
      dem >= dem_rcl[1,1] & dem < dem_rcl[1,2] ~ dem_rcl[1,3],
      dem >= dem_rcl[2,1] & dem < dem_rcl[2,2] ~ dem_rcl[2,3],
      dem >= dem_rcl[3,1] & dem < dem_rcl[3,2] ~ dem_rcl[3,3],
      TRUE ~ NA_real_
    ),
    dem_class = factor(as.integer(dem_class_num), levels=c(1,2,3),
                       labels=c("<250","250-1000",">1000")),
    # Slope
    slope_class_num = case_when(
      slope >= slope_rcl[1,1] & slope < slope_rcl[1,2] ~ slope_rcl[1,3],
      slope >= slope_rcl[2,1] & slope < slope_rcl[2,2] ~ slope_rcl[2,3],
      slope >= slope_rcl[3,1] & slope < slope_rcl[3,2] ~ slope_rcl[3,3],
      slope >= slope_rcl[4,1] & slope < slope_rcl[4,2] ~ slope_rcl[4,3],
      slope >= slope_rcl[5,1] & slope < slope_rcl[5,2] ~ slope_rcl[5,3],
      TRUE ~ NA_real_
    ),
    slope_class = factor(as.integer(slope_class_num), levels=c(1,2,3,4,5),
                         labels=c("<0.20","0.2-1","1-5","5-15",">15")),
    # GDD classes (per crop)
    gdd_maize_class_num = case_when(
      GDD_maize >= gdd_rcl[1,1] & GDD_maize < gdd_rcl[1,2] ~ gdd_rcl[1,3],
      GDD_maize >= gdd_rcl[2,1] & GDD_maize < gdd_rcl[2,2] ~ gdd_rcl[2,3],
      GDD_maize >= gdd_rcl[3,1] & GDD_maize < gdd_rcl[3,2] ~ gdd_rcl[3,3],
      GDD_maize >= gdd_rcl[4,1] & GDD_maize < gdd_rcl[4,2] ~ gdd_rcl[4,3],
      GDD_maize >= gdd_rcl[5,1] & GDD_maize < gdd_rcl[5,2] ~ gdd_rcl[5,3],
      TRUE ~ NA_real_
    ),
    gdd_wheat_class_num = case_when(
      GDD_wheat >= gdd_rcl[1,1] & GDD_wheat < gdd_rcl[1,2] ~ gdd_rcl[1,3],
      GDD_wheat >= gdd_rcl[2,1] & GDD_wheat < gdd_rcl[2,2] ~ gdd_rcl[2,3],
      GDD_wheat >= gdd_rcl[3,1] & GDD_wheat < gdd_rcl[3,2] ~ gdd_rcl[3,3],
      GDD_wheat >= gdd_rcl[4,1] & GDD_wheat < gdd_rcl[4,2] ~ gdd_rcl[4,3],
      GDD_wheat >= gdd_rcl[5,1] & GDD_wheat < gdd_rcl[5,2] ~ gdd_rcl[5,3],
      TRUE ~ NA_real_
    ),
    gdd_rice_class_num = case_when(
      GDD_rice >= gdd_rcl[1,1] & GDD_rice < gdd_rcl[1,2] ~ gdd_rcl[1,3],
      GDD_rice >= gdd_rcl[2,1] & GDD_rice < gdd_rcl[2,2] ~ gdd_rcl[2,3],
      GDD_rice >= gdd_rcl[3,1] & GDD_rice < gdd_rcl[3,2] ~ gdd_rcl[3,3],
      GDD_rice >= gdd_rcl[4,1] & GDD_rice < gdd_rcl[4,2] ~ gdd_rcl[4,3],
      GDD_rice >= gdd_rcl[5,1] & GDD_rice < gdd_rcl[5,2] ~ gdd_rcl[5,3],
      TRUE ~ NA_real_
    ),
    gdd_soybean_class_num = case_when(
      GDD_soybean >= gdd_rcl[1,1] & GDD_soybean < gdd_rcl[1,2] ~ gdd_rcl[1,3],
      GDD_soybean >= gdd_rcl[2,1] & GDD_soybean < gdd_rcl[2,2] ~ gdd_rcl[2,3],
      GDD_soybean >= gdd_rcl[3,1] & GDD_soybean < gdd_rcl[3,2] ~ gdd_rcl[3,3],
      GDD_soybean >= gdd_rcl[4,1] & GDD_soybean < gdd_rcl[4,2] ~ gdd_rcl[4,3],
      GDD_soybean >= gdd_rcl[5,1] & GDD_soybean < gdd_rcl[5,2] ~ gdd_rcl[5,3],
      TRUE ~ NA_real_
    ),
    gdd_maize_class   = factor(as.integer(gdd_maize_class_num),   levels=1:5, labels=paste0("GDD_maize",1:5)),
    gdd_wheat_class   = factor(as.integer(gdd_wheat_class_num),   levels=1:5, labels=paste0("GDD_wheat",1:5)),
    gdd_rice_class    = factor(as.integer(gdd_rice_class_num),    levels=1:5, labels=paste0("GDD_rice", 1:5)),
    gdd_soybean_class = factor(as.integer(gdd_soybean_class_num), levels=1:5, labels=paste0("GDD_soybean",1:5))
  ) %>%
  mutate(texture = mk_texture_class(clay, silt, sand))

# Optional: landform
if ("landform" %in% names(df_classed)) {
  df_classed <- df_classed %>%
    mutate(
      landform_collapsed = collapse_landform(landform),
      landform_class = factor(
        dplyr::recode(as.character(landform_collapsed), !!!landform_names),
        levels = unname(landform_names)
      )
    )
}

# WRB mapping
df_classed <- df_classed %>%
  mutate(
    wrb_code = suppressWarnings(as.integer(wrb)),
    wrb_name = case_when(
      is.na(wrb_code) ~ NA_character_,
      wrb_code %in% wrb_full_cod ~ wrb_full_leg[match(wrb_code, wrb_full_cod)],
      TRUE ~ NA_character_
    ),
    wrb_class = factor(wrb_name, levels = wrb_full_leg)
  )

# ---------------- Aridity band + standardize factors ----------------
safe_num <- function(d, nm) {
  n <- nrow(d)
  if (nm %in% names(d)) suppressWarnings(as.numeric(d[[nm]])) else rep(NA_real_, n)
}
AI_vec <- dplyr::coalesce(
  safe_num(df_classed, "aridity_index"),
  safe_num(df_classed, "AI"),
  safe_num(df_classed, "ai"),
  safe_num(df_classed, "aridity"),
  safe_num(df_classed, "aridity_class")
)

df <- df_classed %>%
  mutate(
    AI = AI_vec,
    aridity_class_num = case_when(
      is.na(AI) ~ NA_real_,
      AI >= aridity_rcl[1,1] & AI < aridity_rcl[1,2] ~ aridity_rcl[1,3],
      AI >= aridity_rcl[2,1] & AI < aridity_rcl[2,2] ~ aridity_rcl[2,3],
      AI >= aridity_rcl[3,1] & AI < aridity_rcl[3,2] ~ aridity_rcl[3,3],
      AI >= aridity_rcl[4,1] & AI < aridity_rcl[4,2] ~ aridity_rcl[4,3],
      AI >= aridity_rcl[5,1] & AI <= aridity_rcl[5,2] ~ aridity_rcl[5,3],
      TRUE ~ NA_real_
    ),
    aridity_band = factor(
      dplyr::recode(as.character(as.integer(aridity_class_num)),
                    `1`="Hyper-Arid",`2`="Arid",`3`="Semi-arid",`4`="Sub-Humid",`5`="Humid",
                    .default = NA_character_),
      levels = c("Hyper-Arid","Arid","Semi-arid","Sub-Humid","Humid")
    ),
    ph_class  = factor(ph_class,  levels=c("acidic","neutral","alkaline")),
    soc_class = factor(soc_class, levels=c("<5","5-10",">10")),
    p_class   = factor(p_class,   levels=c("<10.9","10.9-21.4",">21.4")),
    bd_class  = factor(bd_class,  levels=c("<1.20","1.20-1.47",">1.47")),
    texture   = factor(texture,   levels=c("fine","medium","coarse")),
    kg_clim   = if ("kg_clim" %in% names(.))
      factor(kg_clim, levels=c("Arid","Continental","Temperate","Tropical"))
    else factor(NA_character_, levels=c("Arid","Continental","Temperate","Tropical")),
    dem_class   = factor(dem_class,   levels=c("<250","250-1000",">1000")),
    slope_class = factor(slope_class, levels=c("<0.20","0.2-1","1-5","5-15",">15")),
    Crop_Group  = factor(Crop_Group,  levels=c("Maize","Rice","Soybean","Wheat","Cereal","Cash crop","V_F_others")),
    key         = factor(key,         levels=c("AF","CC","NT","OF")),
    gdd_maize_band   = factor(case_when(
      gdd_maize_class=="GDD_maize1"~"<0.8", gdd_maize_class=="GDD_maize2"~"0.8-2.7",
      gdd_maize_class=="GDD_maize3"~"2.7-4", gdd_maize_class=="GDD_maize4"~"4-6",
      gdd_maize_class=="GDD_maize5"~"6-10", TRUE~NA_character_),
      levels=c("<0.8","0.8-2.7","2.7-4","4-6","6-10")),
    gdd_wheat_band   = factor(case_when(
      gdd_wheat_class=="GDD_wheat1"~"<0.8", gdd_wheat_class=="GDD_wheat2"~"0.8-2.7",
      gdd_wheat_class=="GDD_wheat3"~"2.7-4", gdd_wheat_class=="GDD_wheat4"~"4-6",
      gdd_wheat_class=="GDD_wheat5"~"6-10", TRUE~NA_character_),
      levels=c("<0.8","0.8-2.7","2.7-4","4-6","6-10")),
    gdd_rice_band    = factor(case_when(
      gdd_rice_class=="GDD_rice1"~"<0.8", gdd_rice_class=="GDD_rice2"~"0.8-2.7",
      gdd_rice_class=="GDD_rice3"~"2.7-4", gdd_rice_class=="GDD_rice4"~"4-6",
      gdd_rice_class=="GDD_rice5"~"6-10", TRUE~NA_character_),
      levels=c("<0.8","0.8-2.7","2.7-4","4-6","6-10")),
    gdd_soybean_band = factor(case_when(
      gdd_soybean_class=="GDD_soybean1"~"<0.8", gdd_soybean_class=="GDD_soybean2"~"0.8-2.7",
      gdd_soybean_class=="GDD_soybean3"~"2.7-4", gdd_soybean_class=="GDD_soybean4"~"4-6",
      gdd_soybean_class=="GDD_soybean5"~"6-10", TRUE~NA_character_),
      levels=c("<0.8","0.8-2.7","2.7-4","4-6","6-10"))
  )

# ---------------- Long/pivot + bootstrap (GLOBAL panels) ----------------
covariate_cols <- c(
  "key","Crop_Group","kg_clim","aridity_band",
  "ph_class","soc_class","p_class","bd_class","texture",
  "gdd_maize_band","gdd_wheat_band","gdd_rice_band","gdd_soybean_band",
  "dem_class","slope_class","wrb_class","landform_class"
)
covariate_cols_use <- intersect(covariate_cols, names(df))

df_long <- df %>%
  select(effectSize, study_id, references_norm,all_of(covariate_cols_use)) %>%
  pivot_longer(cols = all_of(covariate_cols_use),
               names_to = "cov", values_to = "cat") %>%
  filter(!is.na(cat), !is.na(study_id))

counts_by_group <- df_long %>%
  group_by(cov, cat) %>%
  summarise(
    n_obs = n(),
    n_studies = n_distinct(references_norm),
    .groups = "drop"
  )

plan(multisession, workers = max(1, parallel::detectCores() - 1))
grouped_data <- df_long %>% group_by(cov, cat) %>% group_split()

set.seed(7)
results_by_group <- furrr::future_map_dfr(
  grouped_data,
  function(g) {
    est <- boot_mean_ci_study(g, study_col = "study_id", value_col = "effectSize", R = 1000, conf = 0.95)
    tibble(cov = g$cov[1], cat = g$cat[1],
           mean = est$mean, ci_low = est$ci_low, ci_high = est$ci_high, k = est$k)
  },
  .options = furrr::furrr_options(seed = TRUE)
)

# Overall
set.seed(7)
overall_list <- value(future(
  boot_mean_ci_study(df, study_col = "study_id", value_col = "effectSize", R = 1000, conf = 0.95),
  seed = TRUE))
plan(sequential)

overall <- tibble(
  cov = "overall", cat = "overall",
  mean = overall_list$mean, ci_low = overall_list$ci_low, ci_high = overall_list$ci_high, k = overall_list$k,
  n_obs = nrow(df), n_studies = n_distinct(df$references_norm)
)

results_all <- results_by_group %>%
  left_join(counts_by_group, by = c("cov","cat")) %>%
  bind_rows(overall) %>%
  mutate(
    cat_label = ifelse(cov == "overall",
                       paste0(cat, " (", n_studies, ")"),
                       paste0(cat, " (", n_studies, ", ", n_obs, ")"))
  )

# ---------------- Label map ----------------
label_map <- tibble::tribble(
  ~cov,               ~component,          ~variable,
  "overall",          "Management",        "Practice",
  "key",              "Management",        "Practice",
  "Crop_Group",       "Crops",             "Crop group",
  "kg_clim",          "Climate",           "Köppen–Geiger",
  "aridity_band",     "Climate",           "Aridity class",
  "ph_class",         "Soil properties",   "pH",
  "soc_class",        "Soil properties",   "SOC (%)",
  "p_class",          "Soil properties",   "Available P (ppm)",
  "bd_class",         "Soil properties",   "Bulk density (g/cm^3)",
  "texture",          "Soil properties",   "Texture",
  "gdd_maize_band",   "Climate",           "GDD (maize, 10^2 C·day)",
  "gdd_wheat_band",   "Climate",           "GDD (wheat, 10^2 C·day)",
  "gdd_rice_band",    "Climate",           "GDD (rice, 10^2 C·day)",
  "gdd_soybean_band", "Climate",           "GDD (soybean, 10^2 C·day)",
  "dem_class",        "Topography",        "Elevation (m)",
  "slope_class",      "Topography",        "Slope (%)",
  "landform_class",   "Topography",        "Landform",
  "wrb_class",        "Soil properties",   "WRB"
)

mega_df <- results_all %>%
  left_join(label_map, by = "cov") %>%
  transmute(
    component = coalesce(component, "Misc"),
    variable  = coalesce(variable, cov),
    level     = as.character(cat),
    label     = cat_label,
    est       = mean,
    lwr       = ci_low,
    upr       = ci_high,
    k         = k,
    n_obs     = n_obs,
    n_studies = n_studies,
    order     = dplyr::row_number()
  )

#mega_keycrop<-read_xlsx("./output/Figure_2/figure_2_keycrop_data.xlsx")
# ---------------- EXTRA: Crops within each practice (interaction) ----------------
# This ensures AF only shows crops actually present in AF
df_keycrop <- df %>%
  filter(!is.na(Crop_Group), !is.na(key)) %>%
  mutate(key_crop = interaction(key, Crop_Group, sep=": ", lex.order = TRUE))

d_long_keycrop <- df_keycrop %>%
  select(effectSize, study_id, references_norm,key_crop) %>%
  filter(!is.na(key_crop)) %>%
  mutate(cov="key_crop", cat = key_crop) %>%
  select(effectSize, study_id, references_norm,cov, cat)

counts_keycrop <- d_long_keycrop %>%
  group_by(cov, cat) %>%
  summarise(n_obs = n(), n_studies = n_distinct(references_norm), .groups = "drop")

set.seed(7)
res_keycrop <- d_long_keycrop %>%
  group_by(cov, cat) %>%
  group_split() %>%
  map_dfr(function(g){
    est <- boot_mean_ci_study(g, study_col="study_id", value_col="effectSize", R=1000, conf=0.95)
    tibble(cov="key_crop", cat=g$cat[1], mean=est$mean, ci_low=est$ci_low, ci_high=est$ci_high, k=est$k)
  }) %>%
  left_join(counts_keycrop, by=c("cov","cat")) %>%
  mutate(component="Crops", variable="Crop group × practice")

# =======================
# GLOBAL CROPS PANEL ONLY
# =======================

# Overall (all practices pooled) crop means with study-clustered bootstrap
d_crops <- df %>%
  dplyr::filter(!is.na(Crop_Group))

# counts
counts_crops <- d_crops %>%
  dplyr::group_by(Crop_Group) %>%
  dplyr::summarise(
    n_obs     = dplyr::n(),
    n_studies = dplyr::n_distinct(references_norm),   # clustered by study_id
    .groups   = "drop"
  )

# bootstrap per crop
set.seed(7)
results_crops <- d_crops %>%
  dplyr::group_by(Crop_Group) %>%
  dplyr::group_split() %>%
  purrr::map_dfr(function(g){
    est <- boot_mean_ci_study(g, study_col = "study_id",
                              value_col = "effectSize",
                              R = 1000, conf = 0.95)
    tibble::tibble(
      Crop_Group = g$Crop_Group[1],
      mean = est$mean, ci_low = est$ci_low, ci_high = est$ci_high, k = est$k
    )
  }) %>%
  dplyr::left_join(counts_crops, by = "Crop_Group")

# final mega for crops (one facet: "Crop group")
set.seed(7)
mega_crops <- results_crops %>%
  dplyr::transmute(
    component  = "Crops",
    variable   = "Crop group",
    level      = as.character(Crop_Group),
    label      = paste0(Crop_Group, " (", n_studies, ", ", n_obs, ")"),
    est        = mean, lwr = ci_low, upr = ci_high,
    k, n_obs, n_studies,
    order      = dplyr::row_number()
  ) %>%
  # optional: control row order like the screenshot
  dplyr::mutate(level = factor(
    level,
    levels = c("Maize","Rice","Soybean","Wheat","Cereal","Cash crop","V_F_others")
  )) %>%
  dplyr::arrange(level)

# (optional) export
# writexl::write_xlsx(mega_crops, file.path(out_dir, "figure_2_global_crops.xlsx"))

# ---------------- Plot helpers ----------------
prep_for_plot <- function(df) {
  df <- df %>% filter(!is.na(est))
  df_all <- df %>% group_by(variable) %>% mutate(any_valid = any(n_studies >= 3, na.rm = TRUE)) %>% ungroup()
  df <- df_all %>% filter(n_studies >= 3 | !any_valid)

  df %>%
    group_by(variable) %>%
    arrange(
      case_when(
        variable %in% c("WRB","Landform") ~ -est,
        TRUE ~ as.numeric(order)
      ),
      .by_group = TRUE
    ) %>%
    mutate(
      label = paste0(level, " (", n_studies, ", ", n_obs, ")"),
      label = factor(label, levels = rev(unique(label)))
    ) %>%
    ungroup()
}

plot_component <- function(df_component,
                           title = NULL,
                           x_limits = NULL,
                           x_lab = "% change (crop yield)",
                           facet_cols = 1,
                           variable_order = NULL) {
  df_plot <- prep_for_plot(df_component)

  all_vars <- unique(df_component$variable)
  if (!is.null(variable_order)) {
    ord   <- intersect(variable_order, all_vars)
    other <- setdiff(all_vars, ord)
    all_vars <- c(ord, other)
  }
  df_plot$variable <- factor(df_plot$variable, levels = all_vars)

  if (is.null(x_limits)) {
    rng <- range(df_plot$lwr, df_plot$upr, na.rm = TRUE)
    pad <- diff(rng) * 0.08
    x_limits <- c(rng[1] - pad, rng[2] + pad)
  }

  ggplot(df_plot, aes(y = label, x = est)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, color = "red") +
    geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0, linewidth = 0.5) +
    geom_point(size = 1.8) +
    facet_wrap(~ variable, scales = "free_y", ncol = facet_cols) +
    scale_x_continuous(limits = x_limits, expand = expansion(mult = c(0.02, 0.05))) +
    labs(title = title, x = x_lab, y = NULL) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x  = element_text(size = 24),
      axis.text.y  = element_text(size = 24),
      axis.title   = element_blank(),
      strip.text   = element_text(size = 13, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# ---------------- Build panels ----------------
xlims    <- c(-20, 40)
x_breaks <- seq(xlims[1], xlims[2], by = 10)
x_lab    <- "% change (crop yield)"
extra_left <- theme(plot.margin = margin(8, 10, 8, 35))

#mega_df <- read_xlsx(file.path(out_dir, "figure_2_data.xlsx"))

# Management panel (key)
p_mgmt <- mega_df %>%
  dplyr::filter(component == "Management", variable == "Practice") %>%
  dplyr::mutate(
    order = dplyr::case_when(
      level == "overall" ~ 1L,
      level == "AF"      ~ 2L,
      level == "CC"      ~ 3L,
      level == "NT"      ~ 4L,
      level == "OF"      ~ 5L,
      TRUE               ~ order
    )
  ) %>%
  plot_component(
    title = NULL,
    x_limits = xlims,
    x_lab = x_lab,
    facet_cols = 1,
    variable_order = c("Practice")
  ) +
  scale_x_continuous(
    limits = xlims,
    breaks = x_breaks,
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  ) +
  extra_left

# Crops × Practice (the fix)
p_crops <- mega_crops %>%
  plot_component(
    title = NULL,
    x_limits = xlims,
    x_lab = x_lab,
    facet_cols = 1,
    variable_order = c("Crop group")
  ) +
  scale_x_continuous(limits = xlims, breaks = x_breaks,
                     expand = expansion(mult = c(0.02, 0.05))) +
  theme(strip.text = element_blank(),
        strip.background = element_blank()) +
  extra_left


# Climate
climate_var_order <- c(
  "Köppen–Geiger","Aridity class",
  "GDD (maize, 10^2 C·day)","GDD (wheat, 10^2 C·day)",
  "GDD (rice, 10^2 C·day)","GDD (soybean, 10^2 C·day)"
)
p_clim <- mega_df %>%
  filter(component == "Climate") %>%
  plot_component(title = NULL, x_limits = xlims, x_lab = x_lab, facet_cols = 1,
                 variable_order = climate_var_order) +
  scale_x_continuous(limits = xlims, breaks = x_breaks,
                     expand = expansion(mult = c(0.02, 0.05))) +
  theme(strip.text = element_blank(), strip.background = element_blank()) +
  extra_left

# Soil (WRB facet enlarged)
soil_var_order <- c("pH","SOC (%)","Bulk density (g/cm^3)","Texture","Available P (ppm)","WRB")
p_soil <- mega_df %>%
  filter(component == "Soil properties") %>%
  plot_component(title = NULL, x_limits = xlims, x_lab = x_lab, facet_cols = 1,
                 variable_order = soil_var_order) +
  scale_x_continuous(limits = xlims, breaks = x_breaks,
                     expand = expansion(mult = c(0.02, 0.05))) +
  theme(strip.text = element_blank(), strip.background = element_blank()) +
  extra_left
rows <- rep(1, length(soil_var_order)); rows[soil_var_order=="WRB"] <- 2.5
p_soil_bigwrb <- p_soil +
  ggh4x::force_panelsizes(rows = grid::unit(rows, "null"),
                          cols = grid::unit(1, "null"))

# Topography
topo_var_order <- c("Elevation (m)", "Slope (%)", "Landform")
p_topo <- mega_df %>%
  filter(component == "Topography") %>%
  plot_component(title = NULL, x_limits = xlims, x_lab = x_lab, facet_cols = 1,
                 variable_order = topo_var_order) +
  scale_x_continuous(limits = xlims, breaks = x_breaks,
                     expand = expansion(mult = c(0.02, 0.05))) +
  theme(strip.text = element_blank(), strip.background = element_blank()) +
  extra_left

# Hide x-axes in the top row
blank_x <- theme(axis.text.x  = element_blank(),
                 axis.title.x = element_blank(),
                 axis.ticks.x = element_blank())
p_mgmt_top  <- p_mgmt  + blank_x
p_crops_top <- p_crops + blank_x

# Layout: Row1 (mgmt | crops | blank) ; Row2 (climate | soil | topo)
blank_panel <- patchwork::plot_spacer()
design <- "
ABC
DEF
"

figure_pub <- p_mgmt_top + p_crops_top + blank_panel +
              p_clim + p_soil_bigwrb + p_topo +
  plot_layout(design = design, widths = c(1,1,1))

figure_pub 

# ---------------- Save ----------------
ggsave(file.path(out_dir, "figure_4.pdf"),
       figure_pub, device = cairo_pdf,
       width = 21, height = 25, units = "in",
       dpi = 600, limitsize = FALSE)

# Data exports (optional)
write_xlsx(mega_df,      file.path(out_dir, "figure_4_data.xlsx"))
write_xlsx(df,           file.path(out_dir, "full_data_figure_4.xlsx"))

print(mega_df, n=10000)
