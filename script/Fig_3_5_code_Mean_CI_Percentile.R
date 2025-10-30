################################################################################
# Build one tidy dataset with columns:
#   Management_type, cov, cat, label, n, mean, median, p25, p75, ci_low, ci_high
# for Overall + all moderators (management, crops, climate, soil, topography)
# using:
#   - ./input/data/all_dis_cov2_last.xlsx (AF, NT, OF)
#   - ./input/data/all_dis_cov2.xlsx      (CC)
#   - ./input/data/all_dis_cov3.xlsx      (WRB & Landform, all mgt types)
# Notes:
# - `cat` keeps the original code / text.
# - `label` provides a human-readable label (mapped when coded).
# - No plots are created.
################################################################################

# ---- Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(boot)
  library(writexl)
  library(purrr)
  library(stringr)
})

# ---- Helpers -----------------------------------------------------------------

# Convert ES (log response ratio) to percent change
perc <- function(data) { 100 * (exp(data) - 1) }

# Robust mean CI via bootstrap (BCa; fallback to percentile CI)
summary_stats <- function(x, R = 1000, conf = 0.95) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) {
    return(tibble(
      n       = length(x),
      mean    = if (length(x)) mean(x) else NA_real_,
      median  = if (length(x)) median(x) else NA_real_,
      p25     = if (length(x)) as.numeric(quantile(x, 0.25, names = FALSE)) else NA_real_,
      p75     = if (length(x)) as.numeric(quantile(x, 0.75, names = FALSE)) else NA_real_,
      ci_low  = NA_real_,
      ci_high = NA_real_
    ))
  }
  my.mean <- function(xx, i) mean(xx[i])
  b <- boot::boot(x, statistic = my.mean, R = R)

  bca <- try(boot::boot.ci(b, type = "bca", conf = conf), silent = TRUE)
  if (!inherits(bca, "try-error")) {
    ci_low  <- bca$bca[4]
    ci_high <- bca$bca[5]
  } else {
    perc_ci <- try(boot::boot.ci(b, type = "perc", conf = conf), silent = TRUE)
    ci_low  <- if (!inherits(perc_ci, "try-error")) perc_ci$percent[4] else NA_real_
    ci_high <- if (!inherits(perc_ci, "try-error")) perc_ci$percent[5] else NA_real_
  }

  tibble(
    n       = length(x),
    mean    = mean(x),
    median  = median(x),
    p25     = as.numeric(quantile(x, 0.25, names = FALSE)),
    p75     = as.numeric(quantile(x, 0.75, names = FALSE)),
    ci_low  = ci_low,
    ci_high = ci_high
  )
}

# Summarize by ONE grouping variable
summarise_by <- function(data, group_var, cov_label, R = 1000, conf = 0.95) {
  data %>%
    filter(!is.na({{ group_var }})) %>%
    group_by({{ group_var }}) %>%
    group_modify(~ summary_stats(.x$effectSize, R = R, conf = conf)) %>%
    ungroup() %>%
    rename(cat = {{ group_var }}) %>%
    mutate(
      cat = as.character(cat),
      cov = cov_label, .before = 1
    )
}

# Summarize by TWO grouping variables (e.g., Management_type + WRB/Landform)
summarise_by2 <- function(data, group_var1, group_var2, cov_label, R = 1000, conf = 0.95) {
  data %>%
    filter(!is.na({{ group_var1 }}), !is.na({{ group_var2 }})) %>%
    group_by({{ group_var1 }}, {{ group_var2 }}) %>%
    group_modify(~ summary_stats(.x$effectSize, R = R, conf = conf)) %>%
    ungroup() %>%
    rename(Management_type = {{ group_var1 }},
           cat             = {{ group_var2 }}) %>%
    mutate(cat = as.character(cat),
           cov = cov_label, .before = 1)
}

# ---- Code→label maps ---------------------------------------------------------

map_aridity <- tibble(
  cov  = "aridity index",
  code = c("1","2","3","4","5"),
  label= c("Hyper-arid (AI<0.05)", "Arid (0.05–0.20)", "Semi-arid (0.20–0.50)",
           "Sub-humid (0.50–0.65)", "Humid (>0.65)")
)
map_pH <- tibble(
  cov  = "pH",
  code = c("1","2","3"),
  label= c("Acidic (pH<6.3)", "Neutral (6.3–7.4)", "Alkaline (pH>7.4)")
)
map_soc <- tibble(
  cov  = "soc",
  code = c("1","2","3"),
  label= c("<5 g/kg", "5–10 g/kg", ">10 g/kg")
)
map_p <- tibble(
  cov  = "phosphorus",
  code = c("1","2","3"),
  label= c("<10.9 mg/kg", "10.9–21.4 mg/kg", ">21.4 mg/kg")
)
map_bd <- tibble(
  cov  = "bd",
  code = c("1","2","3"),
  label= c("<1.20 kg/dm³", "1.20–1.47 kg/dm³", ">1.47 kg/dm³")
)
map_tex <- tibble(
  cov  = "texture",
  code = c("1","2","3"),
  label= c("Fine", "Medium", "Coarse")
)
map_dem <- tibble(
  cov  = "dem",
  code = c("1","2","3"),
  label= c("<250 m", "250–1000 m", ">1000 m")
)
map_slope <- tibble(
  cov  = "slope_class",
  code = c("1","2","3","4","5"),
  label= c("<0.20%", "0.2–1%", "1–5%", "5–15%", ">15%")
)
map_gdd_maize <- tibble(cov="GDD_maize",  code=as.character(1:5),
                        label=c("<0.8","0.8–2.7","2.7–4","4–6","6–10"))
map_gdd_wheat <- tibble(cov="GDD_wheat",  code=as.character(1:5),
                        label=c("<0.8","0.8–2.7","2.7–4","4–6","6–10"))
map_gdd_rice  <- tibble(cov="GDD_rice",   code=as.character(1:5),
                        label=c("<0.8","0.8–2.7","2.7–4","4–6","6–10"))
map_gdd_soy   <- tibble(cov="GDD_soybean",code=as.character(1:5),
                        label=c("<0.8","0.8–2.7","2.7–4","4–6","6–10"))
map_wrb <- tibble(
  cov  = "WRB_soil_type",
  code = as.character(c(0,1,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,20,22,23,24,26,29)),
  label= c("Acrisols","Albeluvisols","Alisols","Andosols","Arenosols",
           "Calcisols","Cambisols","Chernozems","Ferralsols","Fluvisols",
           "Gleysols","Gypsisols","Histosols","Kastanozems","Leptosols",
           "Lixisols","Luvisols","Phaeozems","Plinthosols","Podzols",
           "Regosols","Solonetz","Vertisols")
)
map_landform <- tibble(
  cov  = "Landform",
  code = as.character(c(1,2,3,4,5,6,7,8,9,11,13,15,17,19,21)),
  label= c("Mtn_sumt","Cliff_sl","Lwhi_mtn","Shills_dcsl","Lhgsl_steep",
           "Lhgsl_mod","Mtn_vs","Mod_hills","Tfphi_dis","Tfphi_surf",
           "Val_sl","Tfplw_dis","Tfplw_surf","Hi_plain","Lw_plain")
)

label_map <- bind_rows(
  map_aridity, map_pH, map_soc, map_p, map_bd, map_tex, map_dem, map_slope,
  map_gdd_maize, map_gdd_wheat, map_gdd_rice, map_gdd_soy, map_wrb, map_landform
)

# ---- Function to process ONE management type --------------------------------
process_one <- function(data_path, key_val, guess_max = 1000, R = 1000, conf = 0.95) {
  df <- readxl::read_xlsx(data_path, guess_max = guess_max) %>%
    filter(key %in% key_val, !Crop_Group %in% c("Grass")) %>%
    dplyr::select(
      effectSize, key, Crop_Group, kg_clim, aridity_class, ph_class, soc_class,
      p_class, bd_class, texture, gdd_maize_class, gdd_wheat_class, gdd_rice_class,
      gdd_soybean_class, dem_class, slope_class
    ) %>%
    drop_na(Crop_Group) %>%
    dplyr::mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others","V_F_others",Crop_Group)) %>%
    mutate(ES = perc(effectSize)) %>%
    filter(!ES > 100) %>%
    select(!effectSize) %>%
    dplyr::rename(effectSize = ES)

  # Summaries for this management type
  overall_quant <- summary_stats(df$effectSize, R = R, conf = conf) %>%
    mutate(cov = "overall", cat = "overall", .before = 1)

  q_mgt   <- summarise_by(df, key,               "mgt",           R = R, conf = conf)
  q_crop  <- summarise_by(df, Crop_Group,        "crop",          R = R, conf = conf)
  q_clim  <- summarise_by(df, kg_clim,           "clim",          R = R, conf = conf)
  q_ai    <- summarise_by(df, aridity_class,     "aridity index", R = R, conf = conf)
  q_pH    <- summarise_by(df, ph_class,          "pH",            R = R, conf = conf)
  q_soc   <- summarise_by(df, soc_class,         "soc",           R = R, conf = conf)
  q_p     <- summarise_by(df, p_class,           "phosphorus",    R = R, conf = conf)
  q_bd    <- summarise_by(df, bd_class,          "bd",            R = R, conf = conf)
  q_tex   <- summarise_by(df, texture,           "texture",       R = R, conf = conf)
  q_gdd_m <- summarise_by(df, gdd_maize_class,   "GDD_maize",     R = R, conf = conf)
  q_gdd_w <- summarise_by(df, gdd_wheat_class,   "GDD_wheat",     R = R, conf = conf)
  q_gdd_r <- summarise_by(df, gdd_rice_class,    "GDD_rice",      R = R, conf = conf)
  q_gdd_s <- summarise_by(df, gdd_soybean_class, "GDD_soybean",   R = R, conf = conf)
  q_dem   <- summarise_by(df, dem_class,         "dem",           R = R, conf = conf)
  q_slope <- summarise_by(df, slope_class,       "slope_class",   R = R, conf = conf)

  bind_rows(
    overall_quant %>% mutate(cat = as.character(cat)),
    q_mgt, q_crop, q_clim, q_ai, q_pH, q_soc, q_p, q_bd, q_tex,
    q_gdd_m, q_gdd_w, q_gdd_r, q_gdd_s, q_dem, q_slope
  ) %>%
    select(cov, cat, n, mean, median, p25, p75, ci_low, ci_high) %>%
    mutate(Management_type = key_val, .before = 1)
}

# ---- Run for each management type -------------------------------------------

# AF (exactly as you requested)
af_tbl <- process_one("./input/data/all_dis_cov2_last.xlsx", "AF")

# CC
cc_tbl <- process_one("./input/data/all_dis_cov2.xlsx",      "CC")

# NT
nt_tbl <- process_one("./input/data/all_dis_cov2_last.xlsx", "NT")

# OF
of_tbl <- process_one("./input/data/all_dis_cov2_last.xlsx", "OF")

# Combine management-specific summaries
quant_table_main_all <- bind_rows(af_tbl, cc_tbl, nt_tbl, of_tbl)

# ---- WRB & Landform per management type -------------------------------------

df_wl <- read_xlsx("./input/data/all_dis_cov3.xlsx", guess_max = 1000) %>%
  filter(key %in% c("AF","CC","NT","OF"), !Crop_Group %in% "Grass") %>%
  select(effectSize, key, Crop_Group, wrb, landform) %>%
  drop_na(Crop_Group) %>%
  mutate(Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others", "V_F_others", Crop_Group),
         ES = perc(effectSize)) %>%
  filter(!ES > 100) %>%
  select(-effectSize) %>%
  rename(effectSize = ES,
         Management_type = key)

# Collapse landform codes to grouped "Class"
df_wl$Class <- with(df_wl, ifelse(landform == 0, 0,
                           ifelse(landform == 1, 1,
                           ifelse(landform == 2, 2,
                           ifelse(landform == 3, 3,
                           ifelse(landform == 4, 4,
                           ifelse(landform == 5, 5,
                           ifelse(landform == 6, 6,
                           ifelse(landform == 7, 7,
                           ifelse(landform == 8, 8,
                           ifelse(landform == 9, 9,
                           ifelse(landform == 10, 9,
                           ifelse(landform == 11, 11,
                           ifelse(landform == 12, 11,
                           ifelse(landform == 13, 13,
                           ifelse(landform == 14, 13,
                           ifelse(landform == 15, 15,
                           ifelse(landform == 16, 15,
                           ifelse(landform == 17, 17,
                           ifelse(landform == 18, 17,
                           ifelse(landform == 19, 19,
                           ifelse(landform == 20, 19,
                           ifelse(landform == 21, 21,
                           ifelse(landform == 22, 21, 0))))))))))))))))))))))))

# WRB by management
q_wrb_mgt <- summarise_by2(df_wl, Management_type, wrb,      "WRB_soil_type")

# Landform by management (skip code=0)
l_df       <- df_wl %>% filter(Class != 0) %>% mutate(landform = as.factor(Class))
q_land_mgt <- summarise_by2(l_df,  Management_type, landform, "Landform")

# Combine everything
quant_table_all <- bind_rows(
  quant_table_main_all,
  q_wrb_mgt %>% mutate(Management_type = as.character(Management_type)),
  q_land_mgt %>% mutate(Management_type = as.character(Management_type))
)

# ---- Add explicit LABEL column (code → label maps) --------------------------

quant_table_all_with_labels <- quant_table_all %>%
  mutate(
    cov   = as.character(cov),
    cat   = as.character(cat),
    code  = cat,
    label = cat
  ) %>%
  left_join(label_map, by = c("cov" = "cov", "code" = "code"),
            suffix = c("", "_mapped")) %>%
  mutate(label = ifelse(!is.na(label_mapped), label_mapped, label)) %>%
  select(Management_type, cov, cat, label, n, mean, median, p25, p75, ci_low, ci_high)

# ---- Write outputs -----------------------------------------------------------
dir.create("./output/mean_ci", recursive = TRUE, showWarnings = FALSE)
write_xlsx(quant_table_all,             "./output/mean_ci/percentiles_mean_ci_by_mgt.xlsx")
write_xlsx(quant_table_all_with_labels, "./output/mean_ci/percentiles_mean_ci_by_mgt_with_labels.xlsx")

# Return data.frame if running interactively
as.data.frame(quant_table_all_with_labels)

head(as.data.frame(quant_table_all_with_labels), n=100)
