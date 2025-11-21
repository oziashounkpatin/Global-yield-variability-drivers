# ======================================================================
# Figure panels by practice (AF, CC, NT, OF) with study-clustered bootstrap
# ======================================================================

# --- Packages ----------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(writexl)
  library(soiltexture)
  library(ggplot2)
  library(forcats)
  library(ggh4x)
  library(grid)
  library(cowplot)
  library(patchwork)
})

set.seed(7)

# --- I/O ---------------------------------------------------------------
in_path <- "./input/scps_data.xlsx"   # <-- adjust if needed
out_dir <- "./output/Figure_3_4_5/"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Read --------------------------------------------------------------
df_raw0 <- read_xlsx(in_path, guess_max = 1000)
dim(df_raw0)

# --- Helpers -----------------------------------------------------------
normalize_reference <- function(reference) {
  reference %>%
    tolower() %>%
    gsub("\\s+", " ", .) %>%
    gsub("[[:punct:]]", "", .) %>%
    trimws()
}

extract_year_from_ref <- function(reference) {
  if (is.na(reference)) return(NA_integer_)
  yrs <- stringr::str_extract_all(reference, "(?<!\\d)(19|20)\\d{2}(?!\\d)")[[1]]
  if (length(yrs) == 0) return(NA_integer_)
  suppressWarnings(as.integer(tail(yrs, 1)))
}

# ES(%) = 100*(exp(lnRR)-1)
perc <- function(x) 100 * (exp(x) - 1)

# study-cluster bootstrap of the mean (percentile CI)
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

# --- Base filtering + ES conversion -----------------------------------
df_raw <- df_raw0 %>%
  filter(key %in% c("AF","CC","NT","OF"),
         !Crop_Group %in% c("Grass")) %>%
  mutate(
    Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others", "V_F_others", Crop_Group)
    #effectSize = perc(effectSize)
  ) %>%
  filter(is.finite(effectSize), effectSize <= 100)

write_xlsx(df_raw, "./input/new_scps_data.xlsx")

# --- Build study_id ----------------------------------------------------
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
      round(x, 5),   
      round(y, 5),
      sep = "_"
    )
  )

# --- Class definitions -------------------------------------------------
# SOC
soc_rcl <- matrix(c(0,5,1,  5,10,2,  10,180,3), ncol = 3, byrow = TRUE)
# P
p_rcl   <- matrix(c(0,10.9,1,  10.9,21.4,2,  21.4,185,3), ncol = 3, byrow = TRUE)
# BD
bd_rcl  <- matrix(c(0,1.2,1,  1.2,1.47,2,  1.47,1.7,3), ncol = 3, byrow = TRUE)
# DEM
dem_rcl <- matrix(c(0,250,1,  250,1000,2,  1000,4000,3), ncol = 3, byrow = TRUE)
# Slope
slope_rcl <- matrix(c(0,0.20,1,  0.20,1.00,2,  1.00,5.00,3,  5.00,15.00,4,  15.00,80.00,5),
                    ncol = 3, byrow = TRUE)
# GDD
gdd_rcl <- matrix(c(0,800,1,  800,2700,2,  2700,4000,3,  4000,6000,4,  6000,10000,5),
                  ncol = 3, byrow = TRUE)
# Aridity
aridity_rcl <- matrix(c(0,0.05,1,  0.05,0.20,2,  0.20,0.50,3,  0.50,0.65,4,  0.65,1.00,5),
                      ncol = 3, byrow = TRUE)

# Texture (HYPRES → 3 groups)
mk_texture_class <- function(clay, silt, sand) {
  ok <- is.finite(clay) & is.finite(silt) & is.finite(sand)
  clay[!ok] <- NA_real_; silt[!ok] <- NA_real_; sand[!ok] <- NA_real_
  tri <- data.frame(CLAY = clay, SILT = silt, SAND = sand)
  tex <- TT.points.in.classes(tri.data = tri, class.sys = "HYPRES.TT") %>% as.data.frame()
  texture <- dplyr::case_when(
    tex$VF == 1 & tex$F  == 1 & tex$M == 0 & tex$MF == 0 & tex$C == 0 ~ "1",
    tex$VF == 0 & tex$F  == 1 & tex$M == 0 & tex$MF == 0 & tex$C == 0 ~ "1",
    tex$VF == 1 & tex$F  == 0 & tex$M == 0 & tex$MF == 0 & tex$C == 0 ~ "1",
    tex$VF == 0 & tex$F  == 0 & tex$M == 1 & tex$MF == 1 & tex$C == 0 ~ "2",
    tex$VF == 0 & tex$F  == 0 & tex$M == 1 & tex$MF == 0 & tex$C == 0 ~ "2",
    tex$VF == 0 & tex$F  == 0 & tex$M == 0 & tex$MF == 1 & tex$C == 0 ~ "2",
    tex$VF == 0 & tex$F  == 0 & tex$M == 0 & tex$MF == 0 & tex$C == 1 ~ "3",
    TRUE ~ NA_character_
  )
  factor(texture, levels = c("1","2","3"), labels = c("fine","medium","coarse"))
}

# Landform collapse
landform_names <- c(
  `1`="Mtn_sumt","2"="Cliff_sl","3"="Lwhi_mtn","4"="Shills_dcsl","5"="Lhgsl_steep",
  "6"="Lhgsl_mod","7"="Mtn_vs","8"="Mod_hills","9"="Tfphi_dis","11"="Tfphi_surf",
  "13"="Val_sl","15"="Tfplw_dis","17"="Tfplw_surf","19"="Hi_plain","21"="Lw_plain"
)
collapse_landform <- function(x) {
  xi <- suppressWarnings(as.integer(x))
  dplyr::recode(
    xi,
    `0`  = NA_integer_,
    `1`  = 1L,   `2`  = 2L,   `3`  = 3L,   `4`  = 4L,   `5`  = 5L,   `6`  = 6L,
    `7`  = 7L,   `8`  = 8L,   `9`  = 9L,   `10` = 9L,   `11` = 11L,  `12` = 11L,
    `13` = 13L,  `14` = 13L,  `15` = 15L,  `16` = 15L,  `17` = 17L,  `18` = 17L,
    `19` = 19L,  `20` = 19L,  `21` = 21L,  `22` = 21L,
    .default = NA_integer_
  )
}

# WRB mapping
wrb_full_cod <- c(0,1,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,20,22,23,24,26,29)
wrb_full_leg <- c("Acrisols","Albeluvisols","Alisols","Andosols","Arenosols",
                  "Calcisols","Cambisols","Chernozems","Ferralsols","Fluvisols",
                  "Gleysols","Gypsisols","Histosols","Kastanozems","Leptosols",
                  "Lixisols","Luvisols","Phaeozems","Plinthosols","Podzols",
                  "Regosols","Solonetz","Vertisols")

# safe numeric getter
safe_num <- function(d, nm) {
  n <- nrow(d)
  if (nm %in% names(d)) suppressWarnings(as.numeric(d[[nm]])) else rep(NA_real_, n)
}

# --- Build classes & standardized factors -------------------------------------
ph_col <- intersect(c("ph","pH","soil_pH"), names(df_raw))
if (length(ph_col) == 0) stop("No pH column found (looked for 'ph', 'pH', 'soil_pH').")
ph_col <- ph_col[1]

df_classed <- df_raw %>%
  mutate(
    # pH
    ph_class_num = case_when(
      .data[[ph_col]] < 6.3                          ~ 1,
      .data[[ph_col]] >= 6.3 & .data[[ph_col]] < 7.4 ~ 2,
      .data[[ph_col]] >= 7.4                         ~ 3,
      TRUE ~ NA_real_
    ),
    ph_class = factor(as.integer(ph_class_num), levels = c(1,2,3),
                      labels = c("acidic","neutral","alkaline")),
    # SOC
    soc_class_num = case_when(
      soc >= soc_rcl[1,1] & soc < soc_rcl[1,2] ~ soc_rcl[1,3],
      soc >= soc_rcl[2,1] & soc < soc_rcl[2,2] ~ soc_rcl[2,3],
      soc >= soc_rcl[3,1] & soc < soc_rcl[3,2] ~ soc_rcl[3,3],
      TRUE ~ NA_real_
    ),
    soc_class = factor(as.integer(soc_class_num), levels = c(1,2,3),
                       labels = c("<5","5-10",">10")),
    # P
    p_class_num = case_when(
      phosphorus >= p_rcl[1,1] & phosphorus < p_rcl[1,2] ~ p_rcl[1,3],
      phosphorus >= p_rcl[2,1] & phosphorus < p_rcl[2,2] ~ p_rcl[2,3],
      phosphorus >= p_rcl[3,1] & phosphorus < p_rcl[3,2] ~ p_rcl[3,3],
      TRUE ~ NA_real_
    ),
    p_class = factor(as.integer(p_class_num), levels = c(1,2,3),
                     labels = c("<10.9","10.9-21.4",">21.4")),
    # BD
    bd_class_num = case_when(
      bd >= bd_rcl[1,1] & bd < bd_rcl[1,2] ~ bd_rcl[1,3],
      bd >= bd_rcl[2,1] & bd < bd_rcl[2,2] ~ bd_rcl[2,3],
      bd >= bd_rcl[3,1] & bd < bd_rcl[3,2] ~ bd_rcl[3,3],
      TRUE ~ NA_real_
    ),
    bd_class = factor(as.integer(bd_class_num), levels = c(1,2,3),
                      labels = c("<1.20","1.20-1.47",">1.47")),
    # DEM
    dem_class_num = case_when(
      dem >= dem_rcl[1,1] & dem < dem_rcl[1,2] ~ dem_rcl[1,3],
      dem >= dem_rcl[2,1] & dem < dem_rcl[2,2] ~ dem_rcl[2,3],
      dem >= dem_rcl[3,1] & dem < dem_rcl[3,2] ~ dem_rcl[3,3],
      TRUE ~ NA_real_
    ),
    dem_class = factor(as.integer(dem_class_num), levels = c(1,2,3),
                       labels = c("<250","250-1000",">1000")),
    # Slope
    slope_class_num = case_when(
      slope >= slope_rcl[1,1] & slope < slope_rcl[1,2] ~ slope_rcl[1,3],
      slope >= slope_rcl[2,1] & slope < slope_rcl[2,2] ~ slope_rcl[2,3],
      slope >= slope_rcl[3,1] & slope < slope_rcl[3,2] ~ slope_rcl[3,3],
      slope >= slope_rcl[4,1] & slope < slope_rcl[4,2] ~ slope_rcl[4,3],
      slope >= slope_rcl[5,1] & slope < slope_rcl[5,2] ~ slope_rcl[5,3],
      TRUE ~ NA_real_
    ),
    slope_class = factor(as.integer(slope_class_num), levels = c(1,2,3,4,5),
                         labels = c("<0.20","0.2-1","1-5","5-15",">15")),
    # GDD bands
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
    gdd_maize_band   = factor(c("<0.8","0.8-2.7","2.7-4","4-6","6-10")[as.integer(gdd_maize_class_num)] ,
                              levels = c("<0.8","0.8-2.7","2.7-4","4-6","6-10")),
    gdd_wheat_band   = factor(c("<0.8","0.8-2.7","2.7-4","4-6","6-10")[as.integer(gdd_wheat_class_num)],
                              levels = c("<0.8","0.8-2.7","2.7-4","4-6","6-10")),
    gdd_rice_band    = factor(c("<0.8","0.8-2.7","2.7-4","4-6","6-10")[as.integer(gdd_rice_class_num)] ,
                              levels = c("<0.8","0.8-2.7","2.7-4","4-6","6-10")),
    gdd_soybean_band = factor(c("<0.8","0.8-2.7","2.7-4","4-6","6-10")[as.integer(gdd_soybean_class_num)],
                              levels = c("<0.8","0.8-2.7","2.7-4","4-6","6-10"))
  )

# Texture
df_classed <- df_classed %>% mutate(texture = mk_texture_class(clay, silt, sand))

# Landform (if available)
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
    wrb_code  = suppressWarnings(as.integer(wrb)),
    wrb_name  = case_when(
      is.na(wrb_code) ~ NA_character_,
      wrb_code %in% wrb_full_cod ~ wrb_full_leg[match(wrb_code, wrb_full_cod)],
      TRUE ~ NA_character_
    ),
    wrb_class = factor(wrb_name, levels = wrb_full_leg)
  )

# Standardize
AI_vec <- coalesce(
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
        `1`="Hyper-Arid", `2`="Arid", `3`="Semi-arid", `4`="Sub-Humid", `5`="Humid",
        .default = NA_character_
      ),
      levels = c("Hyper-Arid","Arid","Semi-arid","Sub-Humid","Humid")
    ),
    ph_class  = factor(ph_class,  levels = c("acidic","neutral","alkaline")),
    soc_class = factor(soc_class, levels = c("<5","5-10",">10")),
    p_class   = factor(p_class,   levels = c("<10.9","10.9-21.4",">21.4")),
    bd_class  = factor(bd_class,  levels = c("<1.20","1.20-1.47",">1.47")),
    texture   = factor(texture,   levels = c("fine","medium","coarse")),
    kg_clim   = if ("kg_clim" %in% names(.))
      factor(kg_clim, levels = c("Arid","Continental","Temperate","Tropical"))
    else factor(NA_character_, levels = c("Arid","Continental","Temperate","Tropical")),
    dem_class   = factor(dem_class,   levels = c("<250","250-1000",">1000")),
    slope_class = factor(slope_class, levels = c("<0.20","0.2-1","1-5","5-15",">15")),
    Crop_Group  = factor(Crop_Group,  levels = c("Maize","Rice","Soybean","Wheat","Cereal","Cash crop","V_F_others")),
    key         = factor(key,         levels = c("AF","CC","NT","OF"))
  )

df <- df %>%
  mutate(key_crop = ifelse(is.na(key) | is.na(Crop_Group),
                           NA_character_,
                           paste(key, Crop_Group, sep=":")))

covariate_cols <- c(
  "key","Crop_Group",             # keep originals if you still want the marginals
  "key_crop",                     # <- NEW interaction
  "kg_clim","aridity_band","ph_class","soc_class","p_class","bd_class",
  "texture","gdd_maize_band","gdd_wheat_band","gdd_rice_band","gdd_soybean_band",
  "dem_class","slope_class","wrb_class","landform_class"
)

# --- Label map ---------------------------------------------------------
label_map <- tibble::tribble(
  ~cov,               ~component,          ~variable,
  "overall",          "Management",        "Practice",
  "key",              "Management",        "Practice",
  "Crop_Group",       "Crops",             "Crop group",
  "kg_clim",          "Climate",           "Köppen–Geiger",
  "aridity_band",     "Climate",           "Aridity class",
  "ph_class",         "Soil properties",   "pH",
  "soc_class",        "Soil properties",   "SOC",
  "p_class",          "Soil properties",   "P",
  "bd_class",         "Soil properties",   "Bulk density",
  "texture",          "Soil properties",   "Texture",
  "gdd_maize_band",   "Climate",           "GDD (maize)",
  "gdd_wheat_band",   "Climate",           "GDD (wheat)",
  "gdd_rice_band",    "Climate",           "GDD (rice)",
  "gdd_soybean_band", "Climate",           "GDD (soybean)",
  "dem_class",        "Topography",        "Elevation",
  "slope_class",      "Topography",        "Slope",
  "landform_class",   "Topography",        "Landform",
  "wrb_class",        "Soil properties",   "WRB"
)

# --- Plot helpers ------------------------------------------------------
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
                           title,
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

# --- Panel specs -------------------------------------------------------
key_labs <- c(AF="Agroforestry", CC="Cover crop", NT="No-tillage", OF="Organic farming")
keys <- names(key_labs)

xlims_crops <- c(-60, 40)
xlims_soil  <- c(-20, 60)   # covers SOC upr ≈ 53
xlims_topo  <- c(-30, 50)   # covers topo range –26..47

x_breaks_from <- function(xlims) seq(xlims[1], xlims[2], by = 10)

x_lab    <- "% change (crop yield)"

climate_var_order <- c(
  "Köppen–Geiger","Aridity class",
  "GDD (maize)","GDD (wheat)",
  "GDD (rice)","GDD (soybean)"
)

soil_var_order <- c("pH", "SOC", "P", "Bulk density", "Texture", "WRB")
topo_var_order <- c("Elevation", "Slope", "Landform")

# --- Compute one mega_df per practice (GDD auto-pruned by crop presence) ---
compute_mega_for_key <- function(key_code) {
  d_key <- df %>% dplyr::filter(key == key_code)

  # Which crop groups actually exist for this practice?
  present_crops <- d_key %>%
    dplyr::filter(!is.na(Crop_Group)) %>%
    dplyr::pull(Crop_Group) %>%
    unique()

  # Decide which GDD covariates to drop
  drop_gdd_covs <- c(
    if (!"Maize"   %in% present_crops)   "gdd_maize_band"   else NULL,
    if (!"Rice"    %in% present_crops)   "gdd_rice_band"    else NULL,
    if (!"Soybean" %in% present_crops)   "gdd_soybean_band" else NULL,
    if (!"Wheat"   %in% present_crops)   "gdd_wheat_band"   else NULL
  )

  # Candidate covariates for this practice
  covariate_cols <- c(
    "Crop_Group","kg_clim","aridity_band",
    "ph_class","soc_class","p_class","bd_class","texture",
    "gdd_maize_band","gdd_wheat_band","gdd_rice_band","gdd_soybean_band",
    "dem_class","slope_class","wrb_class","landform_class"
  )

  # Remove the GDD covariates for missing crops
  covariate_cols <- setdiff(covariate_cols, drop_gdd_covs)
  covariate_cols_use <- intersect(covariate_cols, names(d_key))

  # Long format for this practice
  d_long <- d_key %>%
    dplyr::select(effectSize, study_id, references_norm,dplyr::all_of(covariate_cols_use)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(covariate_cols_use),
                        names_to = "cov", values_to = "cat") %>%
    dplyr::filter(!is.na(cat), !is.na(study_id))

  # Counts (use study_id for n_studies to match clustered bootstrap)
  counts_by_group <- d_long %>%
    dplyr::group_by(cov, cat) %>%
    dplyr::summarise(
      n_obs     = dplyr::n(),
      n_studies = dplyr::n_distinct(references_norm),
      .groups   = "drop"
    )

  # Bootstrap per (cov, cat)
  grouped_data <- d_long %>% dplyr::group_by(cov, cat) %>% dplyr::group_split()

  results_by_group <- purrr::map_dfr(
    grouped_data,
    function(g) {
      est <- boot_mean_ci_study(g, study_col = "study_id",
                                value_col = "effectSize",
                                R = 1000, conf = 0.95)
      tibble::tibble(
        cov = g$cov[1], cat = g$cat[1],
        mean = est$mean, ci_low = est$ci_low, ci_high = est$ci_high, k = est$k
      )
    }
  )

  # (Safety) If anything slipped through, drop here too
  results_by_group <- results_by_group %>%
    dplyr::filter(!cov %in% drop_gdd_covs)

  # Label + final mega_df for this practice
results_by_group %>%
  left_join(counts_by_group, by = c("cov","cat")) %>%
  left_join(label_map, by = "cov") %>%
  transmute(
    component = coalesce(component, "Misc"),
    variable  = coalesce(variable, cov),
    level     = as.character(cat),                  # for display
    label     = paste0(cat, " (", n_studies, ", ", n_obs, ")"),
    est = mean, lwr = ci_low, upr = ci_high,
    k, n_obs, n_studies,
    # >>> keep factor order (works for GDD bands already defined upstream)
    order = suppressWarnings(as.integer(cat))
  )

}


mega_list <- imap(setNames(keys, keys), ~compute_mega_for_key(.x))

# export tables
iwalk(mega_list, ~write_xlsx(.x, file.path(out_dir, paste0("mega_df_", .y, ".xlsx"))))

# bind for plotting with practice columns
mega_all_keys <- imap_dfr(mega_list, ~mutate(.x, key = .y, key_lab = recode(.y, !!!key_labs)))


# ============================================================
# Per-practice columns with their OWN right-side y labels
# (Crops+Climate, Soil properties, Topography)
# ============================================================

# helper to build a single-practice CROPS+CLIMATE column
plot_crops_climate_single_key <- function(md_key, key_title,
                                          xlims, show_var_strips = TRUE) {
  ord_rows <- c(
    "Crop group",
    "Köppen–Geiger","Aridity class",
    "GDD (maize)","GDD (rice)",
    "GDD (soybean)","GDD (wheat)"
  )

  dp <- md_key %>%
    dplyr::filter(component %in% c("Crops","Climate")) %>%
    prep_for_plot()

  # reorder variables but don't create NA’s
  vars_present <- intersect(ord_rows, unique(dp$variable))
  dp <- dp %>%
    dplyr::mutate(variable = factor(variable, levels = vars_present))

  p <- ggplot(dp, aes(x = est, y = label)) +
    geom_segment(aes(x = lwr, xend = upr, yend = label), linewidth = 0.8) +
    geom_point(size = 2.2) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6, color = "red") +
    facet_grid(rows = vars(variable), scales = "free_y", space = "free_y", switch = "y") +
    scale_x_continuous(limits = xlims,
                       breaks = x_breaks_from(xlims),
                       minor_breaks = NULL) +
    scale_y_discrete(position = "right") +
    labs(x = "% change (crop yield)", y = NULL, title = key_title) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.placement = "outside",
      strip.text.y.left = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(4, "mm"),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11)
    )

  if (!show_var_strips) {
    p <- p + theme(strip.text = element_blank(), strip.background = element_blank())
  }
  p
}



# choose any one practice
md_key <- mega_list$AF   # or compute_mega_for_key("AF")
ord_rows <- c(
  "Crop group",
  "Köppen–Geiger","Aridity class",
  "GDD (maize)","GDD (rice)",
  "GDD (soybean)","GDD (wheat)"
)

dp <- md_key %>%
  dplyr::filter(component %in% c("Crops","Climate")) %>%
  dplyr::mutate(variable = factor(variable, levels = ord_rows)) %>%
  prep_for_plot()

# helper to build a single-practice SOIL column (WRB taller)
plot_soil_single_key <- function(md_key, key_title,
                                 xlims, show_var_strips = TRUE) {

  dp <- md_key %>%
    dplyr::filter(component == "Soil properties") %>%
    prep_for_plot()

  # figure out which soil variables actually appear for this practice
  vars_present <- intersect(soil_var_order, unique(dp$variable))
  dp <- dp %>%
    dplyr::mutate(variable = factor(variable, levels = vars_present))

  p <- ggplot(dp, aes(x = est, y = label)) +
    geom_segment(aes(x = lwr, xend = upr, yend = label), linewidth = 0.8) +
    geom_point(size = 2.2) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6, color = "red") +
    facet_grid(rows = vars(variable), scales = "free_y", space = "free_y", switch = "y") +
    scale_x_continuous(limits = xlims,
                       breaks = x_breaks_from(xlims),
                       minor_breaks = NULL) +
    scale_y_discrete(position = "right") +
    labs(x = "% change (crop yield)", y = NULL, title = key_title) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.placement = "outside",
      strip.text.y.left = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(4, "mm"),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11)
    )

  # now create a rows vector that matches *vars_present*, not soil_var_order
  rows <- rep(1, length(vars_present))
  rows[vars_present == "WRB"] <- 2.5

  p <- p + ggh4x::force_panelsizes(rows = grid::unit(rows, "null"),
                                   cols = grid::unit(1, "null"))

  if (!show_var_strips) {
    p <- p + theme(strip.text = element_blank(), strip.background = element_blank())
  }
  p
}



# helper to build a single-practice TOPO column
plot_topo_single_key <- function(md_key, key_title,
                                 xlims, show_var_strips = TRUE) {

  dp <- md_key %>%
    dplyr::filter(component == "Topography") %>%
    prep_for_plot() %>%
    dplyr::filter(!is.na(variable))   # drop any NA variables

  vars_present <- intersect(topo_var_order, unique(dp$variable))
  dp <- dp %>%
    dplyr::mutate(variable = factor(variable, levels = vars_present))

  p <- ggplot(dp, aes(x = est, y = label)) +
    geom_segment(aes(x = lwr, xend = upr, yend = label), linewidth = 0.8) +
    geom_point(size = 2.2) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6, color = "red") +
    facet_grid(rows = vars(variable), scales = "free_y", space = "free_y", switch = "y") +
    scale_x_continuous(limits = xlims,
                       breaks = x_breaks_from(xlims),
                       minor_breaks = NULL) +
    scale_y_discrete(position = "right") +
    labs(x = "% change (crop yield)", y = NULL, title = key_title) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.placement = "outside",
      strip.text.y.left = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(4, "mm"),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11)
    )

  if (!show_var_strips) {
    p <- p + theme(strip.text = element_blank(), strip.background = element_blank())
  }
  p
}

p_AF_cc <- plot_crops_climate_single_key(mega_list$AF, key_labs["AF"], xlims = xlims_crops, show_var_strips = TRUE)
p_CC_cc <- plot_crops_climate_single_key(mega_list$CC, key_labs["CC"], xlims = xlims_crops, show_var_strips = FALSE)
p_NT_cc <- plot_crops_climate_single_key(mega_list$NT, key_labs["NT"], xlims = xlims_crops, show_var_strips = FALSE)
p_OF_cc <- plot_crops_climate_single_key(mega_list$OF, key_labs["OF"], xlims = xlims_crops, show_var_strips = FALSE)

p_AF_soil <- plot_soil_single_key(mega_list$AF, key_labs["AF"], xlims = xlims_soil, show_var_strips = TRUE)
p_CC_soil <- plot_soil_single_key(mega_list$CC, key_labs["CC"], xlims = xlims_soil, show_var_strips = FALSE)
p_NT_soil <- plot_soil_single_key(mega_list$NT, key_labs["NT"], xlims = xlims_soil, show_var_strips = FALSE)
p_OF_soil <- plot_soil_single_key(mega_list$OF, key_labs["OF"], xlims = xlims_soil, show_var_strips = FALSE)

# TOPOGRAPHY (shared topo xlims) 
p_AF_topo <- plot_topo_single_key(mega_list$AF, key_labs["AF"], xlims = xlims_topo, show_var_strips = TRUE) 
p_CC_topo <- plot_topo_single_key(mega_list$CC, key_labs["CC"], xlims = xlims_topo, show_var_strips = FALSE) 
p_NT_topo <- plot_topo_single_key(mega_list$NT, key_labs["NT"], xlims = xlims_topo, show_var_strips = FALSE) 
p_OF_topo <- plot_topo_single_key(mega_list$OF, key_labs["OF"], xlims = xlims_topo, show_var_strips = FALSE)

# --- Final Crop+Climate, Soil, and Topo Plots Layout ---
fig_crops_clim <- (p_AF_cc | p_CC_cc) / (p_NT_cc | p_OF_cc)
fig_soil_props <- (p_AF_soil | p_CC_soil) / (p_NT_soil | p_OF_soil)
fig_topography <- (p_AF_topo | p_CC_topo) / (p_NT_topo | p_OF_topo)


# Save outputs
file_out_cc <- file.path(out_dir, "Figure_3_crops_clim_by_practice1.pdf")
file_out_soil <- file.path(out_dir, "Figure_4_soil_properties_by_practice2.pdf")
file_out_topo <- file.path(out_dir, "Figure_5_topography_by_practice3.pdf")


walk2(
.x = list(fig_crops_clim, fig_soil_props, fig_topography),
.y = list(file_out_cc, file_out_soil, file_out_topo),
~ ggsave(.y, plot = .x, device = cairo_pdf,
width = 18, height = ifelse(.y == file_out_topo, 9, 11),
units = "in", dpi = 600, limitsize = FALSE)
)