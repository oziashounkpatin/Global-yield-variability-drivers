# ======================================================================
# Figure 5 only: Crops + Climate by practice
# X-axis fixed to -40 / +40% for better visibility of smaller effects
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
out_dir <- "./output/Figure_5_only/"
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
# MINIMAL FIX: keep mean when k < 3, but return NA CI
boot_mean_ci_study <- function(data, study_col = "study_id",
                               value_col = "effectSize",
                               R = 1000, conf = 0.95) {
  data <- data %>% filter(is.finite(.data[[value_col]]), !is.na(.data[[study_col]]))
  studs <- unique(data[[study_col]])
  k <- length(studs)

  est <- mean(data[[value_col]], na.rm = TRUE)

  if (k < 3) {
    return(list(mean = as.numeric(est),
                ci_low = NA_real_,
                ci_high = NA_real_,
                k = k))
  }

  re_mean <- function(study_ids) {
    idx <- unlist(lapply(study_ids, function(s) which(data[[study_col]] == s)), use.names = FALSE)
    mean(data[[value_col]][idx])
  }

  set.seed(7)
  boot_vals <- replicate(R, {
    s_star <- sample(studs, k, replace = TRUE)
    re_mean(s_star)
  })

  qs  <- quantile(boot_vals, probs = c((1 - conf)/2, 1 - (1 - conf)/2), na.rm = TRUE)
  list(mean = as.numeric(est),
       ci_low = as.numeric(qs[1]),
       ci_high = as.numeric(qs[2]),
       k = k)
}

# safe numeric getter
safe_num <- function(d, nm) {
  n <- nrow(d)
  if (nm %in% names(d)) suppressWarnings(as.numeric(d[[nm]])) else rep(NA_real_, n)
}

# --- Base filtering + ES conversion -----------------------------------
df_raw <- df_raw0 %>%
  filter(key %in% c("AF","CC","NT","OF"),
         !Crop_Group %in% c("Grass")) %>%
  mutate(
    Crop_Group = ifelse(Crop_Group == "Veg&Fruit and others", "V_F_others", Crop_Group)
    # effectSize = perc(effectSize)
  ) %>%
  filter(is.finite(effectSize), effectSize <= 100)

# optional export
write_xlsx(df_raw, "./input/new_scps_data.xlsx")

# --- Build study_id ----------------------------------------------------
df_raw <- df_raw %>%
  mutate(
    Year_from_ref   = purrr::map_int(references, extract_year_from_ref),
    Year_final      = dplyr::coalesce(Year_from_ref, suppressWarnings(as.integer(Year))),
    references_norm = normalize_reference(references),
    Crop_key        = dplyr::coalesce(as.character(Crop), as.character(Crop)),
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
gdd_rcl <- matrix(c(0,800,1,  800,2700,2,  2700,4000,3,  4000,6000,4,  6000,10000,5),
                  ncol = 3, byrow = TRUE)

aridity_rcl <- matrix(c(0,0.05,1,  0.05,0.20,2,  0.20,0.50,3,  0.50,0.65,4,  0.65,1.00,5),
                      ncol = 3, byrow = TRUE)

# --- Build classes needed for Figure 5 --------------------------------
ph_col <- intersect(c("ph","pH","soil_pH"), names(df_raw))
if (length(ph_col) == 0) stop("No pH column found (looked for 'ph', 'pH', 'soil_pH').")
ph_col <- ph_col[1]

df_classed <- df_raw %>%
  mutate(
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
    gdd_maize_band   = factor(c("<0.8","0.8-2.7","2.7-4","4-6","6-10")[as.integer(gdd_maize_class_num)],
                              levels = c("<0.8","0.8-2.7","2.7-4","4-6","6-10")),
    gdd_wheat_band   = factor(c("<0.8","0.8-2.7","2.7-4","4-6","6-10")[as.integer(gdd_wheat_class_num)],
                              levels = c("<0.8","0.8-2.7","2.7-4","4-6","6-10")),
    gdd_rice_band    = factor(c("<0.8","0.8-2.7","2.7-4","4-6","6-10")[as.integer(gdd_rice_class_num)],
                              levels = c("<0.8","0.8-2.7","2.7-4","4-6","6-10")),
    gdd_soybean_band = factor(c("<0.8","0.8-2.7","2.7-4","4-6","6-10")[as.integer(gdd_soybean_class_num)],
                              levels = c("<0.8","0.8-2.7","2.7-4","4-6","6-10"))
  )

# Standardize only variables needed for Figure 5
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
        `1` = "Hyper-Arid", `2` = "Arid", `3` = "Semi-arid", `4` = "Sub-Humid", `5` = "Humid",
        .default = NA_character_
      ),
      levels = c("Hyper-Arid","Arid","Semi-arid","Sub-Humid","Humid")
    ),
    kg_clim = if ("kg_clim" %in% names(.)) {
      factor(kg_clim, levels = c("Arid","Continental","Temperate","Tropical"))
    } else {
      factor(NA_character_, levels = c("Arid","Continental","Temperate","Tropical"))
    },
    Crop_Group = factor(Crop_Group,
                        levels = c("Maize","Rice","Soybean","Wheat","Cereal","Cash crop","V_F_others")),
    key = factor(key, levels = c("AF","CC","NT","OF"))
  )

# --- Label map for Figure 5 only --------------------------------------
label_map <- tibble::tribble(
  ~cov,               ~component,   ~variable,
  "Crop_Group",       "Crops",      "Crop group",
  "kg_clim",          "Climate",    "Köppen–Geiger",
  "aridity_band",     "Climate",    "Aridity class",
  "gdd_maize_band",   "Climate",    "GDD (maize)",
  "gdd_wheat_band",   "Climate",    "GDD (wheat)",
  "gdd_rice_band",    "Climate",    "GDD (rice)",
  "gdd_soybean_band", "Climate",    "GDD (soybean)"
)

# --- Plot helpers ------------------------------------------------------
# MINIMAL FIX: keep sparse classes
prep_for_plot <- function(df) {
  df %>%
    filter(!is.na(est)) %>%
    group_by(variable) %>%
    arrange(as.numeric(order), .by_group = TRUE) %>%
    mutate(
      label = paste0(level, " (", n_studies, ", ", n_obs, ")"),
      label = factor(label, levels = rev(unique(label)))
    ) %>%
    ungroup()
}

x_breaks_from <- function(xlims) seq(xlims[1], xlims[2], by = 10)

# --- Panel specs -------------------------------------------------------
key_labs <- c(AF = "Agroforestry", CC = "Cover crop", NT = "No-tillage", OF = "Organic farming")
keys <- names(key_labs)

# fixed x-axis requested by reviewer
xlims_fig5 <- c(-40, 40)

# variable order in Figure 5
ord_rows <- c(
  "Crop group",
  "Köppen–Geiger", "Aridity class",
  "GDD (maize)", "GDD (rice)",
  "GDD (soybean)", "GDD (wheat)"
)

# --- Compute one mega_df per practice for Figure 5 only ----------------
compute_mega_for_key <- function(key_code) {
  d_key <- df %>% dplyr::filter(key == key_code)

  # Which crop groups actually exist for this practice?
  present_crops <- d_key %>%
    dplyr::filter(!is.na(Crop_Group)) %>%
    dplyr::pull(Crop_Group) %>%
    unique()

  # Drop GDD covariates for crops absent in this practice
  drop_gdd_covs <- c(
    if (!"Maize"   %in% present_crops) "gdd_maize_band"   else NULL,
    if (!"Rice"    %in% present_crops) "gdd_rice_band"    else NULL,
    if (!"Soybean" %in% present_crops) "gdd_soybean_band" else NULL,
    if (!"Wheat"   %in% present_crops) "gdd_wheat_band"   else NULL
  )

  covariate_cols <- c(
    "Crop_Group", "kg_clim", "aridity_band",
    "gdd_maize_band", "gdd_wheat_band", "gdd_rice_band", "gdd_soybean_band"
  )

  covariate_cols <- setdiff(covariate_cols, drop_gdd_covs)
  covariate_cols_use <- intersect(covariate_cols, names(d_key))

  d_long <- d_key %>%
    dplyr::select(effectSize, study_id, references_norm, dplyr::all_of(covariate_cols_use)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(covariate_cols_use),
      names_to = "cov",
      values_to = "cat"
    ) %>%
    dplyr::filter(!is.na(cat), !is.na(study_id))

  counts_by_group <- d_long %>%
    dplyr::group_by(cov, cat) %>%
    dplyr::summarise(
      n_obs     = dplyr::n(),
      n_studies = dplyr::n_distinct(references_norm),
      .groups   = "drop"
    )

  grouped_data <- d_long %>%
    dplyr::group_by(cov, cat) %>%
    dplyr::group_split()

  results_by_group <- purrr::map_dfr(
    grouped_data,
    function(g) {
      est <- boot_mean_ci_study(
        g,
        study_col = "study_id",
        value_col = "effectSize",
        R = 1000,
        conf = 0.95
      )

      tibble::tibble(
        cov = g$cov[1],
        cat = g$cat[1],
        mean = est$mean,
        ci_low = est$ci_low,
        ci_high = est$ci_high,
        k = est$k
      )
    }
  )

  results_by_group <- results_by_group %>%
    dplyr::filter(!cov %in% drop_gdd_covs)

  results_by_group %>%
    left_join(counts_by_group, by = c("cov", "cat")) %>%
    left_join(label_map, by = "cov") %>%
    transmute(
      component = coalesce(component, "Misc"),
      variable  = coalesce(variable, cov),
      level     = as.character(cat),
      label     = paste0(cat, " (", n_studies, ", ", n_obs, ")"),
      est = mean,
      lwr = ci_low,
      upr = ci_high,
      k,
      n_obs,
      n_studies,
      order = suppressWarnings(as.integer(cat))
    )
}

mega_list <- imap(setNames(keys, keys), ~compute_mega_for_key(.x))

# export summary tables if needed
iwalk(mega_list, ~write_xlsx(.x, file.path(out_dir, paste0("mega_df_", .y, "_fig5.xlsx"))))

# --- Plot function for Figure 5 ---------------------------------------
plot_crops_climate_single_key <- function(md_key, key_title,
                                          xlims, show_var_strips = TRUE) {

  dp <- md_key %>%
    dplyr::filter(component %in% c("Crops", "Climate")) %>%
    prep_for_plot()

  vars_present <- intersect(ord_rows, unique(dp$variable))
  dp <- dp %>%
    dplyr::mutate(variable = factor(variable, levels = vars_present))

  p <- ggplot(dp, aes(x = est, y = label)) +
    geom_segment(
      data = dp %>% dplyr::filter(!is.na(lwr), !is.na(upr)),
      aes(x = lwr, xend = upr, yend = label),
      linewidth = 0.8
    ) +
    geom_point(size = 2.2) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6, color = "red") +
    facet_grid(rows = vars(variable), scales = "free_y", space = "free_y", switch = "y") +
    scale_x_continuous(
      breaks = x_breaks_from(xlims),
      minor_breaks = NULL
    ) +
    scale_y_discrete(position = "right") +
    labs(x = "% change (crop yield)", y = NULL, title = key_title) +
    coord_cartesian(xlim = xlims, clip = "off") +
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

# --- Build Figure 5 ----------------------------------------------------
p_AF_cc <- plot_crops_climate_single_key(
  mega_list$AF, key_labs["AF"],
  xlims = xlims_fig5, show_var_strips = TRUE
)

p_CC_cc <- plot_crops_climate_single_key(
  mega_list$CC, key_labs["CC"],
  xlims = xlims_fig5, show_var_strips = FALSE
)

p_NT_cc <- plot_crops_climate_single_key(
  mega_list$NT, key_labs["NT"],
  xlims = xlims_fig5, show_var_strips = FALSE
)

p_OF_cc <- plot_crops_climate_single_key(
  mega_list$OF, key_labs["OF"],
  xlims = xlims_fig5, show_var_strips = FALSE
)

fig_5 <- (p_AF_cc | p_CC_cc) / (p_NT_cc | p_OF_cc)

# --- Save output -------------------------------------------------------
file_out_fig5 <- file.path(out_dir, "Figure_final_5_crops_clim_by_practice_fixed_-40_40.pdf")

ggsave(
  file_out_fig5,
  plot = fig_5,
  device = cairo_pdf,
  width = 18,
  height = 11,
  units = "in",
  dpi = 600,
  limitsize = FALSE
)