################################################################################
# Tidy summaries of yield effects by climate × management and management combos
# Outputs:
# - ./output/mean_ci/climate_by_management/<MGT>_<CLIMATE>_management.xlsx
# - ./output/mean_ci/climate_by_management_ALL.xlsx            (with helper col)
# - ./output/mean_ci/climate_by_management_ALL_corecols.xlsx   (core columns)
#
# Columns:
#   Management_type, cov, cat, label, n, mean, median, p25, p75, ci_low, ci_high
#
# Notes:
# - effectSize expected to be a log response ratio; converted to percent change.
# - “management combos” built from N_input, soil_cover, weed_control, rotation.
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(boot)
  library(writexl)
  library(tibble)
})

# ----------------------------- Settings ---------------------------------------

mgts          <- c("AF","CC","NT","OF")
climate_zones <- c("Arid","Temperate","Continental","Tropical")

# Data sources in your project
path_cc   <- "./input/data/all_dis_cov2.xlsx"       # CC lives here
path_rest <- "./input/data/all_dis_cov2_last.xlsx"  # AF, NT, OF live here

# Output dirs
dir.create("./output/mean_ci", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/mean_ci/climate_by_management", recursive = TRUE, showWarnings = FALSE)

# Bootstrap parameters
BOOT_R  <- 1000
CONF_LEV <- 0.95

# ----------------------------- Helpers ----------------------------------------

# Convert log response ratio to percent change
perc <- function(x) 100 * (exp(x) - 1)

# Robust mean CI via bootstrap (BCa; fallback to percentile CI if needed)
summary_stats <- function(x, R = BOOT_R, conf = CONF_LEV) {
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

# Sanitize Yes/No to "yes"/"no" and build a readable combo label
make_mgt_combo <- function(df) {
  df %>%
    mutate(
      across(c(N_input, soil_cover, weed_control, rotation),
             ~ case_when(
               . %in% c("Yes","yes","YES", 1, "1", TRUE) ~ "yes",
               . %in% c("No","no","NO", 0, "0", FALSE)    ~ "no",
               TRUE ~ NA_character_
             )
      )
    ) %>%
    mutate(
      combo = case_when(
        N_input=="yes" & soil_cover=="no"  & weed_control=="no"  & rotation=="no"  ~ "only N_input",
        N_input=="no"  & soil_cover=="yes" & weed_control=="no"  & rotation=="no"  ~ "only soil_cover",
        N_input=="no"  & soil_cover=="no"  & weed_control=="yes" & rotation=="no"  ~ "only weed_control",
        N_input=="no"  & soil_cover=="no"  & weed_control=="no"  & rotation=="yes" ~ "only rotation",
        N_input=="yes" & soil_cover=="yes" & weed_control=="no"  & rotation=="no"  ~ "N_input & soil_cover",
        N_input=="yes" & soil_cover=="no"  & weed_control=="yes" & rotation=="no"  ~ "N_input & weed_control",
        N_input=="yes" & soil_cover=="no"  & weed_control=="no"  & rotation=="yes" ~ "N_input & rotation",
        N_input=="no"  & soil_cover=="yes" & weed_control=="yes" & rotation=="no"  ~ "soil_cover & weed_control",
        N_input=="no"  & soil_cover=="yes" & weed_control=="no"  & rotation=="yes" ~ "soil_cover & rotation",
        N_input=="no"  & soil_cover=="no"  & weed_control=="yes" & rotation=="yes" ~ "weed_control & rotation",
        N_input=="yes" & soil_cover=="yes" & weed_control=="yes" & rotation=="no"  ~ "N_input & soil_cover & weed_control",
        N_input=="yes" & soil_cover=="no"  & weed_control=="yes" & rotation=="yes" ~ "N_input & weed_control & rotation",
        N_input=="no"  & soil_cover=="yes" & weed_control=="yes" & rotation=="yes" ~ "soil_cover & weed_control & rotation",
        N_input=="yes" & soil_cover=="yes" & weed_control=="yes" & rotation=="yes" ~ "N_input & soil_cover & weed_control & rotation",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(combo))
}

# Read data for one management type; CC is stored separately from AF/NT/OF
read_data_by_mgt <- function(mgt) {
  path <- if (mgt == "CC") path_cc else path_rest
  read_xlsx(path, guess_max = 1000) %>%
    filter(key %in% mgt, !Crop_Group %in% "Grass") %>%
    select(effectSize, key, kg_clim, Crop_Group, N_input, soil_cover, weed_control, rotation) %>%
    tidyr::drop_na(Crop_Group, kg_clim) %>%
    mutate(ES_perc = perc(effectSize)) %>%
    select(-effectSize) %>%
    rename(Management_type = key)
}

# Summarise one (management, climate) subset: overall + management combos
summarise_one_climate <- function(df_mc, mgt, clim, R = BOOT_R, conf = CONF_LEV) {
  # Always return the same columns, even if empty
  empty_schema <- tibble(
    Management_type = character(),
    cov   = character(),
    cat   = character(),
    label = character(),
    n     = double(),
    mean  = double(),
    median= double(),
    p25   = double(),
    p75   = double(),
    ci_low= double(),
    ci_high= double()
  )
  if (nrow(df_mc) == 0) return(empty_schema)

  overall <- summary_stats(df_mc$ES_perc, R = R, conf = conf) %>%
    dplyr::mutate(
      Management_type = mgt,
      cov   = "management",
      cat   = "overall",
      label = "overall",
      .before = 1
    )

  by_combo <- df_mc %>%
    dplyr::group_by(combo) %>%
    dplyr::group_modify(~ summary_stats(.x$ES_perc, R = R, conf = conf)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Management_type = mgt,
      cov   = "management",
      cat   = combo,
      label = combo,
      n, mean, median, p25, p75, ci_low, ci_high
    )

  dplyr::bind_rows(overall, by_combo) %>%
    dplyr::mutate(
      cat   = as.character(.data$cat),
      label = as.character(.data$label)
    )
}


# ----------------------------- Run --------------------------------------------
all_out <- list()

for (mgt in mgts) {
  df_mgt <- read_data_by_mgt(mgt)

  for (clim in climate_zones) {
    df_mc <- df_mgt %>%
      dplyr::filter(kg_clim == clim) %>%
      make_mgt_combo()

    out_tbl <- summarise_one_climate(df_mc, mgt, clim, R = BOOT_R, conf = CONF_LEV)

    if (nrow(out_tbl) == 0) {
      all_out[[paste(mgt, clim, sep = "_")]] <- out_tbl
      next
    }

    out_tbl <- out_tbl %>%
      dplyr::mutate(cat = as.character(.data$cat),
                    label = as.character(.data$label)) %>%
      dplyr::mutate(
        cat   = paste0(clim, ifelse(.data$cat == "overall", "", paste0(" & ", .data$cat))),
        label = .data$cat
      )

    all_out[[paste(mgt, clim, sep = "_")]] <- out_tbl
  }
}


# Mega table with helper column for pure climate zone
mega_tbl <- bind_rows(all_out) %>%
  mutate(
    climate_zone = stringr::str_replace(cat, "\\s*&.*$", "")
  ) %>%
  select(Management_type, cov, cat, label, n, mean, median, p25, p75, ci_low, ci_high, climate_zone)

as.data.frame(mega_tbl )

# Optional: rounding and simple cleanup
mega_tbl_clean <- mega_tbl %>%
  filter(is.finite(mean), is.finite(median)) %>%
  mutate(across(c(mean, median, p25, p75, ci_low, ci_high), ~ round(., 2)))

# Write mega outputs
writexl::write_xlsx(mega_tbl_clean, "./output/mean_ci/climate_by_management_ALL.xlsx")
writexl::write_xlsx(
  mega_tbl_clean %>% select(Management_type, cov, cat, label, n, mean, median, p25, p75, ci_low, ci_high),
  "./output/mean_ci/climate_by_management_ALL_corecols.xlsx"
)

message("Done. Files written to ./output/mean_ci/")
