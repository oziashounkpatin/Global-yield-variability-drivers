################################################################################
# NT • Arid • management-combo summaries
# Output columns:
#   Management_type, cov, cat, label, n, mean, median, p25, p75, ci_low, ci_high
# Notes:
#   - Stats are computed on log scale + BCa CI for mean; THEN converted with perc()
#   - Uses your ID scheme for combinations of four practices
################################################################################

# --- Libraries (qualify dplyr/tidyr to avoid plyr conflicts) -----------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(ggplot2)
  library(magrittr)
  library(stringr)
  library(boot)
  library(writexl)
  library(reshape2)
  # The user loads many more; safe to keep but not required here:
  # library(bootES); library(rcompanion); library(plotrix); library(plyr)
  # library(fuzzyjoin); library(janitor); library(readr)
})

# --- Helpers ------------------------------------------------------------------
# Convert log response ratio -> percent change
perc <- function(x) 100 * (exp(x) - 1)

# Non-boot summaries on LOG scale
stat_summ <- function(x) {
  x <- x[is.finite(x)]
  dplyr::tibble(
    n      = length(x),
    mean   = if (length(x)) mean(x) else NA_real_,
    median = if (length(x)) stats::median(x) else NA_real_,
    p25    = if (length(x)) as.numeric(stats::quantile(x, 0.25, names = FALSE)) else NA_real_,
    p75    = if (length(x)) as.numeric(stats::quantile(x, 0.75, names = FALSE)) else NA_real_
  )
}

# BCa (fallback: percentile) CI for mean on LOG scale
boot_mean_ci <- function(x, R = 1000, conf = 0.95, seed = 7) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(dplyr::tibble(ci_low = NA_real_, ci_high = NA_real_))
  set.seed(seed)
  my.mean <- function(xx, i) mean(xx[i])
  b <- boot::boot(x, statistic = my.mean, R = R)
  bca <- try(boot::boot.ci(b, type = "bca", conf = conf), silent = TRUE)
  if (!inherits(bca, "try-error") && !is.null(bca$bca)) {
    return(dplyr::tibble(ci_low = bca$bca[4], ci_high = bca$bca[5]))
  } else {
    perc_ci <- try(boot::boot.ci(b, type = "perc", conf = conf), silent = TRUE)
    ci_low  <- if (!inherits(perc_ci, "try-error") && !is.null(perc_ci$percent)) perc_ci$percent[4] else NA_real_
    ci_high <- if (!inherits(perc_ci, "try-error") && !is.null(perc_ci$percent)) perc_ci$percent[5] else NA_real_
    return(dplyr::tibble(ci_low = ci_low, ci_high = ci_high))
  }
}

# --- Data: NT in Arid, plus your management-combo IDs ------------------------
df <- readxl::read_xlsx("./input/data/all_dis_cov2.xlsx", guess_max = 1000) %>%
  dplyr::filter(key %in% c("NT"),
                !Crop_Group %in% c("Grass"),
                kg_clim %in% c("Arid")) %>%
  dplyr::select(effectSize, key, Crop_Group, N_input, soil_cover, weed_control, rotation) %>%
  tidyr::drop_na(Crop_Group)

# Optional: a quick histogram (log scale) just like yours
tr_theme1 <- ggplot2::theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent',color=NA),
  legend.key=element_blank(),
  legend.box.background = element_rect(fill='transparent'),
  axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1)
)

hist_nt <- ggplot2::ggplot(df, ggplot2::aes(x = effectSize)) +
  ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density)),
                          colour = "black", fill = "white") +
  ggplot2::stat_function(fun = dnorm,
                         args = list(mean = mean(df$effectSize, na.rm = TRUE),
                                     sd   = stats::sd(df$effectSize, na.rm = TRUE))) +
  tr_theme1
# print(hist_nt)  # uncomment to view

# Normalize yes/no flags
soil_man <- df %>%
  dplyr::mutate(dplyr::across(c(N_input, soil_cover, weed_control, rotation), as.factor)) %>%
  dplyr::mutate(
    N_input     = dplyr::recode(N_input, No = "no", Yes = "yes"),
    soil_cover  = dplyr::recode(soil_cover, No = "no", Yes = "yes"),
    weed_control= dplyr::recode(weed_control, No = "no", Yes = "yes"),
    rotation    = dplyr::recode(rotation, No = "no", Yes = "yes")
  )

# Combination IDs (your case_when)
soil_man_id <- soil_man %>%
  dplyr::mutate(
    ID = dplyr::case_when(
      N_input=="yes" & soil_cover=="no"  & weed_control=="no" & rotation=="no" ~ "only N_input",
      N_input=="no"  & soil_cover=="yes" & weed_control=="no" & rotation=="no" ~ "only soil_cover",
      N_input=="no"  & soil_cover=="no"  & weed_control=="yes" & rotation=="no" ~ "only weed_control",
      N_input=="no"  & soil_cover=="no"  & weed_control=="no" & rotation=="yes" ~ "only rotation",

      N_input=="yes" & soil_cover=="yes" & weed_control=="no" & rotation=="no" ~ "N_input & soil_cover",
      N_input=="yes" & soil_cover=="no"  & weed_control=="yes" & rotation=="no" ~ "N_input & weed_control",
      N_input=="yes" & soil_cover=="no"  & weed_control=="no" & rotation=="yes" ~ "N_input & rotation",
      N_input=="no"  & soil_cover=="yes" & weed_control=="yes" & rotation=="no" ~ "soil_cover & weed_control",
      N_input=="no"  & soil_cover=="yes" & weed_control=="no" & rotation=="yes" ~ "soil_cover & rotation",
      N_input=="no"  & soil_cover=="no"  & weed_control=="yes" & rotation=="yes" ~ "weed_control & rotation",

      N_input=="yes" & soil_cover=="yes" & weed_control=="yes" & rotation=="no" ~ "N_input & soil_cover & weed_control",
      N_input=="yes" & soil_cover=="no"  & weed_control=="yes" & rotation=="yes" ~ "N_input & weed_control & rotation",
      N_input=="no"  & soil_cover=="yes" & weed_control=="yes" & rotation=="yes" ~ "soil_cover & weed_control & rotation",

      N_input=="yes" & soil_cover=="yes" & weed_control=="yes" & rotation=="yes" ~ "N_input & soil_cover & weed_control & rotation",
      TRUE ~ NA_character_
    )
  ) %>%
  tidyr::drop_na(ID)

# Long form of effect sizes (LOG scale), trim like your original (<= 2.5)
soil_long <- reshape2::melt(soil_man_id, id.vars = c("ID","key"),
                            measure.vars = c("effectSize"))
soil_long <- soil_long %>% dplyr::filter(value <= 2.5)

# Keep NT only, exclude combos you filtered out
soil_long_nt <- soil_long %>%
  dplyr::filter(key %in% c("NT")) %>%
  dplyr::filter(!ID %in% c("only N_input", "only rotation",
                           "soil_cover & rotation", "weed_control & rotation"))

# --- Summaries on LOG scale, then convert to percent --------------------------
# Overall row (Arid, NT)
overall_stats_log <- stat_summ(soil_long_nt$value) %>%
  dplyr::bind_cols(boot_mean_ci(soil_long_nt$value)) %>%
  dplyr::mutate(
    Management_type = "NT",
    cov   = "overall",
    cat   = "Arid",
    label = cat,
    .before = 1
  )

# Per-combination rows
by_id_stats_log <- soil_long_nt %>%
  dplyr::group_by(ID) %>%
  dplyr::group_modify(~ dplyr::bind_cols(
    stat_summ(.x$value),
    boot_mean_ci(.x$value)
  )) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    Management_type = "NT",
    cov   = ID,
    cat   = "Arid",
    label = cat,
    n, mean, median, p25, p75, ci_low, ci_high
  )

# Bind and convert *all* summary columns to percent at the END
nt_arid_mgt <- dplyr::bind_rows(overall_stats_log, by_id_stats_log) %>%
  dplyr::mutate(dplyr::across(c(mean, median, p25, p75, ci_low, ci_high), perc)) %>%
  dplyr::arrange(factor(cov, levels = c("overall")), cov)

# Inspect
nt_arid_mgt

# Save
dir.create("./output/mean_ci/NT_clim", recursive = TRUE, showWarnings = FALSE)
writexl::write_xlsx(nt_arid_mgt, "./output/mean_ci/NT_clim/NT_Arid_management_with_quantiles.xlsx")
