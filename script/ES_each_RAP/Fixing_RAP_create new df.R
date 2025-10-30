# ---- Packages ----
library(dplyr)
library(readxl)
library(stringr)
library(forcats)
library(writexl)

# ---- Paths (edit if needed) ----
fig_path <- "C:/Users/hounkpk1/RAP_Drivers/output/Figure_data/Figure2.xlsx"  # template used in your plots
af_path  <- "./output/mean_ci/AF_clim/AF_mean_ci_plot_data.xlsx"             # your AF data
out_path <- "./output/mean_ci/AF_clim/AF_mean_ci_plot_data__FIG2_format.xlsx"

# ---- Normalizer for matching text across files ----
norm_txt <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[\u2013\u2014]", "-") %>%  # en/em dash -> hyphen
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# ---- Read template (Figure2) and prep lookup keys ----
fig <- read_xlsx(fig_path) %>%
  mutate(
    variable_tmpl = norm_txt(variable),
    level_tmpl    = norm_txt(level)
  )

# Component order and variable order (to replicate Figure2’s ordering)
comp_order <- fig %>% distinct(component) %>% pull(component)
var_order_tbl <- fig %>%
  distinct(component, variable_tmpl) %>%
  group_by(component) %>%
  mutate(var_rank = row_number()) %>%
  ungroup()

# Pull order per (variable, level) from template
lev_order <- fig %>% select(variable_tmpl, level_tmpl, order)

# Map variable -> component
var2comp <- fig %>% distinct(variable_tmpl, component)

# ---- Read AF data ----
# Expected columns (based on your paste): cov, cat, Type, mean, ci_low, ci_high, cat1, no, ID
af <- read_xlsx(af_path)

# ---- Map AF cov -> Figure2 variable names ----
cov_map <- c(
  "GDD_maize"     = "GDD Maize",
  "GDD_rice"      = "GDD Rice",
  "GDD_soybean"   = "GDD Soybean",
  "GDD_wheat"     = "GDD Wheat",
  "aridity index" = "Aridity",
  "no_cov"        = "Climatic regions",
  "slope_class"   = "Slope",
  "dem"           = "Elevation",
  "landform"      = "Landform",
  "texture"       = "Texture",
  "bd"            = "BD",
  "phosphorus"    = "Phosphorus",
  "soc"           = "SOC",
  "pH"            = "pH",
  "crop"          = "Crops",
  "RAP"           = "RAP"
)

af_std <- af %>%
  transmute(
    variable = recode(cov, !!!cov_map, .default = cov),
    level    = cat1,
    est      = mean,
    lwr      = ci_low,
    upr      = ci_high
  ) %>%
  mutate(
    variable_tmpl = norm_txt(variable),
    level_tmpl    = norm_txt(level)
  )

# ---- Join template component + order; fallbacks for non-matching labels ----
af_join <- af_std %>%
  left_join(var2comp,  by = "variable_tmpl") %>%           # bring component
  left_join(lev_order, by = c("variable_tmpl", "level_tmpl")) %>%  # bring order
  group_by(variable_tmpl) %>%
  mutate(
    # If component is partially missing within a variable, propagate known value
    component = coalesce(component, first(component[!is.na(component)])),
    # If order is missing for some levels (label mismatches), use within-variable sequence
    order = ifelse(is.na(order), seq_len(dplyr::n()), order)
  ) %>%
  ungroup() %>%
  left_join(var_order_tbl, by = c("component", "variable_tmpl"))

# ---- Check any (variable, level) that didn’t match template labels ----
unmatched <- af_join %>% filter(is.na(order) | is.na(var_rank) | is.na(component)) %>%
  select(variable, level) %>% distinct()
if (nrow(unmatched) > 0) {
  message("Heads-up: Some (variable, level) pairs didn’t fully match Figure2. Fix label spelling/case if needed:")
  print(unmatched)
}

# ---- Final table in Figure2 schema + ordering identical to Figure2 ----
af_fig2 <- af_join %>%
  transmute(
    component = component,
    variable  = variable,   # keep original human-readable variable
    level     = level,
    est       = est,
    lwr       = lwr,
    upr       = upr,
    order     = as.integer(order)
  ) %>%
  mutate(
    component = fct_relevel(component, comp_order)
  ) %>%
  # Order rows by component (Figure2 order), variable (Figure2 order), then your 'order' (desc like template)
  left_join(var_order_tbl, by = c("component", "variable" = "variable_tmpl")) %>%
  arrange(component, var_rank, desc(order)) %>%
  select(component, variable, level, est, lwr, upr, order)

# ---- Save output for plotting with your existing code ----
write_xlsx(af_fig2, out_path)
message("Wrote: ", out_path)

as.data.frame(af_fig2)
# ---- (Optional) Preview ----
print(af_fig2, n = nrow(af_fig2))
