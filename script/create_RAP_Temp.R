# ---- Dependencies ----
library(dplyr)
library(readxl)
library(stringr)
library(forcats)
library(writexl)

# ---------- helper: text normalizer (spaces/dashes) ----------
.norm_txt <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[\u2013\u2014]", "-") %>%  # en/em dash -> hyphen
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# ---------- helper: pick the first existing column from a set ----------
.pick_col <- function(d, ...) {
  nms <- c(...)
  hit <- nms[nms %in% names(d)]
  if (length(hit) == 0) return(rep(NA, nrow(d)))
  d[[hit[1]]]
}

# ---------- main transformer ----------
to_figure2_schema <- function(
  data,                          # data.frame OR path to .xlsx (AF-style)
  template_path,                 # path to Figure2.xlsx (template)
  out_path = NULL,               # optional: write transformed table here
  cov_map = NULL,                # optional named vector: input cov -> template variable
  verbose = TRUE
) {
  # --- read input data if a path ---
  if (is.character(data)) {
    stopifnot(file.exists(data))
    data_in <- read_xlsx(data)
  } else if (inherits(data, "data.frame")) {
    data_in <- data
  } else {
    stop("`data` must be a data.frame or path to an .xlsx file.")
  }

  # --- read template (Figure2) ---
  stopifnot(file.exists(template_path))
  fig <- read_xlsx(template_path) %>%
    mutate(
      variable_tmpl = .norm_txt(variable),
      level_tmpl    = .norm_txt(level)
    )

  # component/variable order from the template
  comp_order <- fig %>% distinct(component) %>% pull(component)
  var_order_tbl <- fig %>%
    distinct(component, variable_tmpl) %>%
    group_by(component) %>%
    mutate(var_rank = row_number()) %>%
    ungroup()

  lev_order <- fig %>% select(variable_tmpl, level_tmpl, order)
  var2comp  <- fig %>% distinct(variable_tmpl, component)

  # --- standardize incoming columns (AF-like) ---
  # Accept common synonyms: cov|variable, cat1|level|cat, mean|est, ci_low|lwr, ci_high|upr
  std <- tibble(
    cov_in  = .pick_col(data_in, "cov", "variable", "Variable", "group"),
    level   = .pick_col(data_in, "level", "cat1", "cat", "Level", "label"),
    est     = .pick_col(data_in, "est", "mean", "Estimate"),
    lwr     = .pick_col(data_in, "lwr", "ci_low", "lower", "ci.low"),
    upr     = .pick_col(data_in, "upr", "ci_high", "upper", "ci.high")
  )

  if (all(is.na(std$cov_in)))  stop("Could not find a 'cov' or 'variable' column in input.")
  if (all(is.na(std$level)))   stop("Could not find a 'level/cat1' column in input.")
  if (all(is.na(std$est)))     stop("Could not find 'est/mean' column in input.")
  if (all(is.na(std$lwr)))     stop("Could not find 'lwr/ci_low' column in input.")
  if (all(is.na(std$upr)))     stop("Could not find 'upr/ci_high' column in input.")

  # --- default mapping (override with cov_map if provided) ---
  default_cov_map <- c(
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
  cov_map <- if (is.null(cov_map)) default_cov_map else c(default_cov_map, cov_map)

  # --- build Figure2-like columns from AF-like input ---
  af_std <- std %>%
    mutate(
      variable = recode(cov_in, !!!cov_map, .default = cov_in),
      variable_tmpl = .norm_txt(variable),
      level_tmpl    = .norm_txt(level)
    )

  # --- join component + order from template; robust fallbacks ---
  af_join <- af_std %>%
    left_join(var2comp,  by = "variable_tmpl") %>%                       # add component
    left_join(lev_order, by = c("variable_tmpl", "level_tmpl")) %>%      # add template order
    group_by(variable_tmpl) %>%
    mutate(
      # fill missing component within a variable group (if some matches found)
      component = coalesce(component, first(component[!is.na(component)])),
      # fallback order: append unmatched levels after the max template order
      max_ord   = ifelse(all(is.na(order)), 0L, max(order, na.rm = TRUE)),
      seq_fallback = row_number(),
      order = ifelse(is.na(order), max_ord + seq_fallback, order)
    ) %>%
    ungroup() %>%
    select(-max_ord, -seq_fallback) %>%
    left_join(var_order_tbl, by = c("component", "variable_tmpl"))

  # --- report unmatched labels (for quick fixups) ---
  unmatched <- af_join %>%
    filter(is.na(var_rank) | is.na(component)) %>%
    distinct(variable, level) %>% arrange(variable, level)

  if (verbose && nrow(unmatched) > 0) {
    message("Heads-up: some (variable, level) didn't fully match the Figure2 template.\n",
            "Consider fixing spelling/case or extending `cov_map`.\n")
    print(unmatched)
  }

  # --- final Figure2 schema + ordering identical to template where possible ---
  af_fig2 <- af_join %>%
    transmute(
      component = component,
      variable  = variable,
      level     = level,
      est       = est,
      lwr       = lwr,
      upr       = upr,
      order     = as.integer(order),
      var_rank  = var_rank
    ) %>%
    mutate(
      component = factor(component, levels = comp_order)
    ) %>%
    arrange(is.na(component), component, is.na(var_rank), var_rank, desc(order)) %>%
    select(component, variable, level, est, lwr, upr, order)

  # --- optionally write out ---
  if (!is.null(out_path)) {
    write_xlsx(af_fig2, out_path)
    if (verbose) message("Wrote: ", out_path)
  }

  # return both the transformed table and the unmatched report
  return(list(data = af_fig2, unmatched = unmatched))
}

# paths
af<-"./output/mean_ci/AF_clim/AF_mean_ci_plot_data.xlsx"
cc<-"./output/mean_ci/CC_clim/CC_mean_ci_plot_data.xlsx"
nt<-"./output/mean_ci/NT_clim/NT_mean_ci_plot_data.xlsx"
of<-"./output/mean_ci/OF_clim/OF_mean_ci_plot_data.xlsx"
ref<-read_xlsx(template_path)

template_path <- "C:/Users/hounkpk1/RAP_Drivers/output/Figure_data/Figure2.xlsx"
af_path       <- "./output/mean_ci/AF_clim/AF_mean_ci_plot_data.xlsx"
out_path      <- "./output/mean_ci/NT_clim/NT_mean_ci_plot_data__FIG2_format.xlsx"

res <- to_figure2_schema(
  data = cc,
  template_path = template_path,
  out_path = out_path,   # write Excel
  verbose = TRUE
)

# The transformed table ready for plotting:
af_fig2 <- res$data
head(af_fig2)

as.data.frame(af_fig2)

# Any labels that didn’t match the template (fix spelling/case or extend cov_map):
res$unmatched

as.data.frame(ref)
