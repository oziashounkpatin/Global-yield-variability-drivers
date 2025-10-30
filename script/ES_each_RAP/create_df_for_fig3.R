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
  data,                          # data.frame OR path to .xlsx (e.g., nt_fig_3.xlsx)
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

  # Keep display names & build lookups
  var_display <- fig %>%
    distinct(variable_tmpl, variable_display = variable)

  # order of components/variables as they appear in Figure2
  comp_order <- fig %>% mutate(row_id = row_number()) %>%
    group_by(component) %>% summarise(first_row = min(row_id), .groups="drop") %>%
    arrange(first_row) %>% pull(component)

  var_order_tbl <- fig %>%
    mutate(row_id = row_number()) %>%
    group_by(component, variable_tmpl) %>%
    summarise(first_row = min(row_id), .groups="drop") %>%
    arrange(first_row) %>%
    group_by(component) %>%
    mutate(var_rank = row_number()) %>%
    ungroup() %>%
    select(component, variable_tmpl, var_rank)

  tpl <- fig %>% select(component, variable, variable_tmpl, level_tmpl, order)
  lev_order <- tpl %>% select(variable_tmpl, level_tmpl, order)
  var2comp  <- tpl %>% distinct(variable_tmpl, component)
  level2var <- tpl %>% distinct(level_tmpl, variable_tmpl)   # inverse: level -> variable

  # --- standardize incoming columns (broad synonyms) ---
  std <- tibble(
    cov_in = .pick_col(
      data_in,
      "cov","variable","Variable","group","covariate","driver","component","predictor","cov_name"
    ),
    level  = .pick_col(
      data_in,
      "level","cat1","cat","Level","label","class","bin","category"
    ),
    est    = .pick_col(
      data_in,
      "est","mean","Estimate","mid","estimate","avg"
    ),
    lwr    = .pick_col(
      data_in,
      "lwr","ci_low","lower","ci.low","lo","lcl","low"
    ),
    upr    = .pick_col(
      data_in,
      "upr","ci_high","upper","ci.high","hi","ucl","high"
    )
  )

  if (all(is.na(std$cov_in)))  stop("Could not find a 'cov/variable' column in input.")
  if (all(is.na(std$level)))   stop("Could not find a 'level/cat1' column in input.")
  if (all(is.na(std$est)))     stop("Could not find 'est/mean' column in input.")
  if (all(is.na(std$lwr)))     stop("Could not find 'lwr/ci_low/lower' column in input.")
  if (all(is.na(std$upr)))     stop("Could not find 'upr/ci_high/upper' column in input.")

  # --- default mapping (extend/override via cov_map if needed) ---
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
    "soil_type"     = "Soil type",   # add explicit Soil type
    "crop"          = "Crops",
    "RAP"           = "RAP"
  )
  cov_map <- if (is.null(cov_map)) default_cov_map else c(default_cov_map, cov_map)

  # --- build standardized table from input ---
  dat_std <- std %>%
    mutate(
      variable        = recode(cov_in, !!!cov_map, .default = cov_in),
      variable_raw    = variable,                  # keep original display (pre-fix)
      level_raw       = level,
      variable_tmpl   = .norm_txt(variable),
      level_tmpl      = .norm_txt(level)
    )

  # --- FIX 1: detect rows where 'variable' is actually a template LEVEL (e.g., Acrisols, Mtn_sumt) ---
  is_level_name <- dat_std$variable_tmpl %in% level2var$level_tmpl
  if (any(is_level_name)) {
    # Find which template variable each level belongs to
    map_target <- level2var$variable_tmpl[match(dat_std$variable_tmpl[is_level_name],
                                                level2var$level_tmpl)]
    # Replace variable/level to proper positions
    dat_std$level_tmpl[is_level_name]    <- dat_std$variable_tmpl[is_level_name]  # level = the name (e.g., Acrisols)
    dat_std$variable_tmpl[is_level_name] <- map_target                            # variable = e.g., "Soil type"
    # Display columns: set level to the readable original variable text; set variable to template display
    dat_std$level[is_level_name]    <- dat_std$variable_raw[is_level_name]
    dat_std$variable[is_level_name] <- var_display$variable_display[
      match(dat_std$variable_tmpl[is_level_name], var_display$variable_tmpl)
    ]
  }

  # --- join component + order from template; robust fallbacks ---
  dat_join <- dat_std %>%
    left_join(var2comp,  by = "variable_tmpl") %>%                       # add component
    left_join(lev_order, by = c("variable_tmpl", "level_tmpl")) %>%      # add template order
    group_by(variable_tmpl) %>%
    mutate(
      # fill missing component within a variable group (if some matches found)
      component = coalesce(component, first(component[!is.na(component)])),
      # fallback order: numeric 'level' (if purely numeric) else append sequence
      order = dplyr::case_when(
        !is.na(order) ~ order,
        str_detect(level_raw, "^\\s*\\d+\\s*$") ~ as.integer(level_raw),
        TRUE ~ max(order, na.rm = TRUE) + row_number()
      )
    ) %>%
    ungroup() %>%
    left_join(var_order_tbl, by = c("component", "variable_tmpl"))

  # --- report any remaining unmatched labels ---
  unmatched <- dat_join %>%
    filter(is.na(var_rank) | is.na(component)) %>%
    distinct(variable, level) %>%
    arrange(variable, level)

  if (verbose && nrow(unmatched) > 0) {
    message("Heads-up: Some (variable, level) didn't fully match the Figure2 template.\n",
            "Check spelling/case or extend `cov_map`.\n")
    print(unmatched)
  }

  # --- final Figure2 schema + ordering identical to template where possible ---
  dat_fig2 <- dat_join %>%
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
    mutate(component = factor(component, levels = comp_order)) %>%
    arrange(is.na(component), component, is.na(var_rank), var_rank, desc(order)) %>%
    select(component, variable, level, est, lwr, upr, order)

  if (!is.null(out_path)) {
    write_xlsx(dat_fig2, out_path)
    if (verbose) message("Wrote: ", out_path)
  }

  list(data = dat_fig2, unmatched = unmatched)
}


################################################################################
# A

# paths
template_path <- "./output/Figure_data/Figure2.xlsx"
af_path       <- "./output/Figure_data/af_fig_3.xlsx"
cc_path       <- "./output/Figure_data/cc_fig_3.xlsx"
nt_path       <- "./output/Figure_data/nt_fig_3.xlsx"
of_path       <- "./output/Figure_data/of_fig_3.xlsx"
af_out_path   <- "./output/Figure_data/mod_af_fig3.xlsx"
cc_out_path   <- "./output/Figure_data/mod_cc_fig3.xlsx"
nt_out_path   <- "./output/Figure_data/mod_nt_fig3.xlsx"
of_out_path   <- "./output/Figure_data/mod_of_fig3.xlsx"

# Agroforestry
af_res <- to_figure2_schema(
  data = af_path,
  template_path = template_path,
  out_path = af_out_path,   # write Excel
  verbose = TRUE
)

df_af<-af_res$data %>% dplyr::mutate(RAP="af") %>%
                  dplyr::relocate(RAP, .before = 1) %>%
                  mutate(level= if_else(level == "Cereal","Other cereal",level))

af<-read_xlsx(af_path)            
dim(af)
dim(df_af)

# Cover Crop
cc_res <- to_figure2_schema(
  data = cc_path,
  template_path = template_path,
  out_path = cc_out_path,   # write Excel
  verbose = TRUE
)

df_cc<-cc_res$data %>% dplyr::mutate(RAP="cc") %>%
                  dplyr::relocate(RAP, .before = 1)

cc<-read_xlsx(cc_path)            
dim(cc)
dim(df_cc)

# No-tillage
nt_res <- to_figure2_schema(
  data = nt_path,
  template_path = template_path,
  out_path = nt_out_path,   # write Excel
  verbose = TRUE
)

df_nt<-nt_res$data %>% dplyr::mutate(RAP="nt") %>%
                  dplyr::relocate(RAP, .before = 1) %>%
                  mutate(level= if_else(level == "Cereal","Other cereal",level))

nt<-read_xlsx(nt_path)            
dim(nt)
dim(df_nt)

# Organic farming
of_res <- to_figure2_schema(
  data = of_path,
  template_path = template_path,
  out_path = of_out_path,   # write Excel
  verbose = TRUE
)

df_of<-of_res$data %>% dplyr::mutate(RAP="of") %>%
                  dplyr::relocate(RAP, .before = 1) %>%
                  mutate(level= if_else(level == "Cereal","Other cereal",level))

of<-read_xlsx(of_path)            
dim(of)
dim(df_of)

# as.data.frame(df_af) %>% dplyr::select(level)
# as.data.frame(df_cc) %>% dplyr::select(level)
# as.data.frame(df_nt) %>% dplyr::select(level)
# as.data.frame(df_of) %>% dplyr::select(level)

# Bind_all now
df_fig3<-bind_rows(df_af,df_cc,df_nt,df_of)


# df_bind <- df_fig3 %>% mutate(level = case_when(
# 
#     (!level %in% c("GDD_maize1","GDD_maize2","GDD_maize3","GDD_maize4","GDD_maize5",
#                  "GDD_wheat1","GDD_wheat2","GDD_wheat3","GDD_wheat4","GDD_wheat5",
#                  "GDD_rice1","GDD_rice2","GDD_rice3","GDD_rice4","GDD_rice5",
#                  "GDD_soybean1","GDD_soybean2","GDD_soybean3","GDD_soybean4","GDD_soybean5"))~level,
#     
#     # GDD
# # gdd_mat <- c(0,   800, 1,
# #             800, 2700,  2,
# #             2700, 4000,  3,
# #             4000, 6000, 4,
# #             6000, 10000, 5)
#   
#       (level %in% c("GDD_maize1"))~"<0.8",
#       (level %in% c("GDD_maize2"))~"0.8-2.7",
#       (level %in% c("GDD_maize3"))~"2.7-4",
#       (level %in% c("GDD_maize4"))~"4-6",
#       (level %in% c("GDD_maize5"))~"6-10",
#       (level %in% c("GDD_wheat1"))~"<0.8",
#       (level %in% c("GDD_wheat2"))~"0.8-2.7",
#       (level %in% c("GDD_wheat3"))~"2.7-4",
#       (level %in% c("GDD_wheat4"))~"4-6",
#       (level %in% c("GDD_wheat5"))~"6-10",
#       (level %in% c("GDD_rice1"))~"<0.8",
#       (level %in% c("GDD_rice2"))~"0.8-2.7",
#       (level %in% c("GDD_rice3"))~"2.7-4",
#       (level %in% c("GDD_rice4"))~"4-6",
#       (level %in% c("GDD_rice5"))~"6-10",
#       (level %in% c("GDD_soybean1"))~"<0.8",
#       (level %in% c("GDD_soybean2"))~"0.8-2.7",
#       (level %in% c("GDD_soybean3"))~"2.7-4",
#       (level %in% c("GDD_soybean4"))~"4-6",
#       (level %in% c("GDD_soybean5"))~"6-10"
#      ))

as.data.frame(df_fig3)

write_xlsx(df_fig3, "./output/Figure_data/Figure3.xlsx")
