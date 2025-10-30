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

template_path <- "./output/Figure_data/Figure2.xlsx"
nt_path       <- "./output/Figure_data/nt_fig_3.xlsx"   # make sure the path/underscore is correct
nt_out_path   <- "./output/Figure_data/mod_nt_fig3.xlsx"

nt_res <- to_figure2_schema(
  data = nt_path,
  template_path = template_path,
  out_path = nt_out_path,
  verbose = TRUE
)

nt_fig3 <- nt_res$data       # transformed, Figure2-compatible
nt_res$unmatched            # anything still not matched

as.data.frame(nt_fig3)


as.data.frame(read_xlsx(template_path))
