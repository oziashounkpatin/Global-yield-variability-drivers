# --- READ + NORMALIZE ---------------------------------------------------------
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(tibble)

infile  <- "C:/Users/hounkpk1/RAP_Drivers/output/Figure_data/Figure3.xlsx"
outdir  <- "./output/new_graphs"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

df <- read_xlsx(infile) %>%
  select(RAP, component:order) %>%
  # map lowercase RAP codes to pretty labels for faceting
  mutate(
    RAP_lab = recode(str_to_lower(RAP),
                     af = "Agroforestry",
                     cc = "Cover crop",
                     nt = "No-tillage",
                     of = "Organic farming",
                     .default = NA_character_),
    RAP_lab = factor(RAP_lab,
                     levels = c("Agroforestry","Cover crop","No-tillage","Organic farming"))
  ) %>%
  # standardize variable names to consistent, short labels
  mutate(
    variable = case_when(
      str_detect(variable, regex("^climatic\\s*regions$", TRUE)) ~ "CR",
      str_detect(variable, regex("^aridity$", TRUE))             ~ "Aridity",
      str_detect(variable, regex("gdd\\s*maize", TRUE))          ~ "GDD Maize",
      str_detect(variable, regex("gdd\\s*rice", TRUE))           ~ "GDD Rice",
      str_detect(variable, regex("gdd\\s*soybean", TRUE))        ~ "GDD Soybean",
      str_detect(variable, regex("gdd\\s*wheat", TRUE))          ~ "GDD Wheat",
      str_detect(variable, regex("^phosphorus$|^p$", TRUE))      ~ "P",
      str_detect(variable, regex("^ph$", TRUE))                  ~ "pH",
      str_detect(variable, regex("^soc$", TRUE))                 ~ "SOC",
      str_detect(variable, regex("^bd$", TRUE))                  ~ "BD",
      str_detect(variable, regex("^texture$", TRUE))             ~ "Texture",
      str_detect(variable, regex("^soil\\s*type", TRUE))         ~ "Soil type",
      str_detect(variable, regex("^elev|^dem", TRUE))            ~ "Elevation",
      str_detect(variable, regex("^slope$", TRUE))               ~ "Slope",
      str_detect(variable, regex("^landform$", TRUE))            ~ "Landform",
      str_detect(variable, regex("^crops", TRUE))                ~ "Crops",
      TRUE ~ variable
    )
  ) %>%
  # move "overall" from Climate -> Crops (keeps your numbers)
  mutate(
    component = if_else(component == "Climate" &
                          str_detect(level, regex("^climatic regions overall$|^overall$", TRUE)),
                        "Crops", component),
    variable  = if_else(component == "Crops" & str_detect(level, regex("overall", TRUE)),
                        "Crops", variable),
    level     = if_else(component == "Crops" & str_detect(level, regex("overall", TRUE)),
                        "overall", level),
    order     = if_else(component == "Crops" & level == "overall", 0L, order)
  )

# --- DESIRED VARIABLE ORDER PER COMPONENT -------------------------------------
var_order <- list(
  "Crops"           = c("Crops"),
  "Climate"         = c("CR","Aridity","GDD Maize","GDD Rice","GDD Soybean","GDD Wheat"),
  "Soil properties" = c("P","pH","SOC","BD","Texture","Soil type"),
  "Topography"      = c("Elevation","Slope","Landform")
)

# --- PLOTTER (handles Crops + Climate together) --------------------------------
scale_factor <- 1.35
plot_crops_climate <- function(dd, title = "",
                               x_limits = c(-20, 50), x_by = 20,
                               y_text_size = 11, y_pad = 10,
                               strip_pad = 16, left_outer = 42) {

  # desired variable order across BOTH components
  ord_both <- c("Crops", "CR", "Aridity", "GDD Maize", "GDD Rice", "GDD Soybean", "GDD Wheat")

  dd <- dd %>%
    filter(component %in% c("Crops","Climate")) %>%
    mutate(
      component = factor(component, levels = c("Crops","Climate")),  # ✅ Crops first
      variable  = factor(variable, levels = ord_both)
    ) %>%
    arrange(component, variable, desc(order)) %>%
    group_by(component, variable) %>%
    mutate(level = factor(level, levels = unique(level))) %>%
    ungroup()

  x_breaks <- seq(
    ceiling(x_limits[1] / x_by) * x_by,
    floor(x_limits[2]  / x_by) * x_by,
    by = x_by
  )

  ggplot(dd, aes(x = est, y = level)) +
    geom_segment(aes(x = lwr, xend = upr, y = level, yend = level), linewidth = 0.9) +
    geom_point(size = 2.6) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.7, color = "red") +
    facet_grid(
      rows = vars(component, variable),  # ✅ Crops section on top
      cols  = vars(RAP_lab),
      scales = "free_y", space = "free_y", switch = "y"
    ) +
    scale_x_continuous(limits = x_limits, breaks = x_breaks, minor_breaks = NULL) +
    labs(x = "% change (crop yield)", y = NULL, title = title) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 11 * scale_factor) +
    theme(
      plot.title         = element_text(face = "bold", size = 18 * scale_factor, hjust = 0.5, margin = margin(b = 10)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.3),
      strip.placement    = "outside",
      strip.text.y.left  = element_text(face = "bold", size = 12.5 * scale_factor, margin = margin(r = strip_pad)),
      strip.background   = element_rect(fill = NA, color = NA),

      axis.text.y        = element_text(size = y_text_size, margin = margin(r = y_pad)),
      axis.text.x        = element_text(size = 30),
      axis.title.x       = element_text(size = 13.5 * scale_factor, face = "bold", margin = margin(t = 6)),
      panel.spacing.y    = unit(4, "mm"),
      panel.spacing.x    = unit(5, "mm"),
      plot.margin        = margin(12, 18, 12, left_outer)
    )
}

# --- BUILD THE COMBINED FIGURE -------------------------------------------------
p_crops_clim <- df %>%
  filter(component %in% c("Crops","Climate")) %>%
  plot_crops_climate(title = "", x_limits = c(-20, 50), x_by = 20, y_text_size = 11)

# --- SAVE (size tweakable) -----------------------------------------------------
ggsave(file.path(outdir, "Fig3_Crops_Climate.pdf"),
       p_crops_clim, width = 16, height = 12, units = "in", 
       device = cairo_pdf)

# --- BUILD 4 FIGURES WITH THE NEW ORDER ---------------------------------------
# p_crops <- df %>% filter(component == "Crops") %>%
#   plot_component("", x_limits = c(-20, 50), x_by = 20, y_text_size = 11.5)
# 
# p_clim  <- df %>% filter(component == "Climate") %>%
#   plot_component("", x_limits = c(-20, 50), x_by = 20, y_text_size = 10.5)

p_soil  <- df %>% filter(component == "Soil properties") %>%
  plot_component("", x_limits = c(-20, 20), x_by = 10, y_text_size = 9.5)

p_topo  <- df %>% filter(component == "Topography") %>%
  plot_component("", x_limits = c(-20, 20), x_by = 10, y_text_size = 9.5)


# --- SAVE (sizes can be tweaked if labels are long) ---------------------------
#ggsave(file.path(outdir, "Fig31_Crops.pdf"),       p_crops, width = 12, height = 5, units = "in", device = cairo_pdf)
#ggsave(file.path(outdir, "Fig32_Climate.pdf"),     p_clim,  width = 16, height = 11.5, units = "in", device = cairo_pdf)
#ggsave(file.path(outdir, "Fig33_SoilProps.pdf"),   p_soil,  width = 16, height = 11.5, units = "in", device = cairo_pdf)
#ggsave(file.path(outdir, "Fig34_Topography.pdf"),  p_topo,  width = 16, height = 11.5, units = "in", device = cairo_pdf)
