# =========================
# Figure 2: Forest panels
# =========================

# ---- Packages ----
library(ggplot2)
library(dplyr)
library(patchwork)
library(readxl)
library(cowplot)
# install.packages("ragg") # once
# library(ragg)

# ---- Data ----
# Update path if needed
df <- read_xlsx("C:/Users/hounkpk1/RAP_Drivers/output/Figure_data/Figure2.xlsx")

# ---- Global controls ----
xlims <- c(-20, 30)

# One knob to scale text/geom sizes for journal readability
scale_factor <- 1.4  # try between 1.2 and 1.6; increase if printing smaller

# ---- Helper: forest plot for a single component ----
plot_component <- function(dd, title = unique(dd$component), x_limits = c(-20, 30)) {
  dd <- dd %>%
    arrange(variable, desc(order)) %>%
    mutate(
      level    = factor(level, levels = unique(level)),
      variable = factor(variable, levels = unique(dd$variable))
    )

  ggplot(dd, aes(x = est, y = level)) +
    geom_segment(aes(x = lwr, xend = upr, y = level, yend = level), linewidth = 0.9) +
    geom_point(size = 2.6) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.7, color = "red") +
    facet_grid(rows = vars(variable), scales = "free_y", space = "free_y", switch = "y") +
    scale_x_continuous(limits = x_limits, breaks = seq(x_limits[1], x_limits[2], by = 10)) +
    labs(x = "% change (crop yield)", y = NULL, title = title) +
    theme_minimal(base_size = 11 * scale_factor) +
    theme(
      plot.title        = element_text(face = "bold", size = 16 * scale_factor, hjust = 0.5, margin = margin(b = 6)),
      panel.grid.major.y= element_blank(),
      panel.grid.minor  = element_blank(),
      strip.placement   = "outside",
      axis.text.y       = element_text(size = 11.5 * scale_factor),
      axis.text.x       = element_text(size = 11.5 * scale_factor),
      axis.title.x      = element_text(size = 13.5 * scale_factor, face = "bold", margin = margin(t = 6)),
      strip.text.y.left = element_text(face = "bold", size = 12.5 * scale_factor, margin = margin(r = 8)),
      strip.background  = element_rect(fill = NA, color = NA),
      panel.spacing.y   = unit(4, "mm"),
      plot.margin       = margin(8, 8, 8, 8)
    )
}

# ---- Build panels (NOTE: name arguments so dd gets the data frame) ----
p_mgmt  <- plot_component(dd = df %>% filter(component == "Management"),
                          title = "Management",        x_limits = xlims)

p_crops <- plot_component(dd = df %>% filter(component == "Crops"),
                          title = "Crops",             x_limits = xlims)

p_clim  <- plot_component(dd = df %>% filter(component == "Climate"),
                          title = "Climate Variables", x_limits = xlims)

p_soil  <- plot_component(dd = df %>% filter(component == "Soil properties"),
                          title = "Soil Properties",   x_limits = xlims)

p_topo  <- plot_component(dd = df %>% filter(component == "Topography"),
                          title = "Topography",        x_limits = xlims)

# ---- Hide x axis text/ticks for top row ----
blank_x <- theme(
  axis.text.x  = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank()
)
p_mgmt_top  <- p_mgmt  + blank_x
p_crops_top <- p_crops + blank_x

# ---- Align per column so x ticks & zero line match ----
col1 <- cowplot::align_plots(p_mgmt_top,  p_clim, align = "v", axis = "lr")
col2 <- cowplot::align_plots(p_crops_top, p_soil, align = "v", axis = "lr")

# ---- Assemble combined figure ----
final_plot <-
  (wrap_elements(col1[[1]]) + wrap_elements(col2[[1]]) + plot_spacer()) /
  (wrap_elements(col1[[2]]) + wrap_elements(col2[[2]]) + wrap_elements(p_topo)) +
  plot_layout(heights = c(0.6, 2.0), widths = c(1, 1, 1)) &
  theme(plot.margin = margin(8, 8, 8, 8))

# Preview in R
final_plot

# ---- Export (journal-ready) ----
dir.create("./output/new_graphs", showWarnings = FALSE, recursive = TRUE)

## Vector (preferred): crisp and rescalable
ggsave("./output/new_graphs/figure_2_singlecol.pdf",
       final_plot, width = 15, height = 20.5, units = "in",
       device = cairo_pdf)

# ## Vector (preferred): crisp and rescalable
# ggsave("./output/new_graphs/figure_2_singlecol.png",
#        final_plot, width = 15, height = 20.5, units = "in",
#        device = cairo_pdf)

ggsave("./output/new_graphs/figure_2_doublecol.pdf",
       final_plot, width = 12.5, height = 17.5, units = "in",
       device = cairo_pdf)

## Raster (only if required by journal)
# If you have ragg installed, uncomment these for very sharp raster output.
ggsave("./output/new_graphs/figure_2_600dpi.png",
       final_plot, width = 12.0, height = 15.5, units = "in",
       dpi = 600, device = ragg::agg_png)

ggsave("./output/new_graphs/figure_2_600dpi.tiff",
       final_plot, width = 7.0, height = 9.5, units = "in",
       dpi = 600, compression = "lzw", device = ragg::agg_tiff)
