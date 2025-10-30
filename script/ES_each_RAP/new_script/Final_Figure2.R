# =========================
# Journal-ready forest plot
# =========================

# ---- Packages ----
library(ggplot2)
library(dplyr)
library(readxl)
library(patchwork)
library(cowplot)
library(tibble)

# ---- Data ----
df <- read_xlsx("C:/Users/hounkpk1/RAP_Drivers/output/Figure_data/Figure2.xlsx") |>
  as_tibble()

# ---- Global size control ----
scale_factor <- 1.4   # bump up/down to taste (affects base text etc.)

# ---- Helper: single component forest plot ----
# y_text_size lets us shrink y-axis labels for dense facets only.
plot_component <- function(dd, title = unique(dd$component), x_limits = c(-20, 30),
                           y_text_size = 11.5) {

  dd <- dd |>
    arrange(variable, desc(order)) |>
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
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 11 * scale_factor) +
    theme(
      plot.title        = element_text(face = "bold", size = 16 * scale_factor, hjust = 0.5, margin = margin(b = 6)),
      panel.grid.major.y= element_blank(),
      panel.grid.minor  = element_blank(),
      strip.placement   = "outside",
      axis.text.y       = element_text(size = y_text_size, margin = margin(r = 2)),
      axis.text.x       = element_text(size = 11.5 * scale_factor),
      axis.title.x      = element_text(size = 13.5 * scale_factor, face = "bold", margin = margin(t = 6)),
      strip.text.y.left = element_text(face = "bold", size = 12.5 * scale_factor, margin = margin(r = 10)),
      strip.background  = element_rect(fill = NA, color = NA),
      panel.spacing.y   = unit(3.5, "mm"),
      plot.margin       = margin(8, 10, 8, 22)
    )
}

# ---- Build panels (facet-specific y-label sizes) ----
xlims <- c(-20, 30)

p_mgmt  <- df |> dplyr::filter(component == "Management")      |> plot_component(title = "Management",        x_limits = xlims, y_text_size = 11.5)
p_crops <- df |> dplyr::filter(component == "Crops")           |> plot_component(title = "Crops",             x_limits = xlims, y_text_size = 11.5)
p_clim  <- df |> dplyr::filter(component == "Climate")         |> plot_component(title = "Climate Variables", x_limits = xlims, y_text_size = 10.0)
p_soil  <- df |> dplyr::filter(component == "Soil properties") |> plot_component(title = "Soil Properties",   x_limits = xlims, y_text_size = 9.0)
p_topo  <- df |> dplyr::filter(component == "Topography")      |> plot_component(title = "Topography",        x_limits = xlims, y_text_size = 9.0)

# ---- Hide x-axis text/ticks for top row ----
blank_x <- theme(axis.text.x = element_blank(),
                 axis.title.x = element_blank(),
                 axis.ticks.x = element_blank())
p_mgmt_top  <- p_mgmt  + blank_x
p_crops_top <- p_crops + blank_x

# Extra left margin for dense first/second columns (long y labels)
more_left <- theme(plot.margin = margin(8, 10, 8, 32))
p_clim <- p_clim + more_left
p_soil <- p_soil + more_left

# ---- Align columns so x=0 line & ticks line up ----
col1 <- cowplot::align_plots(p_mgmt_top,  p_clim, align = "v", axis = "lr")
col2 <- cowplot::align_plots(p_crops_top, p_soil, align = "v", axis = "lr")

# ---- Assemble with more space for 2nd row (dense panels) ----
final_plot <-
  (wrap_elements(col1[[1]]) + wrap_elements(col2[[1]]) + plot_spacer()) /
  (wrap_elements(col1[[2]]) + wrap_elements(col2[[2]]) + wrap_elements(p_topo)) +
  plot_layout(
    heights = c(0.45, 2.85),  # give the second row lots of vertical space
    widths  = c(1.35, 1.20, 1.00)
  ) &
  theme(plot.margin = margin(10, 14, 10, 24))

# ---- Export (PNG & TIFF, raster) ----
# Make the figure physically tall enough so big labels don’t collide.
w_in <- 15   # ~180 mm (common double-column width)
h_in <- 17.0  # taller page for many y-levels
dpi  <- 600

# PNG (ragg for crisp text)
# ggsave("./output/new_graphs/figure_2_600dpi.png",
#        final_plot, device = ragg::agg_png,
#        width = w_in, height = h_in, units = "in", dpi = dpi, limitsize = FALSE)
# 
# # TIFF (journal favorite)
# ggsave("./output/new_graphs/figure_2_600dpi.tiff",
#        final_plot, device = ragg::agg_tiff,
#        width = w_in, height = h_in, units = "in", dpi = dpi,
#        compression = "lzw", limitsize = FALSE)

ggsave("./output/new_graphs/figure_2_doublecol.pdf",
       final_plot,
      width = w_in, height = h_in,units = "in",
       device = cairo_pdf)
