# ---- Packages ----
library(ggplot2)
library(dplyr)
library(readxl)
library(patchwork)
library(cowplot)
library(tibble)

# ---- Load data ----
df <- read_xlsx("C:/Users/hounkpk1/RAP_Drivers/output/Figure_data/Figure2.xlsx") |>
  as_tibble()

# ---- Global scaling ----
scale_factor <- 1.4  # adjust until labels look right at final size

# ---- Helper function ----
plot_component <- function(dd, title = unique(dd$component), x_limits = c(-20, 30)) {
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
      axis.text.y       = element_text(size = 11.5 * scale_factor, margin = margin(r = 2)),
      axis.text.x       = element_text(size = 11.5 * scale_factor),
      axis.title.x      = element_text(size = 13.5 * scale_factor, face = "bold", margin = margin(t = 6)),
      strip.text.y.left = element_text(face = "bold", size = 12.5 * scale_factor, margin = margin(r = 10)),
      strip.background  = element_rect(fill = NA, color = NA),
      panel.spacing.y   = unit(3.5, "mm"),
      plot.margin       = margin(8, 10, 8, 20)
    )
}

# ---- Build panels ----
xlims <- c(-20, 30)

p_mgmt  <- df |> dplyr::filter(component == "Management")      |> plot_component(title = "Management",        x_limits = xlims)
p_crops <- df |> dplyr::filter(component == "Crops")           |> plot_component(title = "Crops",             x_limits = xlims)
p_clim  <- df |> dplyr::filter(component == "Climate")         |> plot_component(title = "Climate Variables", x_limits = xlims)
p_soil  <- df |> dplyr::filter(component == "Soil properties") |> plot_component(title = "Soil Properties",   x_limits = xlims)
p_topo  <- df |> dplyr::filter(component == "Topography")      |> plot_component(title = "Topography",        x_limits = xlims)

# ---- Hide x-axis text/ticks in top row ----
blank_x <- theme(axis.text.x = element_blank(),
                 axis.title.x = element_blank(),
                 axis.ticks.x = element_blank())
p_mgmt_top  <- p_mgmt  + blank_x
p_crops_top <- p_crops + blank_x

# Add more left margin for long soil labels
extra_left <- theme(plot.margin = margin(8, 10, 8, 35))
p_clim <- p_clim + extra_left
p_soil <- p_soil + extra_left

# ---- Align columns ----
col1 <- cowplot::align_plots(p_mgmt_top,  p_clim, align = "v", axis = "lr")
col2 <- cowplot::align_plots(p_crops_top, p_soil, align = "v", axis = "lr")

# ---- Assemble final plot ----
final_plot <-
  (wrap_elements(col1[[1]]) + wrap_elements(col2[[1]]) + plot_spacer()) /
  (wrap_elements(col1[[2]]) + wrap_elements(col2[[2]]) + wrap_elements(p_topo)) +
  plot_layout(
    heights = c(0.6, 2.0),
    widths  = c(1.35, 1.2, 1.0)   # give more room to first two columns
  ) &
  theme(plot.margin = margin(10, 12, 10, 20))

final_plot

# ---- Export journal-ready PNG & TIFF ----
# Choose physical size: here ~180 mm width (double column)
w_in <- 7.1
h_in <- 9.5
dpi  <- 600

# PNG
ggsave("./output/new_graphs/figure_2_600dpi.png",
       final_plot, device = ragg::agg_png,
       width = w_in, height = h_in, units = "in", dpi = dpi, limitsize = FALSE)

# TIFF
ggsave("./output/new_graphs/figure_2_600dpi.tiff",
       final_plot, device = ragg::agg_tiff,
       width = w_in, height = h_in, units = "in", dpi = dpi,
       compression = "lzw", limitsize = FALSE)



# --- Vector (best for line art & text) ---
# Embeds fonts; editors can scale without losing quality.
# ggsave("./output/new_graphs/figure_2_singlecol.pdf",
#        final_plot,
#        width = 3.5, height = 8.5, units = "in",
#        device = cairo_pdf, dpi = 600, # dpi doesn't affect vector paths but some journals like it set
#        fallback_resolution = 600)
# 
# ggsave("./output/new_graphs/figure_2_doublecol.pdf",
#        final_plot,
#        width = 7.0, height = 9.5, units = "in",
#        device = cairo_pdf)

