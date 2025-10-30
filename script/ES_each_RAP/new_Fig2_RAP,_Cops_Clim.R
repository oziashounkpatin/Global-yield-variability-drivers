# ---- Packages ----
library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)
library(readxl)
library(cowplot)

# ---- Data ----
df <- read_xlsx("C:/Users/hounkpk1/RAP_Drivers/output/Figure_data/Figure2.xlsx")

# ---- Helper: forest plot for a single component ----
plot_component <- function(dd, title = unique(dd$component), x_limits = c(-20, 30)) {
  dd <- dd %>%
    arrange(variable, desc(order)) %>%
    mutate(
      level = factor(level, levels = unique(level)),
      variable = factor(variable, levels = unique(dd$variable))
    )

  ggplot(dd, aes(x = est, y = level)) +
    geom_segment(aes(x = lwr, xend = upr, y = level, yend = level), linewidth = 0.6) +
    geom_point(size = 2) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "red") +
    facet_grid(rows = vars(variable), scales = "free_y", space = "free_y", switch = "y") +
    scale_x_continuous(limits = x_limits, breaks = seq(x_limits[1], x_limits[2], by = 10)) +
    labs(x = "% change (crop yield)", y = NULL, title = title) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 6)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.placement = "outside",
      axis.text = element_text(size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      strip.text.y.left = element_text(face = "bold", margin = margin(r = 6), size = 14),
      strip.background = element_rect(fill = NA, color = NA),
      panel.spacing.y = unit(5, "mm"),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# ---- Panels ----
xlims <- c(-20, 30)
p_mgmt  <- df %>% filter(component == "Management")      %>% plot_component("Management",        x_limits = xlims)
p_crops <- df %>% filter(component == "Crops")           %>% plot_component("Crops",             x_limits = xlims)
p_clim  <- df %>% filter(component == "Climate")         %>% plot_component("Climate Variables", x_limits = xlims)
p_soil  <- df %>% filter(component == "Soil properties") %>% plot_component("Soil Properties",   x_limits = xlims)
p_topo  <- df %>% filter(component == "Topography")      %>% plot_component("Topography",        x_limits = xlims)

# ---- Hide x axis text/ticks for top row ----
p_mgmt_top  <- p_mgmt  + theme(axis.text.x = element_blank(),
                               axis.title.x = element_blank(),
                               axis.ticks.x = element_blank())
p_crops_top <- p_crops + theme(axis.text.x = element_blank(),
                               axis.title.x = element_blank(),
                               axis.ticks.x = element_blank())

# ---- Align per column so panel widths match (red line & x ticks align) ----
col1 <- cowplot::align_plots(p_mgmt_top,  p_clim, align = "v", axis = "lr")
col2 <- cowplot::align_plots(p_crops_top, p_soil, align = "v", axis = "lr")
# col3 is only Topography

# ---- Assemble with patchwork ----
final_plot <-
  (wrap_elements(col1[[1]]) + wrap_elements(col2[[1]]) + plot_spacer()) /
  (wrap_elements(col1[[2]]) + wrap_elements(col2[[2]]) + wrap_elements(p_topo)) +
  plot_layout(heights = c(0.6, 2.0), widths = c(1, 1, 1)) &
  theme(plot.margin = margin(6, 6, 6, 6))

final_plot
# Optionally save
# ggsave("Figure2_forest.png", final_plot2, width = 9, height = 10, dpi = 300)

# Print or save
ggsave("./output/new_graphs/figure_2.pdf", final_plot, width = 16, height = 19, dpi = 300)
ggsave("./output/new_graphs/figure_2.png", final_plot, width = 16, height = 19, dpi = 300)
ggsave("./output/new_graphs/figure_2_copy.pdf", final_plot, width = 16, height = 19, dpi = 300)
#-------------------------------------------------------
# Vertical stacking
# Use the same data frame df and plot_component() from before
# 
# xlims <- c(-20, 30)  # adjust as needed
# 
# p_climate <- df %>%
#   filter(component == "Climate") %>%
#   plot_component("Climate Variables", x_limits = xlims)
# 
# p_soil <- df %>%
#   filter(component == "Soil properties") %>%
#   plot_component("Soil Properties", x_limits = xlims)
# 
# p_topo <- df %>%
#   filter(component == "Topography") %>%
#   plot_component("Topography", x_limits = xlims)
# 
# # Stack them vertically using patchwork
# library(patchwork)
# final_plot1 <- p_climate / p_soil / p_topo +
#   plot_layout(ncol = 1, heights = c(1, 1.2, 1.5)) &   # adjust heights as needed
#   theme(plot.margin = margin(6, 6, 6, 6))
# 
# # Show or save
# final_plot1
# ggsave("./output/new_graphs/figure_23.png", final_plot1, width = 7, height = 14, dpi = 300)
