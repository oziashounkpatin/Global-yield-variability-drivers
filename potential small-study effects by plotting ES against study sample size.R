# ======================================================================
# Small-study effects: combined + faceted panels (FINAL VERSION)
# ======================================================================

# --- Packages ----------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(ggplot2)
  library(patchwork)
  library(grid)   # for unit()
})

set.seed(7)

# --- I/O ---------------------------------------------------------------
in_path <- "./output/Figure_4/full_data_figure_21.xlsx"
out_dir <- "./output/Figure_10/"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Read --------------------------------------------------------------
df_raw0 <- read_xlsx(in_path, guess_max = 1000)

# --- Prepare data ------------------------------------------------------
df_plot <- df_raw0 %>%
  mutate(
    effectSize = as.numeric(effectSize),
    study_size = ave(effectSize, study_id, FUN = length)
  ) %>%
  filter(!is.na(effectSize), study_size > 0)

# --- Colors ------------------------------------------------------------
cols <- c(
  AF = "#E6C98B",
  CC = "#B87963",
  NT = "#6C6FA3",
  OF = "#9FD0E4"
)

# ======================================================================
# 1. TOP PANEL (ALL DATA)
# ======================================================================
p_all <- ggplot(df_plot, aes(study_size, effectSize, color = key)) +
  
  geom_point(alpha = 0.35, size = 1) +
  
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  
  geom_hline(yintercept = mean(df_plot$effectSize, na.rm = TRUE),
             linetype = "dashed", color = "black") +
  
  scale_x_log10() +
  scale_color_manual(values = cols) +
  
  labs(
    y = "Effect size (%)",
    x = NULL,
    color = "All farming approaches"
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(size = 5, alpha = 1)
    )
  ) +
  
  theme_bw(base_size = 18) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.2, "cm"),
    
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 14),
    
    axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    
    panel.grid = element_blank()
  )

# ======================================================================
# 2. FACET PANEL (BY PRACTICE)
# ======================================================================
p_facet <- ggplot(df_plot, aes(study_size, effectSize, color = key)) +
  
  geom_point(alpha = 0.35, size = 1) +
  
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  
  geom_hline(
    data = df_plot %>%
      group_by(key) %>%
      summarise(m = mean(effectSize, na.rm = TRUE), .groups = "drop"),
    aes(yintercept = m),
    linetype = "dashed",
    color = "black"
  ) +
  
  facet_wrap(~ key, ncol = 2, scales = "free_x") +
  
  scale_x_log10() +
  scale_color_manual(values = cols) +
  
  labs(
    x = "Study size (log scale)",
    y = "Effect size (%)"
  ) +
  
  theme_bw(base_size = 18) +
  theme(
    legend.position = "none",
    
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    
    panel.grid = element_blank()
  )

# ======================================================================
# 3. COMBINE
# ======================================================================
final_plot <- p_all / p_facet + plot_layout(heights = c(1, 2))

# ======================================================================
# 4. SAVE
# ======================================================================
ggsave(
  filename = paste0(out_dir, "Figure_10_final_clean.png"),
  plot = final_plot,
  width = 10,
  height = 12,
  dpi = 300
)

ggsave(
  filename = paste0(out_dir, "Figure_10_final_clean.pdf"),
  plot = final_plot,
  width = 10,
  height = 12,
  dpi = 300
)
# --- Display -----------------------------------------------------------
final_plot