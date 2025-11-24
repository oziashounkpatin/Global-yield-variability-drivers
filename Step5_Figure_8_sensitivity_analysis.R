# ---- chunk_1 ----

#knitr::opts_chunk$set(echo = TRUE)


# ---- chunk_2 ----

# packages
packages <- c("here", "tidyverse", "readxl", "RColorBrewer", "scico")

# install if not installed
not_installed <- packages[!(packages %in% installed.packages()[,"Package"])]

if (length(not_installed) > 0) {
  install.packages(not_installed)
}

# Load packages
lapply(packages, library, character.only = TRUE)


# After once installed, this is faster:
library(here)
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(scico)  

# Exact colours used:
scico(4, palette = "managua")
exact_colours <- c("#FFCE66", "#92463A", "#4D5492", "#80E6FF")


# Convert ES to perc
# perc<- function(data){
#   data_conv=100*(exp(data) - 1)
#   return(data_conv)
# }

# Read the data
df_raw <- read_excel("./input/scps_data.xlsx")



# Explore data: number of studies and crops
length(unique(df_raw$key)) # 4 studies?
length(unique(df_raw$Crop_Group)) # 8 crop

names(df_raw)


# ---- chunk_3 ----

# Group data by studies and count the number of cases
# df_raw<-df_raw %>% 
#   #dplyr::rename(ES=effectSize) %>%
#   #filter(!ES > 100) %>%
#   dplyr::select(!effectSize)  %>%
#   dplyr::rename(effectSize=ES) 

  
 df_raw %>% group_by(key) %>% 
  summarise(Number_of_cases = n()) %>% 
  arrange(Number_of_cases) # OF 587, AF 853, CC 1029, NT 7763


# ---- chunk_4 ----

tail(df_raw)


# ---- chunk_5 ----

# Calculate quantiles outside ggplot for cleaner code
lower_quantile <- df_raw |> pull(effectSize) |> quantile(0.025)
upper_quantile <- df_raw |> pull(effectSize) |> quantile(0.975)


# ---- chunk_6 ----




# ---- chunk_7 ----

df_raw %>%
  filter(key == "AF") |> 
  group_by(key) %>%
  summarise(avg_effectSize = mean(effectSize, na.rm = TRUE)) # 0.3851843	

# df_raw %>% filter(key == "AF") |> pull(effectSize) |> mean()

# get 95% CI
df_raw %>%
  filter(key == "AF") |> 
  group_by(key) %>%
  summarise(avg_effectSize = mean(effectSize, na.rm = TRUE),
            lower = quantile(effectSize, 0.025, na.rm = TRUE),
            upper = quantile(effectSize, 0.975, na.rm = TRUE)) # 0.3851843, -0.6427598,	2.142308	for AF




# plot distribution for all keys
plt_4distributions <- df_raw |> 
  ggplot(aes(x = effectSize)) + 
  geom_density(aes(fill = key), alpha = 0.6, linewidth = 0.8) +  # Map fill to key
  facet_wrap(~key) + 
  geom_vline(xintercept = lower_quantile, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = upper_quantile, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  scale_fill_scico_d(palette = "managua") +  # Use a Scico palette
  labs(
    x = "Effect Size", 
    y = "Density",
    title = "Distribution of effect sizes by study"
  ) +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(size = 12)
  )



# stack them
plt_density_plot_combined <- df_raw |> 
  ggplot(aes(x = effectSize, fill = key)) +  # Add fill for different keys
  geom_density(alpha = 0.5, linewidth = 0.8) +  # Adjust alpha and linewidth
  scale_fill_scico_d(palette = "managua") +  # Use a Scico palette
  geom_vline(xintercept = lower_quantile, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = upper_quantile, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  labs(
    x = "Effect Size", 
    y = "Density",
    title = "Distribution of effect sizes by study",
    caption = "Dashed lines represent 95% confidence interval"
  ) +
  theme_classic() +  # Cleaner theme
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "bottom",  # Place legend at the bottom
    legend.title = element_blank()  # Remove legend title
  )




plt_4distributions
plt_density_plot_combined


# ---- chunk_8 ----

## 4 hist with free y
plt_4histograms <- df_raw |> 
  ggplot(aes(x = effectSize)) + 
  geom_histogram(aes(fill = key), alpha = 0.6, color = "black", bins = 40) +  # optimal number of bins?
  facet_wrap(~key, scales = "free_y") + 
  geom_vline(xintercept = lower_quantile, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = upper_quantile, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  scale_fill_scico_d(palette = "managua") +  # Use a Scico palette
  labs(
    x = "Effect Size", 
    y = "Density",
    title = "Distribution of effect sizes by study"
  ) +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(size = 16)
  )
plt_4histograms

# NOTES: remove the "key" from the legend


# ---- chunk_9 ----

# Function for this
f_calculate_avg_effectSize_bykey <- function(key_to_be_included, obs_to_be_excluded){
  
  avg_effectSize <- 
    df_raw |> 
    filter(key == key_to_be_included) |> 
    mutate(ID = row_number()) |> # add ID
    filter(ID != obs_to_be_excluded) %>% 
    pull(effectSize) |>
    mean(na.rm=T) 
  
  # Create a data frame to store the result
  result <- tibble(
    key = key_to_be_included,
    excluded_obs = obs_to_be_excluded,
    avg_effectSize = avg_effectSize
  )
  
  return(result)
}

# try
f_calculate_avg_effectSize_bykey(key_to_be_included = "AF",  obs_to_be_excluded = 1) # 0.3842059	
f_calculate_avg_effectSize_bykey(key_to_be_included = "AF",  obs_to_be_excluded = 10) # 0.3855088	


# Apply this function to all keys (could be done with a function and lapply too)
avg_mean_in_AF <- map_df(
  unique(df_raw |> filter(key == "AF") |>  mutate(ID = row_number()) |>  pull(ID)), 
  ~f_calculate_avg_effectSize_bykey(key_to_be_included = "AF", obs_to_be_excluded = .x)
)

avg_mean_in_CC <- map_df(
  unique(df_raw |> filter(key == "CC") |>  mutate(ID = row_number()) |>  pull(ID)), 
  ~f_calculate_avg_effectSize_bykey(key_to_be_included = "CC", obs_to_be_excluded = .x)
)

avg_mean_in_NT <- map_df(
  unique(df_raw |> filter(key == "NT") |>  mutate(ID = row_number()) |>  pull(ID)), 
  ~f_calculate_avg_effectSize_bykey(key_to_be_included = "NT", obs_to_be_excluded = .x)
)

avg_mean_in_OF <- map_df(
  unique(df_raw |> filter(key == "OF") |>  mutate(ID = row_number()) |>  pull(ID)), 
  ~f_calculate_avg_effectSize_bykey(key_to_be_included = "OF", obs_to_be_excluded = .x)
)

# Combine
avg_mean_all <- bind_rows(avg_mean_in_AF, avg_mean_in_CC, avg_mean_in_NT, avg_mean_in_OF)


# quantiles for each key
df_quantiles <- avg_mean_all |> 
  group_by(key) |> 
  summarise(
    lower = quantile(avg_effectSize, 0.025),
    upper = quantile(avg_effectSize, 0.975)
  )




# ------------------------------------------------------------------------- Plot
plt_4_jacknifes <- ggplot(avg_mean_all, aes(x = excluded_obs, y = avg_effectSize)) +
  geom_point(aes(color = key), size = 1, alpha = 0.6) +
  geom_hline(data = df_quantiles, aes(yintercept = lower), linetype = "dashed", color = "black") +
  geom_hline(data = df_quantiles, aes(yintercept = upper), linetype = "dashed", color = "black") +
  facet_wrap(~key, scales = "free") +
  scale_color_scico_d(palette = "managua") +  
  labs(
    x = "Excluded observation ID",
    y = "Average effect size",
    title = "Sensitivity analysis by excluding one observation at a time"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 16),
    legend.position = "none"  
  )

# All together, no keys separated
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
f_avg_effectSize <- function(obs_to_be_excluded) {
  
  avg_effectSize <- 
    df_raw |> 
    mutate(ID = row_number()) |>
    filter(ID != obs_to_be_excluded) %>% 
    summarise(avg_effectSize = mean(effectSize, na.rm = TRUE)) %>% 
    pull(avg_effectSize)
  
  # Create a data frame to store the result
  result <- tibble(
    excluded_obs = obs_to_be_excluded,
    avg_effectSize = avg_effectSize
  )
  
  return(result)
}

f_avg_effectSize(1) # 0.05262971	

# Apply the function to all observations
avg_mean_in_all <- map_df(
  unique(df_raw |> mutate(ID = row_number()) |> pull(ID)), 
  ~f_avg_effectSize(.x)
)



# Plot
plt_1_jacknife <- 
  ggplot(avg_mean_in_all, aes(x = excluded_obs, y = avg_effectSize)) +
  geom_point(size = 1, alpha = 0.6, color = "#2C3E50") +  # Yhtenäinen väri kaikille pisteille
  geom_hline(yintercept = lower_quantile, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = upper_quantile, linetype = "dashed", color = "grey50") +
  labs(
    x = "Excluded Observation ID",
    y = "Average effect Size",
    title = "Excluding One observation at a time - average effect size"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16),
    panel.grid.minor = element_blank()
  )


plt_4_jacknifes
plt_1_jacknife


# ---- chunk_10 ----

# plt_4distributions
plt_4histograms
plt_density_plot_combined
plt_4_jacknifes

# Output folder
out_dir <- "./output/figure_8/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save as pdf files
ggsave(here(out_dir, "plt_density_plot_combined.pdf"), plt_density_plot_combined)
ggsave(here(out_dir, "plt_4histograms.pdf"), plt_4histograms)
ggsave(here(out_dir, "plt_4_jacknifes.pdf"), plt_4_jacknifes)


