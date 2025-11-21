# ---- chunk_1 ----

knitr::opts_chunk$set(echo = TRUE)


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
perc<- function(data){
  data_conv=100*(exp(data) - 1)
  return(data_conv)
}

# Read the data
df_raw <- read_excel("dataset_Ozias.xlsx")



# Explore data: number of studies and crops
length(unique(df_raw$key)) # 4 studies?
length(unique(df_raw$Crop_Group)) # 8 crop

names(df_raw)


# ---- chunk_3 ----

# Group data by studies and count the number of cases
df_raw<-df_raw %>% 
  mutate(ES=perc(effectSize)) %>%
  filter(!ES > 100) %>%
  dplyr::select(!effectSize)  %>%
  dplyr::rename(effectSize=ES) 

  
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


# Save as pdf files
ggsave(here("figures", "plt_density_plot_combined.pdf"), plt_density_plot_combined)
ggsave(here("figures", "plt_4histograms.pdf"), plt_4histograms)
ggsave(here("figures", "plt_4_jacknifes.pdf"), plt_4_jacknifes)


# ---- chunk_11 ----

# Example without function
# df_raw %>%
#   filter(key != "OF") %>%
#   group_by(Crop_Group) %>%
#   summarise(avg_effectSize = mean(effectSize, na.rm = TRUE))

# Function for this (drop when Crop_Group is NA)
f_calculate_avg_effectSize <- function(key_to_be_excluded){
  
  avg_effectSize <- 
    df_raw %>% 
    filter(key != key_to_be_excluded) %>% 
    group_by(Crop_Group) %>% 
    summarise(avg_effectSize = mean(effectSize, na.rm = TRUE)) %>% 
    drop_na() %>% 
    mutate(excluded_key = key_to_be_excluded) # Lisätään sarake, joka ilmoittaa poistetun keyn
  
  return(avg_effectSize)
}

# Example
f_calculate_avg_effectSize("OF")

# Run function for all keys
all_keys <- unique(df_raw$key)

# Sovelletaan funktiota kaikkiin key-arvoihin ja tallennetaan tulokset
results <- map_df(all_keys, f_calculate_avg_effectSize)
results


# ---- chunk_12 ----

# Wide 
results_wide_arranged <- results %>%
  pivot_wider(names_from = Crop_Group, values_from = avg_effectSize) %>%
  arrange(excluded_key)

results_wide_arranged

# Long
results_long <- results_wide_arranged %>%
  pivot_longer(cols = -excluded_key, names_to = "Crop_Group", values_to = "avg_effectSize")



# Draw a combined plot 
# similarly as done below wiht plt_combined_arranged -- use facte_grid etc
library(scico)
plt_jacknife <-
  ggplot(results_long, 
         aes(x = factor(excluded_key, levels = unique(excluded_key)), 
             y = avg_effectSize, color = Crop_Group, group = Crop_Group)) +
  geom_line(linewidth = 1.2) +
  facet_grid(Crop_Group ~ ., 
             scales = "free_y")+#,
             #labeller = as_labeller(c("AF" = "AF", "CC" = "CC", "NT" = "NT"))) + # not necessary?
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.position = "none") +
  scale_color_scico_d(palette = "roma") +
  labs(title = "Sensitivity analysis - Jacknife",
       x = "Excluded study",
       y = "Average effect size") 
 

plt_jacknife


# ---- chunk_13 ----

# Load necessary libraries
library(tidyverse)
library(fixest)

# Read the data
df_raw <- read_csv("data_johannes.csv")

# Add a new column ln_gni as the natural logarithm of gni
df_raw <- df_raw %>% 
  mutate(ln_gni = log(gni))

# Explore data: number of countries, crops and years
length(unique(df_raw$country)) # 81 countries
unique(df_raw$year) %>% sort() # years 2000-2020



# Group data by country and count the number of cases
df_raw %>% 
  group_by(country) %>% 
  summarise(Number_of_cases = n()) %>% 
  arrange(Number_of_cases)

# Create a list of countries with more than 80 observations
#my_cntrylist_over80 <- 
my_countries_obs <-
  df_raw %>% 
  group_by(country) %>% 
  summarise(Number_of_cases = n()) %>% 
 # filter(Number_of_cases > 80) %>% 
  arrange(Number_of_cases)# %>%   pull(country)


# ---- chunk_14 ----

# Create a function to run feols excluding one country at a time
f_exclude_countries <- function(country_tobe_excluded) {
  feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + 
           electricityRural + phoneSub + polStab + export + regionalName + as.factor(year),
         cluster = ~ regionalName, 
         data = df_raw %>% filter(country != country_tobe_excluded)) %>%
    broom::tidy() %>%
    dplyr::select(term, estimate, p.value) %>%
    mutate(excluded_country = country_tobe_excluded)
}

# Example function calls
f_exclude_countries("Angola")
f_exclude_countries("Burundi")

# Use the function for all countries
my_countries <- unique(df_raw$country)

# Apply the function to all countries and store the results
results <- map_df(my_countries, f_exclude_countries)

# Widen the results and rename p-value columns ---> not sure if this is needed
results_wider <- results %>%
  pivot_wider(names_from = term, values_from = c(estimate, p.value)) %>%
  rename(
    pval_intercept = `p.value_(Intercept)`,
    pval_ln_gni = `p.value_ln_gni`,
    pval_agriShr = `p.value_agriShr`,
    pval_agriEmp = `p.value_agriEmp`,
    pval_msch = `p.value_msch`,
    pval_electricityRural = `p.value_electricityRural`,
    pval_phoneSub = `p.value_phoneSub`,
    pval_polStab = `p.value_polStab`,
    pval_export = `p.value_export`
  )

results_wider


# ---- chunk_15 ----

library(flextable)
library(modelsummary)

model <-   feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + 
                   electricityRural + phoneSub + polStab + export + regionalName + as.factor(year),
                 cluster = ~ regionalName, 
                 data = df_raw)



# Create the table for the feols model
model_table <- modelsummary(model, stars = T, output = "flextable")
model_table
# uncomment this
#save_as_docx(model_table, path = ("selected_results.docx"))


# ---- chunk_16 ----

results_arranged <- map_df(unique(my_countries_obs$country), f_exclude_countries)

results_wider_arranged <- results_arranged %>%
  pivot_wider(names_from = term, values_from = c(estimate, p.value)) %>%
  rename(
    pval_intercept = `p.value_(Intercept)`,
    pval_ln_gni = `p.value_ln_gni`,
    pval_agriShr = `p.value_agriShr`,
    pval_agriEmp = `p.value_agriEmp`,
    pval_msch = `p.value_msch`,
    pval_electricityRural = `p.value_electricityRural`,
    pval_phoneSub = `p.value_phoneSub`,
    pval_polStab = `p.value_polStab`,
    pval_export = `p.value_export`
  )


results_long_arranged <- results_wider_arranged %>%
  pivot_longer(
    cols = starts_with("estimate_"),
    names_to = "variable",
    values_to = "value"
  ) %>% 
  dplyr::select(excluded_country, variable, value) %>%
  drop_na()



# Select only some variables
results_long_arranged <- results_long_arranged %>% 
  filter(variable %in% c("estimate_(Intercept)", "estimate_ln_gni",                                 
 "estimate_agriShr",                                  
  "estimate_agriEmp",                                  
  "estimate_msch",                                     
  "estimate_electricityRural",                         
  "estimate_phoneSub",                                 
  "estimate_polStab",                                  
  "estimate_export"   ))


# Draw a combined plot
library(scico)

plt_combined_arranged <- ggplot(results_long_arranged, aes(x = factor(excluded_country, levels = unique(excluded_country)), y = value, color = variable, group = variable)) +
  geom_line(size = 1.2) + 
  facet_grid(variable ~ .,
             scales = "free_y",
             labeller = as_labeller(c("estimate_(Intercept)" = "Intercept",
                                      estimate_ln_gni = "Log GNI",
                                      estimate_agriShr = "Agri Share",
                                      estimate_agriEmp = "Agri Emp",
                                      estimate_msch = "Mean Sch",
                                      estimate_electricityRural = "Elec Acc",
                                      estimate_phoneSub = "Phone Sub",
                                      estimate_polStab = "Pol Stab",
                                      estimate_export = "Exports"))) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.position = "none") +
  scale_color_scico_d(palette = "roma") +
  labs(title = "Impact of excluding a country on regression output",
       x = "Excluded country countries with fewer observations         countries with more observations",
       y = "Estimate")


plt_combined_arranged

# uncomment this

# ggsave("sensitivity_plot_x_axis_based_on_N_obs.png", plot = plt_combined_arranged, dpi = 300, width = 300, height = 225, units = "mm")
# 
# ggsave("sensitivity_plot_x_axis_based_on_N_obs.pdf", plot = plt_combined_arranged)
