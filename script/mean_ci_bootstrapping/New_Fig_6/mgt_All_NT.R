suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(boot)
  library(readxl)
  library(writexl)
  library(tidyverse)
  library(patchwork)
})

# load data
df_arid<-read_xlsx("./input/data/all_dis_cov2_last.xlsx",guess_max = 1000) %>%
    filter(key %in% c("NT"), !Crop_Group %in% c("Grass"),kg_clim %in% c("Arid"))%>%
    dplyr::select(effectSize,key,Crop_Group,N_input,soil_cover,
                  weed_control,rotation) %>%
                  drop_na(Crop_Group)
				  
df_continental<-read_xlsx("./input/data/all_dis_cov2_last.xlsx",guess_max = 1000) %>%
    filter(key %in% c("NT"), !Crop_Group %in% c("Grass"),kg_clim %in% c("Continental"))%>%
    dplyr::select(effectSize,key,Crop_Group,N_input,soil_cover,
                  weed_control,rotation) %>%
                  drop_na(Crop_Group)
				  
df_temperate<-read_xlsx("./input/data/all_dis_cov2_last.xlsx",guess_max = 1000) %>%
    filter(key %in% c("NT"), !Crop_Group %in% c("Grass"),kg_clim %in% c("Temperate"))%>%
    dplyr::select(effectSize,key,Crop_Group,N_input,soil_cover,
                  weed_control,rotation) %>%
                  drop_na(Crop_Group)
				  
df_tropical<-read_xlsx("./input/data/all_dis_cov2_last.xlsx",guess_max = 1000) %>%
    filter(key %in% c("NT"), !Crop_Group %in% c("Grass"),kg_clim %in% c("Tropical"))%>%
    dplyr::select(effectSize,key,Crop_Group,N_input,soil_cover,
                  weed_control,rotation) %>%
                  drop_na(Crop_Group)

df_in<-bind_rows(df_arid,df_continental,df_temperate,df_tropical)
dim(df_in)
# --- helpers ------------------------------------------------------------------
perc <- function(x) 100 * (exp(x) - 1)

stat_summ <- function(x) {
  x <- x[is.finite(x)]
  dplyr::tibble(
    n      = length(x),
    mean   = if (length(x)) mean(x) else NA_real_,
    median = if (length(x)) stats::median(x) else NA_real_,
    p25    = if (length(x)) as.numeric(stats::quantile(x, 0.25, names = FALSE)) else NA_real_,
    p75    = if (length(x)) as.numeric(stats::quantile(x, 0.75, names = FALSE)) else NA_real_
  )
}

boot_mean_ci <- function(x, R = 1000, conf = 0.95, seed = 7) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(dplyr::tibble(ci_low = NA_real_, ci_high = NA_real_))
  set.seed(seed)
  my.mean <- function(xx, i) mean(xx[i])
  b <- boot::boot(x, statistic = my.mean, R = R)
  bca <- try(boot::boot.ci(b, type = "bca", conf = conf), silent = TRUE)
  if (!inherits(bca, "try-error") && !is.null(bca$bca)) {
    dplyr::tibble(ci_low = bca$bca[4], ci_high = bca$bca[5])
  } else {
    perc_ci <- try(boot::boot.ci(b, type = "perc", conf = conf), silent = TRUE)
    ci_low  <- if (!inherits(perc_ci, "try-error") && !is.null(perc_ci$percent)) perc_ci$percent[4] else NA_real_
    ci_high <- if (!inherits(perc_ci, "try-error") && !is.null(perc_ci$percent)) perc_ci$percent[5] else NA_real_
    dplyr::tibble(ci_low = ci_low, ci_high = ci_high)
  }
}

# --- core summariser for one climate df --------------------------------------
summarise_nt_climate <- function(df, cat_label = "Arid", key_val = "NT",
                                 trim_upper_log = 2.5, R = 1000, conf = 0.95) {
  # Normalize flags and build combo IDs
  df2 <- df %>%
    dplyr::mutate(dplyr::across(c(N_input, soil_cover, weed_control, rotation),
                                ~ tolower(as.character(.x)))) %>%
    dplyr::mutate(dplyr::across(c(N_input, soil_cover, weed_control, rotation),
                                ~ dplyr::recode(.x,
                                  "yes"="yes","no"="no","y"="yes","n"="no",
                                  "1"="yes","0"="no","true"="yes","false"="no",
                                  .default=.x))) %>%
    dplyr::mutate(
      ID = dplyr::case_when(
        N_input=="yes" & soil_cover=="no"  & weed_control=="no" & rotation=="no" ~ "only N_input",
        N_input=="no"  & soil_cover=="yes" & weed_control=="no" & rotation=="no" ~ "only soil_cover",
        N_input=="no"  & soil_cover=="no"  & weed_control=="yes" & rotation=="no" ~ "only weed_control",
        N_input=="no"  & soil_cover=="no"  & weed_control=="no" & rotation=="yes" ~ "only rotation",

        N_input=="yes" & soil_cover=="yes" & weed_control=="no" & rotation=="no" ~ "N_input & soil_cover",
        N_input=="yes" & soil_cover=="no"  & weed_control=="yes" & rotation=="no" ~ "N_input & weed_control",
        N_input=="yes" & soil_cover=="no"  & weed_control=="no" & rotation=="yes" ~ "N_input & rotation",
        N_input=="no"  & soil_cover=="yes" & weed_control=="yes" & rotation=="no" ~ "soil_cover & weed_control",
        N_input=="no"  & soil_cover=="yes" & weed_control=="no" & rotation=="yes" ~ "soil_cover & rotation",
        N_input=="no"  & soil_cover=="no"  & weed_control=="yes" & rotation=="yes" ~ "weed_control & rotation",

        N_input=="yes" & soil_cover=="yes" & weed_control=="yes" & rotation=="no" ~ "N_input & soil_cover & weed_control",
        N_input=="yes" & soil_cover=="no"  & weed_control=="yes" & rotation=="yes" ~ "N_input & weed_control & rotation",
        N_input=="no"  & soil_cover=="yes" & weed_control=="yes" & rotation=="yes" ~ "soil_cover & weed_control & rotation",
        N_input=="yes" & soil_cover=="yes" & weed_control=="yes" & rotation=="yes" ~ "N_input & soil_cover & weed_control & rotation",
        TRUE ~ NA_character_
      )
    ) %>%
    tidyr::drop_na(ID)

  # Optional trim on log scale (like your <= 2.5)
  df3 <- df2 %>% dplyr::filter(effectSize <= trim_upper_log)

  # Overall (log scale)
  overall_log <- stat_summ(df3$effectSize) %>%
    dplyr::bind_cols(boot_mean_ci(df3$effectSize, R = R, conf = conf)) %>%
    dplyr::mutate(
      Management_type = key_val,
      cov   = "overall",
      cat   = cat_label,
      label = cat,
      .before = 1
    )

  # By combination (log scale)
  combos_log <- df3 %>%
    dplyr::group_by(ID) %>%
    dplyr::group_modify(~ dplyr::bind_cols(
      stat_summ(.x$effectSize),
      boot_mean_ci(.x$effectSize, R = R, conf = conf)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Management_type = key_val,
      cov   = ID,
      cat   = cat_label,
      label = cat,
      n, mean, median, p25, p75, ci_low, ci_high
    )

  # Bind + convert all summaries to percent
  dplyr::bind_rows(overall_log, combos_log) %>%
    dplyr::mutate(dplyr::across(c(mean, median, p25, p75, ci_low, ci_high), perc)) %>%
    dplyr::arrange(factor(cov, levels = c("overall")), cov)
}

# --- apply to your four prepared datasets ------------------------------------
nt_arid         <- summarise_nt_climate(df_arid,cat_label = "Arid") %>% 
                                        filter(n>2, !cov=="overall") 

nt_continental  <- summarise_nt_climate(df_continental,  cat_label = "Continental") %>% 
                                        filter(n>2,!cov=="overall") 

nt_temperate    <- summarise_nt_climate(df_temperate,    cat_label = "Temperate")%>% 
                                        filter(n>2,!cov=="overall") 

nt_tropical     <- summarise_nt_climate(df_tropical,cat_label = "Tropical")%>% 
                                          filter(n>2,!cov=="overall") 

# Combine and save
nt_all_climates <- dplyr::bind_rows(nt_arid, nt_continental, nt_temperate, nt_tropical)

as.data.frame(nt_all_climates)

dir.create("./output/Figure_data/Fig6", recursive = TRUE, showWarnings = FALSE)
writexl::write_xlsx(nt_arid,        "./output/Figure_data/Fig6/NT_Arid_management_combos.xlsx")
writexl::write_xlsx(nt_continental, "./output/Figure_data/Fig6/NT_Continental_management_combos.xlsx")
writexl::write_xlsx(nt_temperate,   "./output/Figure_data/Fig6/NT_Temperate_management_combos.xlsx")
writexl::write_xlsx(nt_tropical,    "./output/Figure_data/Fig6/NT_Tropical_management_combos.xlsx")
writexl::write_xlsx(nt_all_climates,"./output/Figure_data/Fig6/NT_ALL_climates_management_combos.xlsx")

# Plotting----------------------------------------------------------------------

tr_theme2<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.background = element_rect(fill='transparent',color=NA),
         legend.key=element_blank(),
         legend.box.background = element_rect(fill='transparent'),
         axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1),
         axis.text = element_text(size = 16),
          axis.title=element_blank(),
         # axis.title= element_text(size=27),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
       )


tr_theme3<-theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.background = element_rect(fill='transparent',color=NA),
         legend.key=element_blank(),
         legend.box.background = element_rect(fill='transparent'),
         axis.line = element_line(linewidth= 0.25, colour = "black", linetype=1),
         axis.text = element_text(size = 16),
          axis.x.title=element_blank(),
         axis.title= element_text(size=16),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
       )

p1 <- nt_arid %>%
  mutate(cov = factor(cov, levels = c(
    "only weed_control",
    "soil_cover & weed_control",
    "N_input & weed_control",
    "N_input & soil_cover & weed_control",
    "N_input & soil_cover & weed_control & rotation"
  ))) %>%
  ggplot(aes(x = cov, y = mean)) +
  geom_point(fill = "#000000", color = "#000000", size = 1.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                color = "#000000", width = 0.1, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted",
             color = "red", linewidth = 1) +
  scale_y_continuous(limits = c(-34, 75)) +
  scale_x_discrete(labels = c(
    "only weed_control",
    "soil cover + weed control",
    "N input + weed control",
    "N input + soil cover + weed control",
    "N input + soil cover + weed control + rotation"
  )) +
  tr_theme2 + coord_flip()

p2 <- nt_continental %>%
  mutate(cov = factor(cov, levels = c(
    "only soil_cover",
    "soil_cover & weed_control",
    "N_input & weed_control",
    "soil_cover & weed_control & rotation",
    "N_input & weed_control & rotation",
    "N_input & soil_cover & weed_control",
    "N_input & soil_cover & weed_control & rotation"
  ))) %>%
  ggplot(aes(x = cov, y = mean)) +
  geom_point(fill = "#000000", color = "#000000", size = 1.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                color = "#000000", width = 0.1, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted",
             color = "red", linewidth = 1) +
  scale_y_continuous(limits = c(-34, 75)) +
  scale_x_discrete(labels = c(
    "only soil cover",
    "soil cover  + weed control",
    "N input  + weed control",
    "soil cover  + weed control  + rotation",
    "N input  + weed control  + rotation",
    "N input  + soil cover  + weed control",
    "N input  + soil cover  + weed control  + rotation"
  )) +
  tr_theme2 + coord_flip()

p3 <- nt_temperate %>%
  mutate(cov = factor(cov, levels = c(
    "only weed_control",
    "soil_cover & weed_control",
    "N_input & soil_cover",         # <-- underscore fixed
    "N_input & weed_control",
    "N_input & weed_control & rotation",
    "N_input & soil_cover & weed_control",
    "N_input & soil_cover & weed_control & rotation"
  ))) %>%
  ggplot(aes(x = cov, y = mean)) +
  geom_point(fill = "#000000", color = "#000000", size = 1.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                color = "#000000", width = 0.1, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted",
             color = "red", linewidth = 1) +
  scale_y_continuous(limits = c(-34, 75)) +
  scale_x_discrete(labels = c(
    "only weed control",
    "soil_cover + weed control",
    "N_input + soil cover",
    "N_input + weed control",
    "N_input + weed control + rotation",
    "N_input + soil cover + weed control",
    "N_input + soil cover + weed control + rotation"
  )) +
  tr_theme2 + coord_flip()

p4 <- nt_tropical %>%
  mutate(cov = factor(cov, levels = c(
    "N_input & weed_control",
    "N_input & weed_control & rotation",
    "N_input & soil_cover & weed_control",
    "N_input & soil_cover & weed_control & rotation"
  ))) %>%
  ggplot(aes(x = cov, y = mean)) +
  geom_point(fill = "#000000", color = "#000000", size = 1.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                color = "#000000", width = 0.1, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted",
             color = "red", linewidth = 1) +
  scale_y_continuous(limits = c(-34, 75)) +
  ylab("% Change (Crop Yield)") + xlab("") +
  scale_x_discrete(labels = c(
    "N input  + weed control",
    "N input  + weed control  + rotation",
    "N input  + soil cover  + weed control",
    "N input  + soil cover  + weed control  + rotation"
  )) +
  tr_theme3 + coord_flip()

mgt_plot <- p1 + p2 + p3 + p4 + plot_layout(ncol = 1)
mgt_plot

ggsave(mgt_plot,filename = "./output/new_graphs/fig6.pdf",
       width = 25,height = 20,
       dpi = 300, units = "cm")


ggsave(mgt_plot,filename = "./output/new_graphs/fig6.png",
       width = 23,height = 25,
       dpi = 300, units = "cm")

