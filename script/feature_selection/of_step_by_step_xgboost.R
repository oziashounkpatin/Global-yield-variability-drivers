# Install/load packages
library(caret)
library(scico)
library(xgboost)
library(dplyr)
library(spdep)        # For spatial weights
library(blockCV)      # For spatial cross-validation
library(raster)       # If you have raster-based spatial layers
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
library(mlr) 
library(doParallel)
library(spdep)  # For spatial autocorrelatio
library(stringr)
library(terra)
library(SHAPforxgboost)
library(tmap)

# Convert ES to perc
perc<- function(data){
  data_conv=100*(exp(data) - 1)
  return(data_conv)
}

df<-read_xlsx("./input/data/reg_data.xlsx",guess_max = 1000) %>%
    filter(key %in% c("OF"), !Crop_Group %in% c("Grass")) %>%
    dplyr::select(effectSize,x,y,aridity,pH,soc,phosphorus,bd,
                  sand, silt,clay,dem,landform,wrb_new) %>%
    mutate(ES=perc(effectSize)) %>%
    filter(!ES > 100) %>%
    dplyr::select(!effectSize)  %>%
    dplyr::rename(effectSize=ES, wrb=wrb_new) %>%
    relocate(last_col(), .after = y) %>%
    drop_na(effectSize) %>%
    mutate(across(c(landform, wrb), as.factor))

data <- createDummyFeatures(
  df, target = "effectSize",
  cols = c(
    "landform",
    "wrb"
  )
)

# Train Test Split
set.seed(123)
# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
parts = createDataPartition(data$effectSize, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]
X_train = train[,4:ncol(data)]
y_train = train[,3]
                
                
# # Simulated dataset with coordinates
# df$target <- df[,1]
# df$lon <- df_af[,1]
# df$lat <- df_af[,2]
# colnames(df) <- paste0("X", 1:ncol(df))

# # Separate predictors and response
# data<-data[,3:ncol(data)]
# predictors <- data[, 2:ncol(data)]
# response <- df[, 1]
# coords <-df_af[,1:2] # Must match row-wise with df

# Simulated dataset
set.seed(123)

# Spatial coordinates
coords <- data[, c("x", "y")]

# Define trainControl for hyperparameter tuning
train_control <- trainControl(
  method = "cv",  # Cross-validation
  number = 5,     # Number of folds
  search = "grid" # Grid search for hyperparameter tuning
)

# Define hyperparameter grid for XGBoost
xgb_grid <- expand.grid(
  nrounds = c(50, 100),         # Number of boosting rounds
  max_depth = c(3, 6),          # Maximum tree depth
  eta = c(0.01, 0.1),           # Learning rate
  gamma = c(0, 1),              # Minimum loss reduction
  colsample_bytree = c(0.7, 1), # Subsample ratio of columns
  min_child_weight = c(1, 5),   # Minimum sum of instance weight
  subsample = c(0.7, 1)         # Subsample ratio of rows
)

# Define RFE control
rfe_control <- rfeControl(
  functions = caretFuncs,  # Use caret's default functions
  method = "cv",           # Cross-validation
  number = 5               # Number of folds
)

data<-na.omit(as.data.frame(data))


# Set up parallel backend
num_cores <- parallel::detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)              # Create a cluster
registerDoParallel(cl)                    # Register the parallel backend

# Simulated dataset
set.seed(200)
# Perform RFE with XGBoost
rfe_results <- rfe(
  x = X_train,  # Features
  y = y_train,                                # Target variable
  sizes = c(1, 2, 3),                                 # Number of features to test
  rfeControl = rfe_control,                           # RFE control
  method = "xgbTree",                                 # XGBoost model
  tuneGrid = xgb_grid,   
  allowParallel = TRUE,  # Enable parallel processing# Hyperparameter grid
  trControl = train_control                           # Train control
)

# Save model
of_model <- rfe_results$fit
path_model<-"./output/model/"
saveRDS(rfe_results, str_c(path_model, "of_rfe_results.rds"))
saveRDS(of_model, str_c(path_model, "of_model_fit.rds"))
#of_model<-readRDS(str_c(path_model, "of_model_fit.rds"))

# Print RFE results
print(rfe_results)

# Best features
best_features <- predictors(rfe_results)
cat("Best Features Selected by RFE:\n")
print(best_features)

# Check spatial autocorrelation (Moran's I)
# neighbors <- spdep::knn2nb(knearneigh(coords, k = 5))  # Define neighbors
# weights <- spdep::nb2listw(neighbors, style = "W")    # Create spatial weights
# moran_test <- spdep::moran.test(data$effectSize, weights) # Moran's I test
# cat("\nMoran's I Test Results:\n")
# print(moran_test)

# Predict the map

#---define grid
area_grid<-rast("C:/Users/hounkpk1/Food_System/input/covariates/grid_5arcmin/areagrid.tif")
#r_area_depth_grid<-subset(area_grid, names(of_model$finalModel))

area_grid<-rast("C:/Users/hounkpk1/Food_System/input/covariates/grid_5arcmin/areagrid.tif")
names(area_grid)[[43]]<-"phosphorus"

wrb_ldf<-rast("./input/wrb_landform/wrb_ldf.tif")
area_grid<-c(area_grid,wrb_ldf)

model_grid<-subset(area_grid, best_features)

of_map_es<-predict(model_grid,of_model)

# Load mask
crop_mask<-rast("C:/Users/hounkpk1/Food_System/input/cropland_mask/mask_crop.tif")
of_map_es_res<-resample(of_map_es,crop_mask)
crp_of_map_es<- of_map_es_res %>% terra::mask(crop_mask, inverse=F)
writeRaster(crp_of_map_es,"./output/Factor_Analysis/of/of_map_es.tif",overwrite=T)
plot(crp_of_map_es)

#Load class of aridity and phosphorus
ar_p<-rast("./output/Factor_Analysis/of/r_recod.tif")
es_ar_p<-c(crp_of_map_es,ar_p)
names(es_ar_p)<-c("es","class")
df_es_ar_p<-as.data.frame(es_ar_p) %>% drop_na()

df_es_ar_p$class<-as.factor(df_es_ar_p$class)
summary(df_es_ar_p$class)

# Map the af effect size

es_class_breaks <- c(-40,-20,0,20,  
                     30, 40,50,60,70,80)

es_class_labels <- c("-40 - -20 ", "-20 - 0","0 - 10",
                  "10 - 20", "20 - 30", "30 - 40 ", "40 - 50", 
                  "50 - 60", "60 - 70 ", "> 70")

res_pal <- scico(n = 10, direction = 1, palette = "vik",
                 begin = 0.4, end = 1)

# map with tmap
es_mag_maps<-tm_shape(crp_of_map_es)+
        tm_raster(palette = res_pal,
                  midpoint = 0,
                  # showNA = F,
                  # colorNA = NULL,
                  title = NA,
                  labels = es_class_labels,
                  breaks = es_class_breaks)+
        tm_shape(sf_shoreLine) +
        tm_lines(col = "grey",lwd = 0.25)+
        tm_layout(legend.show = T,
                  title.frame=NA,
                  bg.color = NA, 
                  frame = FALSE)
es_mag_maps

# Saving the maps
png_es_of_map<- es_mag_maps +
    tm_layout(legend.show=FALSE)

# save with tmap 
tmap_save(es_mag_maps,
          filename =  "./output/Factor_Analysis/of/es_of_map.pdf",
          width = 80,units='mm', dpi = 450)

# save with tmap 
tmap_save(png_es_of_map,
          filename =  "./output/Factor_Analysis/of/es_of_map.png",
          width = 80,units='mm', dpi = 450)

# Create the boxplot
# Sample data
set.seed(123)

# Choose a scico palette
palette_colors <- c("#001260", "#06558B", "#71A7C4", "#EBE5E0", "#D29773", "#AA4613")  # "batlow" or try "roma", "oslo", "lajolla", etc.

# Create the boxplot
plt_map_es<-ggplot(df_es_ar_p, aes(x = class, y = es, fill = class)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
  #geom_quasirandom(size = 1.5, alpha = 0.6, width = 0.2) +
  scale_fill_manual(values = palette_colors) +
  labs(
    title = "",
    x = NULL,
    y = "% change (crop yield)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = elemeof_text(face = "bold", hjust = 0.5),
    axis.text = elemeof_text(color = "black", size=20),
    axis.title = elemeof_text(size=20),
    axis.line = elemeof_line(color = "black"),
    axis.ticks = elemeof_line(color = "black")
  )

ggsave(plt_map_es,filename = "./output/Factor_Analysis/of/of_boxplot.png",
       width = 50, height = 30, dpi = 450, units = "cm")


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#   SHAPLEY VALUES -----------#


# Prepare data for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(train[, best_features]), label = train$effectSize)

# Train xgboost model (simple parameters for demonstration)
xgb_model <- xgboost(
  data = dtrain,
  nrounds = 50,
  max_depth = 3, 
  eta = 0.1, gamma = 0, 
  colsample_bytree = 1, 
  min_child_weight = 1,
  subsample = 1,
  objective = "reg:squarederror",
  verbose = 0
)

# Compute shapley values
data_shap<-as.matrix(X_train[,best_features],  drop = FALSE)
shap_values <- shap.values(xgb_model = xgb_model, X_train =data_shap)

# SHAP importance (mean absolute SHAP value per feature)
# Get mean absolute SHAP values and sort
shap_importance <- shap_values$mean_shap_score
top5_vars <- names(sort(shap_importance, decreasing = TRUE))[1:5]

# Visualise shapley values
# Prepare SHAP long data for the top 5 variables
shap_long <- shap.prep(
  xgb_model = xgb_model,
  X_train = data_shap[, top5_vars]
)

# Plot SHAP summary for the top 5 variables
# Save as PNG
png("./output/Factor_Analysis/of/of_shap_summary_plot.pdf", width = 800, height = 600, res = 120)
shap.plot.summary(shap_long)
dev.off()

# Map the shaley values 

map_model_grid<-as.matrix(as.data.frame(model_grid), drop=F)

map_shap_values <- shap.values(xgb_model = xgb_model, X_train = map_model_grid)

str(map_shap_values)

head(map_shap_values$shap_score[,1:6])
tail(map_shap_values$shap_score[,1:6])

write_xlsx(shap_values,"./output/Factor_Analysis/of/of_grid_shap_vales.xlsx")
