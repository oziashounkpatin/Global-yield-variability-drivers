xgb_spatial_rfe_optimize_range <- function(df, response_col, coord_cols,
                                           candidate_ranges = c(10000, 20000, 40000),
                                           subset_sizes = c(3, 5, 7, 9, 10),
                                           grid = NULL) {
  library(sf)
  library(blockCV)
  library(xgboost)
  library(caret)
  library(Matrix)
  
  # Ensure target is numeric and check for NA/infinite values
  df[[response_col]] <- as.numeric(df[[response_col]])
  if (any(is.na(df[[response_col]]))) stop("Response column contains NAs.")
  
  # Ensure predictors are numeric and check for NA/infinite values
  df[, !(names(df) %in% c(response_col, coord_cols))] <- lapply(
    df[, !(names(df) %in% c(response_col, coord_cols))], 
    function(x) {
      x <- as.numeric(x)
      if (any(is.na(x))) stop("Predictors contain NAs.")
      if (any(is.infinite(x))) stop("Predictors contain Inf values.")
      return(x)
    }
  )
  
  # Convert lon/lat to numeric
  df$lon <- as.numeric(df$lon)
  df$lat <- as.numeric(df$lat)
  
  # Convert to sf object
  df_sf <- st_as_sf(df, coords = coord_cols, crs = 4326)
  response <- df[[response_col]]
  predictors <- df[, !(names(df) %in% c(response_col, coord_cols))]
  
  # Default tuning grid if none provided
  if (is.null(grid)) {
    grid <- expand.grid(
      eta = c(0.05, 0.1),
      max_depth = c(3, 6),
      subsample = 0.8,
      colsample_bytree = 0.8,
      nrounds = 100
    )
  }
  
  results_list <- list()

  for (range in candidate_ranges) {
    message("Running RFE with spatial range: ", range)
    
    # Spatial folds using createFolds (this replaces spatialBlock)
    folds <- createFolds(response, k = 5)
    
    # Define custom caretFuncs
    xgb_rfeFuncs <- caretFuncs
    
    # Fit function with grid search and cross-validation
    xgb_rfeFuncs$fit <- function(x, y, first, last, ...) {
      x <- as.matrix(x)
      dtrain <- xgb.DMatrix(data = x, label = y)
      best_rmse <- Inf
      best_model <- NULL
      
      for (i in 1:nrow(grid)) {
        params <- list(
          objective = "reg:squarederror",
          eval_metric = "rmse",
          eta = grid$eta[i],
          max_depth = grid$max_depth[i],
          subsample = grid$subsample[i],
          colsample_bytree = grid$colsample_bytree[i],
          verbosity = 0
        )
        
        # Cross-validation with createFolds
        cv <- xgb.cv(
          params = params,
          data = dtrain,
          nrounds = grid$nrounds[i],
          folds = folds,
          early_stopping_rounds = 10,
          verbose = 0
        )
        
        if (min(cv$evaluation_log$test_rmse_mean) < best_rmse) {
          best_rmse <- min(cv$evaluation_log$test_rmse_mean)
          best_model <- xgb.train(
            params = params,
            data = dtrain,
            nrounds = cv$best_iteration,
            verbose = 0
          )
        }
      }
      return(best_model)
    }
    
    # Prediction function
    xgb_rfeFuncs$pred <- function(object, x) {
      predict(object, xgb.DMatrix(as.matrix(x)))
    }
    
    # Ranking function
    xgb_rfeFuncs$rank <- function(object, x, y) {
      imp <- xgb.importance(model = object)
      full_names <- colnames(x)
      imp_full <- data.frame(Variables = full_names, Overall = 0)
      imp_full$Overall[match(imp$Feature, full_names)] <- imp$Gain
      imp_full
    }
    
    # rfeControl with folds
    ctrl <- rfeControl(
      functions = xgb_rfeFuncs,
      method = "cv",
      number = 1,
      indexOut = folds,
      verbose = FALSE
    )
    
    # Run RFE
    rfe_result <- rfe(
      x = predictors,
      y = response,
      sizes = subset_sizes,
      rfeControl = ctrl,
      functions = xgb_rfeFuncs
    )
    
    results_list[[as.character(range)]] <- list(
      rfe = rfe_result,
      rmse = min(rfe_result$results$RMSE),
      best_vars = rfe_result$optVariables
    )
  }
  
  # Select best range based on RMSE
  best_range <- names(which.min(sapply(results_list, function(x) x$rmse)))
  best_result <- results_list[[best_range]]
  
  list(
    best_range = as.numeric(best_range),
    best_rmse = best_result$rmse,
    best_features = best_result$best_vars,
    rfe_object = best_result$rfe,
    all_results = results_list
  )
}


library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(magrittr)
library(stringr)
library(boot)
library(bootES)
library(rcompanion) 
library(writexl)
library("plotrix")
library(ggh4x)
library(showtext)
library(egg)
library(patchwork)

# Convert ES to perc
perc<- function(data){
  data_conv=100*(exp(data) - 1)
  return(data_conv)
}


df_af<-read_xlsx("./input/data/reg_data.xlsx",guess_max = 1000) %>%
    filter(key %in% c("AF"), !Crop_Group %in% c("Grass")) %>%
    dplyr::select(effectSize,x,y,aridity,kg_clim,pH,soc,phosphorus,bd,sand, silt,clay,dem,landform,wrb_new) %>%
    mutate(ES=perc(effectSize)) %>%
    filter(!ES > 100) %>%
    select(!effectSize)  %>%
    dplyr::rename(effectSize=ES, wrb=wrb_new) %>%
    relocate(last_col(), .after = y)

df<-df_af[,3:ncol(df_af)]

set.seed(123)
# Simulated dataset with coordinates
df$target <- df[,1]
df$lon <- df_af[,1]
df$lat <- df_af[,2]
colnames(df) <- paste0("X", 1:ncol(df))

# Run the function to optimize spatial block range
result <- xgb_spatial_rfe_optimize_range(
  df,
  response_col = "target",
  coord_cols = c("lon", "lat"),
  candidate_ranges = c(5000, 10000, 20000)
)

# View results
result$best_range  # Best spatial range
result$best_features  # Selected features
plot(result$rfe_object)  # Plot RFE results
