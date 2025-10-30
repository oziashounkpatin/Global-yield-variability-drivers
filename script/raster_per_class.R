library(terra)
#---define grid
area_grid<-rast("C:/Users/hounkpk1/Food_System/input/covariates/grid_5arcmin/areagrid.tif")
ar_p<-subset(area_grid, c("wrb","landform"))
wrb<-as.factor(ar_p[[1]])
ldf<-as.factor(ar_p[[2]])

# 2. Extract class IDs and names
wrb_class_ids <- levels(wrb)[[1]]$ID
ldf_class_ids <- levels(ldf)[[1]]$ID

wrb_class_names <- paste0("wrb",".",wrb_class_ids)
ldf_class_names <- paste0("landform",".",ldf_class_ids)

# 3. Create one raster per class (binary masks)
wrb_per_class <- lapply(wrb_class_ids, function(wrb_cls) {
  wrb_rc <- wrb == wrb_cls
  #names(wrb_rc) <- wrb_class_ids[wrb_cls]
  return(wrb_rc)
})

ldf_per_class <- lapply(ldf_class_ids, function(ldf_cls) {
  ldf_rc <- ldf == ldf_cls
  #names(ldf_rc) <- ldf_class_ids[ldf_cls]
  return(ldf_rc)
})

# 4. Stack them together (optional)
wrb_class_stack <- rast(wrb_per_class)
names(wrb_class_stack)<-wrb_class_names

ldf_class_stack <- rast(ldf_per_class)
names(ldf_class_stack)<-ldf_class_names

# 5. Plot (optional)
plot(wrb_class_stack)

# Save a plot
wrb_ldf<-c(wrb_class_stack,ldf_class_stack)
writeRaster(wrb_class_stack,"./input/wrb_landform/wrb_class_stack.tif")
writeRaster(ldf_class_stack,"./input/wrb_landform/ldf_class_stack.tif")
writeRaster(wrb_ldf,"./input/wrb_landform/wrb_ldf.tif")
