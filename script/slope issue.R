library(terra)
library(sf)

# Slope classes

# The formula to convert slope degrees to slope percentage is Tan(θ)*100.
# 

# # slope
# Flat	Slope < 0.2%
# Level	0.2% < Slope < 1%
# Gently	1% < Slope < 5%
# Sloping	5% < Slope < 15%
# Steep	Slope > 15%

r<-rast("C:/Users/hounkpk1/OneDrive - Aalto University/Thesis_data/misc/slope.tif")

# convert to spatial data
df$slope<-NULL
sel_dat <- df %>% drop_na(x,y)
sp_dat <- sel_dat %>% 
  drop_na(y)%>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

comb_sp<-terra::extract(r, sp_dat,na.rm = TRUE,bind=T, ID=F)
df<-as.data.frame(comb_sp)

head(df)

slope_mat <- c(0, 0.20, 1,
             0.20, 1.00,2,
             1.00, 5.00,3,
             5.00, 15.00,4,
             15.00, 80,5)

slope_rclmat  <- matrix(slope_mat, ncol=3, byrow=TRUE)

df<- df %>% dplyr::mutate(slope_class = case_when(
    (slope >= slope_rclmat [1,1] & slope < slope_rclmat [1,2])~slope_rclmat [1,3],
    (slope >= slope_rclmat [2,1] & slope < slope_rclmat [2,2])~slope_rclmat [2,3],
    (slope >= slope_rclmat [3,1] & slope < slope_rclmat [3,2])~slope_rclmat [3,3],
    (slope >= slope_rclmat [4,1] & slope < slope_rclmat [4,2])~slope_rclmat [4,3],
    (slope >= slope_rclmat [5,1] & slope < slope_rclmat [5,2])~slope_rclmat [5,3],
    ))

summary(as.factor(df$slope_class))

# save the data and plots
write_xlsx(df,"./Next_Article/input/df_slope.xlsx")


