# # all values = 0 and <= 0.25 become 1, etc.
# m <- c(-1, 0, 0,
#       0, 0.3, 1,
#       0.3, 0.5,2,
#        0.5,1,3,
#        1,2.5,4)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# r_cropland<- classify(es, rclmat, include.lowest=F,right=T)
# r_cropland<-as.factor(r_cropland)

# plot(es_ir[[1]])

# negative 
# 0 - 0.3 small effect
# 0.3 - 0.5 medium effect
# 0.5 - 1 large effect
# 1 - inf huge effect
# 
# tm_shape(r_cropland$ES_mega)+
#   tm_raster(style="cat",labels = c("negative", "small", "medium", "large", "huge"),
#             palette = c("brown1", "deepskyblue", "darkorange", "darkolivegreen", "darkolivegreen1"),# "darkseagreen1", "goldenrod3", "darkorange", "darkorchid1", "darkorchid4"),
#             title="Land Cover")+
#   tm_layout(legend.outside = TRUE)


