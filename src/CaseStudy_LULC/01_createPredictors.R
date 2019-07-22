rm(list=ls())
library(raster)
mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")

aerial <- stack(paste0(rasterpath,"/geonode_ortho_muf_1m.tif"))
names(aerial) <- c("red","green","blue")  #Achtung! red/blue vertauscht!!
dem <- raster(paste0(rasterpath,"/geonode_lidar_dem_01m.tif"))
names(dem) <- "dem"
vvi <- raster(paste0(rasterpath,"/geonode_ortho_muf_vvi.tif"))
names(vvi) <- "vvi"
tgi <- raster(paste0(rasterpath,"/geonode_ortho_muf_tgi.tif"))
names(tgi) <- "tgi"
gli <- raster(paste0(rasterpath,"/geonode_ortho_muf_gli.tif"))
names(gli) <- "gli"
ngrdi <- raster(paste0(rasterpath,"/geonode_ortho_muf_ngrdi.tif"))
names(ngrdi) <- "ngrdi"
pca <- stack(paste0(rasterpath,"/geonode_ortho_muf_rgb_idx_pca_scaled.tif"))[[1]]
names(pca) <- "pca"
pca_3_sd <- focal(pca, w=matrix(1,nrow=3,ncol=3), fun=sd)
names(pca_3_sd) <- "pca_3_sd"
pca_5_sd <- focal(pca, w=matrix(1,nrow=5,ncol=5), fun=sd)
names(pca_5_sd) <- "pca_5_sd"
pca_9_sd <- focal(pca, w=matrix(1,nrow=9,ncol=9), fun=sd)
names(pca_9_sd) <- "pca_9_sd"

########## Terrain based predictors
dem <- resample(dem,aerial)
slope <- terrain(dem,opt="slope")
aspect <- terrain(dem,opt="aspect")

########## Distance based predictors
corners <- extent(aerial)
spp <- SpatialPoints(matrix(c(corners[1],corners[4],
                   corners[1],corners[3],
                   corners[2],corners[3],
                   corners[2],corners[4],
                   (corners[2]+corners[1])/2, 
                   (corners[3]+corners[4])/2),ncol=2,byrow=T))
distances <- list()
for (i in 1:5){
distances[[i]] <- distanceFromPoints(aerial[[1]], spp[i,]) 
}
distances <- stack(distances)
lat <- distances[[1]]
lon <- distances[[1]]
values(lat)<- coordinates(lat)[,2]
values(lon)<- coordinates(lon)[,1]

names(distances)<- paste0("dist_",c("topleft","bottomleft","bottomright","topright","center"))
distances$lat <- lat
distances$lon <- lon

########## Merge predictors

predictors <- stack(aerial,vvi,tgi,distances,dem,slope,aspect,
                    ngrdi,gli,pca,pca_3_sd,pca_5_sd,pca_9_sd)
writeRaster(predictors,paste0(rasterpath,"/predictors.grd"),overwrite=TRUE)
