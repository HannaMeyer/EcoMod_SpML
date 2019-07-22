# script takes a sentinel stack and LAI polygon as input data.

rm(list=ls())
library(raster)
library(rgdal)
library(rgeos)

mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF_LAI/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")

sen <- list.files(paste0(rasterpath,"/s2_20170510jp2/"),full.names = FALSE)
Lidarprop <- shapefile(paste0(vectorpath,"/LAI_sampling.shp"))
cluster <- shapefile(paste0(vectorpath,"/cluster_centroids_new.shp"))
dem <- raster(paste0(rasterpath,"/geonode_lidar_dem_01m.tif"))
names(dem) <- "dem"

setwd(paste0(rasterpath,"/s2_20170510jp2/"))
m10 <- stack("T32UMB_20170510T103031_B02.jp2",
             "T32UMB_20170510T103031_B03.jp2",
             "T32UMB_20170510T103031_B04.jp2",
             "T32UMB_20170510T103031_B08.jp2")
m10 <- crop(m10,dem)

m20 <- stack("T32UMB_20170510T103031_B05.jp2",
             "T32UMB_20170510T103031_B06.jp2",
             "T32UMB_20170510T103031_B07.jp2",
            "T32UMB_20170510T103031_B11.jp2",
             "T32UMB_20170510T103031_B12.jp2",
             "T32UMB_20170510T103031_B8A.jp2")
m20 <- crop(m20,dem)
m20_10 <- resample(m20,m10)
sen_rst <- stack(m10,m20_10)

dem <- resample(dem,sen_rst)
slope <- terrain(dem,opt="slope")
aspect <- terrain(dem,opt="aspect")

########## Distance based predictors
corners <- extent(dem)
spp <- SpatialPoints(matrix(c(corners[1],corners[4],
                              corners[1],corners[3],
                              corners[2],corners[3],
                              corners[2],corners[4],
                              (corners[2]+corners[1])/2, 
                              (corners[3]+corners[4])/2),ncol=2,byrow=T))
distances <- list()
for (i in 1:5){
  distances[[i]] <- distanceFromPoints(dem, spp[i,]) 
}
distances <- stack(distances)
lat <- distances[[1]]
lon <- distances[[1]]
values(lat)<- coordinates(lat)[,2]
values(lon)<- coordinates(lon)[,1]

names(distances)<- paste0("dist_",c("topleft","bottomleft","bottomright","topright","center"))
distances$lat <- lat
distances$lon <- lon

predictors <- stack(sen_rst,dem,slope,aspect,
                   distances)
writeRaster(predictors,paste0(rasterpath,"/LAI_predictors.grd"),overwrite=TRUE)


########################
#create Response Raster
###########

template <- predictors[[1]]
values(template )<- NA
#lidar_lai_raster <- rasterize( Lidarprop,template, 'LAI',fun=mean) 
lidar_lai_raster <- rasterize( Lidarprop,template, 'LAI',fun=mean) 
writeRaster(lidar_lai_raster,paste0(rasterpath,"/LIDAR_LAI.grd"),
            overwrite=TRUE)
lidar_lai_raster <- stack(lidar_lai_raster,lat,lon)


cluster <- cluster[,-which(names(cluster)=="id")]
cluster_buffer <-  gBuffer(cluster,width=60,byid = TRUE) 

lai_extractes <- extract(lidar_lai_raster,cluster_buffer,df=TRUE)
names(lai_extractes)<-c("Cluster","LAI","lat","lon")
lai_extractes <- lai_extractes[complete.cases(lai_extractes),]

lai_extractes_sp <- SpatialPointsDataFrame(coords=data.frame("x"=lai_extractes$lon,"y"=lai_extractes$lat),
                       data=lai_extractes,proj4string = crs(cluster_buffer))
lai_extractes_sp <- lai_extractes_sp[,-which(names(lai_extractes_sp)%in%c("lat","lon"))]
writeOGR(lai_extractes_sp,paste0(vectorpath,"/trainingSites.shp"),
         driver="ESRI Shapefile",layer="cluster_buffer")
