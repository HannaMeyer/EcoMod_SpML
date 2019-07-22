rm(list=ls())
library(raster)
library(sf)
library(Orcs)
library(latticeExtra)
library(viridis)

mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF_LAI/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")
vispath <- paste0(mainpath,"/visualizations")


predictors <- stack(paste0(rasterpath,"/LAI_predictors.grd"))
training_sf <- shapefile(paste0(vectorpath,"/trainingSites.shp"))

rgbbands <- predictors[[1:3]]
ext <- c(475000,479000,5630000,5633500)

#aerial for same extent as case study I
aerial <- stack("/home/hanna/Documents/Projects/SpatialCV/MOF/data/raster/geonode_ortho_muf_1m.tif")

lout <- rgb2spLayout(rgbbands)


png(paste0(vispath,"/map_LAI.png"), width=15,height=10,units="cm",res = 600,type="cairo")
spplot(training_sf, zcol = "LAI", col.regions=viridis(100),
       scales=list(draw=TRUE),xlim=extent(aerial)[1:2],ylim=extent(aerial)[3:4],
       sp.layout = lout,pch=20,colorkey=TRUE)
dev.off()                           

##### Detail


spplot(training_sf, zcol = "LAI", col.regions=viridis(100),
       xlim=c(476550,476695),ylim=c(5631855,5632000),
       scales=list(draw=TRUE),
       sp.layout = lout,pch=20,colorkey=TRUE)
png(paste0(vispath,"/map_LAI_detail.png"), width=15,height=10,units="cm",res = 600,type="cairo")
spplot(training_sf, zcol = "LAI", col.regions=viridis(100),
       xlim=c(476550,478000),ylim=c(5631500,5632650),
       scales=list(draw=TRUE),
       sp.layout = lout,pch=20,colorkey=TRUE)
dev.off()

