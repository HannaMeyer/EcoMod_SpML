rm(list=ls())
library(raster)
library(sf)

mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")


predictors <- stack(paste0(rasterpath,"/predictors.grd"))
training_sf <- shapefile(paste0(vectorpath,"/trainingSites.shp"))

training_sf$ID <- 1:nrow(training_sf)


################################################################################
#Extract predictors
################################################################################
training_df <- extract(predictors,training_sf,df=TRUE)
training_df <- merge(training_df,training_sf,by.x="ID",by.y="ID")

rename <- data.frame("Type"=c("Grassland","Water","Road","Settlement","Larch",
                              "Spruce","Field","Oak", "Douglas Fir","Beech"),
                     "de"=c("Wiese","Wasser","Strasse","Siedlung","Laerche",
                            "Fichte","Felder","Eiche","Duglasie","Buche"))
names(training_df)[names(training_df)=="Type"] <- "Type_de"
training_df <- merge(training_df,rename,by.x="Type_de",by.y="de")

################################################################################
#Spatial block identification
################################################################################

template <- predictors[[1]]
names(template) <- "spatialBlock"
spatialBlock <- aggregate(template,615)
values(spatialBlock) <- 1:ncell(spatialBlock)
writeRaster(spatialBlock,paste0(rasterpath,"/spatialBlock.grd"),overwrite=TRUE)

spatialBlock_res <- resample(spatialBlock,predictors[[1]],method="ngb")
Spblock_extr <- extract(spatialBlock_res,training_sf,df=TRUE,fun=median)
training_df <- merge(training_df,Spblock_extr,by.x="ID",by.y="ID")


save(training_df,file=paste0(modelpath,"/modeldata_extended.RData"))
