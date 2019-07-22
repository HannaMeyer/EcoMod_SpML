rm(list=ls())
library(raster)
library(Orcs)
library(latticeExtra)

mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")
vispath <- paste0(mainpath,"/visualizations")



aerial <- stack(paste0(rasterpath,"/geonode_ortho_muf_1m.tif"))
training_sf <- shapefile(paste0(vectorpath,"/lcc_training_areas_20180126.shp"))
training_sf <- spTransform(training_sf,projection(aerial))
names(aerial) <- c("blue","green","red")


rename <- data.frame("Type_en"=c("Grassland","Water","Road","Settlement","Larch",
                                 "Spruce","Field","Oak", "Douglas fir","Beech"),
                     "de"=c("Wiese","Wasser","Strasse","Siedlung","Laerche",
                            "Fichte","Felder","Eiche","Duglasie","Buche"))

training_sf<- merge(training_sf,rename,by.x="Type",by.y="de")

cols <-data.frame("Type_en"=c("Beech","Douglas Fir","Field","Grassland","Larch",      
                                         "Oak","Road","Settlement","Spruce","Water"),
                             "col"=c("brown4", "pink", "wheat","yellowgreen",
                                     "lightcoral","yellow","grey50","red","purple",
                                     "blue"))


training_sf<- merge(training_sf,cols,by.x="Type_en",by.y="Type_en")
training_sf$Type_en <- factor(training_sf$Type_en)
lookupTable <- unique(cols)
colRegions <- as.vector(lookupTable$col[match(levels(training_sf$Type_en),
                                              lookupTable$Type_en)])

folds <- raster(paste0(rasterpath,"/spatialBlock.grd"))
folds_sp <- rasterToPolygons(folds)

lout <- rgb2spLayout(aerial)

png(paste0(vispath,"/map.png"), width=15,height=10,units="cm",res = 600,type="cairo")
spplot(training_sf, zcol = "Type_en", col.regions = colRegions,
       scales=list(draw=TRUE),
       sp.layout = lout)+
  as.layer(spplot(folds_sp,col.regions="transparent",col="yellow"))
dev.off()                           

