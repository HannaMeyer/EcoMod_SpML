rm(list=ls())
library(raster)
library(sf)
library(Orcs)
library(viridis)


mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF_LAI/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")
vispath <- paste0(mainpath,"/visualizations")
predpath <- paste0(datapath,"/predictions")

predictions <- stack(paste0(predpath,"/LAI_predictions.grd"))
model_random <- get(load(paste0(modelpath,"/LAI_model_random.RData")))

predictions <-predictions[[which(names(predictions)%in%c(
  "noSelection_random","SpatialSelection","RandomSelection","RFE"))]]
names(predictions)[which(names(predictions)=="noSelection_random")]<- "noSelection"
#reorder:
predictions <- stack(predictions$noSelection,
                            predictions$RandomSelection,
                            predictions$SpatialSelection)

names(predictions) <- c("A","B","C")


predictors <- stack(paste0(rasterpath,"/LAI_predictors.grd"))



rgb_sp <- stack(predictors$T32UMB_20170510T103031_B02,
                predictors$T32UMB_20170510T103031_B03,
                predictors$T32UMB_20170510T103031_B04)
rgb_sp <- crop(rgb_sp,c(475000,479000,5630000,5633500))

#forestmask <- stack(paste0(rasterpath,"/predictions_filter.grd"))
#forestmask <- forestmask$SpatialSelection
#forestmask <- reclassify(forestmask,matrix(c(2.9,4,NA,
#                                             6.9,8,NA,
#                                             9.9,10,NA),ncol=3,byrow = TRUE))
#forestmask <- focal(forestmask,w=matrix(1,nrow=5,ncol=5), fun=modal)
#forestmask <- crop(forestmask,rgb_sp)
#forestmask <- resample(forestmask,rgb_sp)
#rgb_sp <- mask(rgb_sp,forestmask)
predictions <- crop (predictions,rgb_sp)
#predictions <- mask(predictions,forestmask)

rgb <- rgb2spLayout(rgb_sp, quantiles = c(0.02, 0.98), alpha = 1)

template <- predictions[[1]]
p1 <- spplot(template,maxpixels=9999999,
             col.regions=viridis(500),
             scales=list(draw=FALSE),
             at=seq(min(values(predictions),na.rm=T),
                    max(values(predictions),na.rm=T),
                    0.1),
             par.settings = list(strip.background=list(col="lightgrey")),
             colorkey=TRUE,sp.layout =list(rgb),
             names.attr=c("RGB"))+
  spplot(template,  col.regions="transparent",sp.layout =rgb)

p_fig <- spplot(predictions,maxpixels=9999999,
                scales=list(draw=FALSE),
                at=seq(min(values(predictions),na.rm=T),
                       max(values(predictions),na.rm=T),
                       0.1),
                par.settings = list(strip.background=list(col="lightgrey")),
                col.regions=viridis(500),
                names.attr=c("no selection", "random selection",
                             "spatial selection"))

P_cbn <- latticeCombineGrid(list(p1,p_fig),layout=c(2,2))
P_cbn$condlevels[[1]][1] <- "RGB"
png(paste0(vispath,"/LAI_predictions.png"), 
    width=18,height=14, units = "cm",res=500,type="cairo") 
print(P_cbn) 
dev.off()

