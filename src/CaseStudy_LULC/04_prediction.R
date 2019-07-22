rm(list=ls())
library(raster)
library(sf)
library(Orcs)


mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")
vispath <- paste0(mainpath,"/visualizations")
predpath <- paste0(datapath,"/predictions")

model_random <- get(load(paste0(modelpath,"/model_random.RData")))
model_LLO <- get(load(paste0(modelpath,"/model_LLO.RData")))
ffs_spatial <- get(load(paste0(modelpath,"/ffsmodel_LLO_final.RData")))
ffs_random <- get(load(paste0(modelpath,"/ffsmodel_random_final.RData")))
rfe_spatial <- get(load(paste0(modelpath,"/rfemodel_LLO.RData")))
predictors <- stack(paste0(rasterpath,"/predictors.grd"))
prediction_random <- predict(predictors,model_random)
prediction_LLO <- predict(predictors,model_LLO)
prediction_ffs_sp <- predict(predictors,ffs_spatial)
prediction_ffs_rnd <- predict(predictors,ffs_random)
prediction_rfe <- predict(predictors,rfe_spatial)


predictions <- stack(prediction_random,prediction_LLO,
                     prediction_ffs_sp,prediction_ffs_rnd,
                     prediction_rfe)

names(predictions) <- c("noSelection_random","noSelection_LLO",
                        "SpatialSelection","RandomSelection",
                        "RFE")

writeRaster(predictions,paste0(predpath,"/predictions.grd"),overwrite=TRUE)

predictions_filter <- predictions
for (i in 1:nlayers(predictions)){
  predictions_filter[[i]] <- focal(predictions[[i]],
                                   w=matrix(1,nrow=3,ncol=3), fun=modal)
  levels(predictions_filter[[i]])<-levels(predictions[[1]])
  
}
names(predictions_filter) <- c("noSelection_random","noSelection_LLO",
                               "SpatialSelection","RandomSelection","RFE")

writeRaster(predictions_filter,paste0(predpath,"/predictions_filter.grd"),overwrite=TRUE)

################################################################################
# prediction prob
################################################################################


predprob <- predict(predictors, model_random, type='prob', 
                    index=1:length(unique(model_random$levels)))
names(predprob) <- unique(model_random$levels)
predprob_sp <- predict(predictors, ffs_spatial, type='prob', 
                    index=1:length(unique(ffs_spatial$levels)))
names(predprob_sp) <- unique(ffs_spatial$levels)
spplot(predprob_sp)
