rm(list=ls())
library(raster)
library(sf)
library(Orcs)


mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF_LAI/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")
vispath <- paste0(mainpath,"/visualizations")
predpath <- paste0(datapath,"/predictions")

model_random <- get(load(paste0(modelpath,"/LAI_model_random.RData")))
model_LLO <- get(load(paste0(modelpath,"/LAI_model_LLO.RData")))
ffs_spatial <- get(load(paste0(modelpath,"/LAI_ffsmodel_LLO_final.RData")))
ffs_random <- get(load(paste0(modelpath,"/LAI_ffsmodel_random_final.RData")))
rfe_spatial <- get(load(paste0(modelpath,"/LAI_rfemodel_final.RData")))
predictors <- stack(paste0(rasterpath,"/LAI_predictors.grd"))


prediction_random <- predict(predictors,model_random)
prediction_LLO <- predict(predictors,model_LLO)
prediction_ffs_sp <- predict(predictors,ffs_spatial$LLO_final)
prediction_ffs_rnd <- predict(predictors,ffs_random)
prediction_rfe <- predict(predictors,rfe_spatial)


predictions <- stack(prediction_random,prediction_LLO,
                     prediction_ffs_sp,prediction_ffs_rnd,
                     prediction_rfe)

names(predictions) <- c("noSelection_random","noSelection_LLO",
                        "SpatialSelection","RandomSelection",
                        "RFE")

#forestmask <- raster(paste0(rasterpath,"/Tree_type_raster.tif"))
#forestmask <- reclassify(forestmask,c(-999,9999,1))
#forestmask <- resample(forestmask,predictions)
#predictions <- crop(predictions,forestmask)
#predictions <- mask(predictions,forestmask)
#predictions <- crop(predictions,c(475915.9,478539.2,5630772,5632976))

writeRaster(predictions,paste0(predpath,"/LAI_predictions.grd"),overwrite=TRUE)


