rm(list=ls())
library(raster)
library(sf)
library(caret)
library(randomForest)
library(Rsenal)
library(CAST)


mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF_LAI/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")
vispath <- paste0(mainpath,"/visualizations")

model_random <- get(load(paste0(modelpath,"/LAI_model_random.RData")))
model_LLO <- get(load(paste0(modelpath,"/LAI_model_LLO.RData")))
model_ffs_LLO <- get(load(paste0(modelpath,"/LAI_ffsmodel_LLO_final.RData")))
model_ffs_random <- get(load(paste0(modelpath,"/LAI_ffsmodel_random_final.RData")))
model_rfe <- get(load(paste0(modelpath,"/LAI_rfemodel_final.RData")))

model_random
model_LLO
model_ffs_LLO$random_final
model_ffs_LLO$LLO_final
model_rfe$LLO_final
model_rfe$random_final
model_ffs_random$LLO_final
model_ffs_random$random_final


##############################valid global
ffs_sp_sp <- model_ffs_LLO$LLO_final$pred[
  model_ffs_LLO$LLO_final$pred$mtry==model_ffs_LLO$LLO_final$bestTune$mtry,]

ffs_sp_random <- model_ffs_LLO$random_final$pred[
  model_ffs_LLO$random_final$pred$mtry==model_ffs_LLO$random_final$bestTune$mtry,]

ffs_random_random <- model_ffs_random$random_final$pred[
  model_ffs_random$random_final$pred$mtry==model_ffs_random$random_final$bestTune$mtry,]

ffs_random_sp <- model_ffs_random$LLO_final$pred[
  model_ffs_random$LLO_final$pred$mtry==model_ffs_random$LLO_final$bestTune$mtry,]


rfe_spatial <- model_rfe$LLO_final$pred[
  model_rfe$LLO_final$pred$mtry==model_rfe$LLO_final$bestTune$mtry,]

rfe_random <- model_rfe$random_final$pred[
  model_rfe$random_final$pred$mtry==model_rfe$random_final$bestTune$mtry,]


full_spatial <- model_LLO$pred[
  model_LLO$pred$mtry==model_LLO$bestTune$mtry,]

full_random <- model_random$pred[
  model_random$pred$mtry==model_random$bestTune$mtry,]

####
regressionStats(ffs_sp_sp$pred,ffs_sp_sp$obs)

regressionStats(ffs_random_sp$pred,ffs_random_sp$obs)

regressionStats(ffs_sp_random$pred,ffs_sp_random$obs)

regressionStats(ffs_random_random$pred,ffs_random_random$obs)

regressionStats(rfe_spatial$pred,rfe_spatial$obs)

regressionStats(rfe_random$pred,rfe_random$obs)

regressionStats(full_spatial$pred,full_spatial$obs)

regressionStats(full_random$pred,full_random$obs)

