rm(list=ls())
library(caret)
library(CAST)
library(parallel)
library(doParallel)
library(randomForest)
library(raster)

#mainpath <- "/mnt/sd19007/users/hmeyer/SpatialCV_MOF_LAI/"
mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF_LAI/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")


################################################################################
# Settings
################################################################################
models <- c("random","LLO","FFS_LLO","FFS_random","FFS_LLO_final","FFS_random_final",
            "RFE_LLO")
ncores <- 3
seed <- 100
k <- "all"
#es klappt k=5, width=40

predictors <- c("T32UMB_20170510T103031_B02","T32UMB_20170510T103031_B03",
                "T32UMB_20170510T103031_B04","T32UMB_20170510T103031_B08",
                "T32UMB_20170510T103031_B05","T32UMB_20170510T103031_B06",
                "T32UMB_20170510T103031_B07","T32UMB_20170510T103031_B11",
                "T32UMB_20170510T103031_B12","T32UMB_20170510T103031_B8A",
                "dem","slope","aspect","lat","lon")#,
                #"dist_topleft","dist_bottomleft","dist_bottomright",
                #"dist_center")

################################################################################
#define response, make subset
################################################################################
modeldata <- get(load(paste0(modelpath,"/modeldata.RData")))
modeldata <- modeldata[complete.cases(modeldata[,which(names(modeldata)%in%predictors)]),]

if (k=="all"){
k <- length(unique(modeldata$Cluster))
}
################################################################################
# define CV and tuning settings
################################################################################
#k <- length(unique(modeldata$spatialBlock))
set.seed(seed)
folds <- CreateSpacetimeFolds(modeldata,spacevar="Cluster",k=k)
set.seed(seed)
ctrl_random <- trainControl(method="cv",savePredictions = TRUE,returnResamp = "final",
                            number=k)
ctrl_LLO <- trainControl(method="cv",savePredictions = TRUE,returnResamp = "final",
                         index=folds$index,indexOut=folds$indexOut)
tuneGrid_ffs <- expand.grid(mtry = 2)
tuneGrid <- expand.grid(mtry = c(2,3,4,seq(6,length(predictors),2)))
################################################################################
#Train models
################################################################################
cl <- makeCluster(ncores)
registerDoParallel(cl)
if (any(models=="random")){
  set.seed(seed)
  model_random <- train(modeldata[,which(names(modeldata)%in%predictors)],
                        modeldata$LAI,
                        method="rf",metric="RMSE",
                        importance=TRUE,tuneGrid = tuneGrid,
                        trControl = ctrl_random)
  save(model_random,file=paste0(modelpath,"/LAI_model_random.RData"))
  rm(model_random)
  gc()
}
if (any(models=="LLO")){
  set.seed(seed)
  model_LLO <- train(modeldata[,which(names(modeldata)%in%predictors)],
                     modeldata$LAI,
                     method="rf",metric="RMSE",
                     importance=TRUE,tuneGrid = tuneGrid,
                     trControl = ctrl_LLO)
  save(model_LLO,file=paste0(modelpath,"/LAI_model_LLO.RData"))
  rm(model_LLO)
  gc()
}

if (any(models=="FFS_LLO")){
  set.seed(seed)
  ffsmodel_LLO <- ffs(modeldata[,which(names(modeldata)%in%predictors)],
                      modeldata$LAI,
                      method="rf",metric="RMSE",
                      tuneGrid = tuneGrid_ffs,
                      trControl = ctrl_LLO)
  
  save(ffsmodel_LLO,file=paste0(modelpath,"/LAI_ffsmodel_LLO.RData"))
  rm(ffsmodel_LLO)
  gc()
}

if (any(models=="FFS_LLO_final")){
  set.seed(seed)
  LAI_ffsmodel_LLO <- get(load(paste0(modelpath,"/LAI_ffsmodel_LLO.RData")))
  LAI_ffsmodel_LLO$LLO_final <- train(modeldata[,which(names(modeldata)%in%ffsmodel_LLO$selectedvars)],
                                  modeldata$LAI,
                                  method="rf",metric="RMSE",
                                  importance=TRUE,tuneGrid = tuneGrid,
                                  trControl = ctrl_LLO)
  LAI_ffsmodel_LLO$random_final <- train(modeldata[,which(names(modeldata)%in%ffsmodel_LLO$selectedvars)],
                                     modeldata$LAI,
                                     method="rf",metric="RMSE",
                                     importance=TRUE,tuneGrid = tuneGrid,
                                     trControl = ctrl_random)
  save(LAI_ffsmodel_LLO,file=paste0(modelpath,"/LAI_ffsmodel_LLO_final.RData"))
  rm(ffsmodel_LLO)
  gc()
}

if (any(models=="FFS_random")){
  set.seed(seed)
  ffsmodel_random <- ffs(modeldata[,which(names(modeldata)%in%predictors)],
                         modeldata$LAI,
                         method="rf",metric="RMSE",
                         tuneGrid = tuneGrid_ffs,
                         trControl = ctrl_random)
  
  save(ffsmodel_random,file=paste0(modelpath,"/LAI_ffsmodel_random.RData"))
  rm(ffsmodel_random)
  gc()
}

if (any(models=="FFS_random_final")){
  set.seed(seed)
  LAI_ffsmodel_random <- get(load(paste0(modelpath,"/LAI_ffsmodel_random.RData")))
  LAI_ffsmodel_random$LLO_final <- train(modeldata[,which(names(modeldata)%in%ffsmodel_random$selectedvars)],
                                     modeldata$LAI,
                                     method="rf",metric="RMSE",
                                     importance=TRUE,
                                     tuneGrid = tuneGrid,
                                     trControl = ctrl_LLO)
  LAI_ffsmodel_random$random_final <- train(modeldata[,which(names(modeldata)%in%ffsmodel_random$selectedvars)],
                                        modeldata$LAI,
                                        method="rf",metric="RMSE",
                                        importance=TRUE,tuneGrid = tuneGrid,
                                        trControl = ctrl_random)
  save(LAI_ffsmodel_random,file=paste0(modelpath,"/LAI_ffsmodel_random_final.RData"))
  rm(ffsmodel_random)
  gc()
}



if (any(models=="RFE_LLO")){
  rtrl_LLO <- rfeControl(functions = rfFuncs,
                         index=ctrl_LLO$index,
                         indexOut=ctrl_LLO$indexOut)

  set.seed(seed)
  rfemodel_LLO <- rfe(modeldata[,which(names(modeldata)%in%predictors)],
                      modeldata$LAI,
                      sizes = c(2:6,seq(8,length(predictors),2)),
                      method="rf",metric="RMSE",
                      tuneGrid = tuneGrid_ffs,
                      rfeControl = rtrl_LLO,
                      trControl=trainControl(method="none"))
  save(rfemodel_LLO,file=paste0(modelpath,"/LAI_rfemodel_LLO.RData"))
  
  rfemodel_LLO$LLO_final <- train(modeldata[,which(names(modeldata)%in%rfemodel_LLO$optVariables)],
                                  modeldata$LAI,
                                  method="rf",metric="RMSE",
                                  importance=TRUE,tuneGrid = tuneGrid,
                                  trControl = ctrl_LLO)
  rfemodel_LLO$random_final <- train(modeldata[,which(names(modeldata)%in%rfemodel_LLO$optVariables)],
                                     modeldata$LAI,
                                     method="rf",metric="RMSE",
                                     importance=TRUE,tuneGrid = tuneGrid,
                                     trControl = ctrl_random)
  save(rfemodel_LLO,file=paste0(modelpath,"/LAI_rfemodel_final.RData"))
  
  
  
}

stopCluster(cl)
warnings()
