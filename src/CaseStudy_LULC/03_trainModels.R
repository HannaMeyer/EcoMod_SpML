rm(list=ls())
library(caret)
library(CAST)
library(parallel)
library(doParallel)
library(randomForest)
library(raster)

mainpath <- "/mnt/sd19007/users/hmeyer/SpatialCV_MOF/"
#mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")


################################################################################
# Settings
################################################################################
models <- c("random","LLO","FFS_LLO","FFS_random","FFS_LLO_final","FFS_random_final",
            "RFE_LLO")
ncores <- 20
k <- 20 # 20 spatial blocks
subset_p <- 0.75  #proportion of pixels used
seed <- 100

predictors <- c("red","green","blue","vvi","tgi","dem","slope","aspect",
                "lat","lon","ngrdi","gli","pca","pca_3_sd","pca_5_sd","pca_9_sd"#,
                # paste0("dist_",
                #  c("topleft","bottomleft","bottomright","topright","center"))
)
################################################################################
#define response, make subset
################################################################################
modeldata <- get(load(paste0(modelpath,"/modeldata_extended.RData")))

if (subset_p!=1){
  set.seed(seed)
  subset <- createDataPartition(modeldata$ID,p=subset_p,list=FALSE)
  modeldata <- modeldata[subset[,1],]
}

modeldata <- modeldata[complete.cases(modeldata[,which(names(modeldata)%in%predictors)]),]

modeldata$Type <- factor(modeldata$Type)
################################################################################
# define CV and tuning settings
################################################################################
#k <- length(unique(modeldata$spatialBlock))
set.seed(seed)
folds <- CreateSpacetimeFolds(modeldata,spacevar="spatialBlock",k=k)

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
                        modeldata$Type,
                        method="rf",metric="Kappa",
                        importance=TRUE,tuneGrid = tuneGrid,
                        trControl = ctrl_random)
  save(model_random,file=paste0(modelpath,"/model_random.RData"))
  rm(model_random)
  gc()
}
if (any(models=="LLO")){
  set.seed(seed)
  model_LLO <- train(modeldata[,which(names(modeldata)%in%predictors)],
                     modeldata$Type,
                     method="rf",metric="Kappa",
                     importance=TRUE,tuneGrid = tuneGrid,
                     trControl = ctrl_LLO)
  save(model_LLO,file=paste0(modelpath,"/model_LLO.RData"))
  rm(model_LLO)
  gc()
}

if (any(models=="FFS_LLO")){
  set.seed(seed)
  ffsmodel_LLO <- ffs(modeldata[,which(names(modeldata)%in%predictors)],
                      modeldata$Type,
                      method="rf",metric="Kappa",
                      tuneGrid = tuneGrid_ffs,
                      trControl = ctrl_LLO)
  
  save(ffsmodel_LLO,file=paste0(modelpath,"/ffsmodel_LLO.RData"))
  rm(ffsmodel_LLO)
  gc()
}

if (any(models=="FFS_LLO_final")){
  set.seed(seed)
  load(paste0(modelpath,"/ffsmodel_LLO.RData"))
  ffsmodel_LLO$LLO_final <- train(modeldata[,which(names(modeldata)%in%ffsmodel_LLO$selectedvars)],
                                  modeldata$Type,
                                  method="rf",metric="Kappa",
                                  importance=TRUE,tuneGrid = tuneGrid,
                                  trControl = ctrl_LLO)
  ffsmodel_LLO$random_final <- train(modeldata[,which(names(modeldata)%in%ffsmodel_LLO$selectedvars)],
                                     modeldata$Type,
                                     method="rf",metric="Kappa",
                                     importance=TRUE,tuneGrid = tuneGrid,
                                     trControl = ctrl_random)
  save(ffsmodel_LLO,file=paste0(modelpath,"/ffsmodel_LLO_final.RData"))
  rm(ffsmodel_LLO)
  gc()
}

if (any(models=="FFS_random")){
  set.seed(seed)
  ffsmodel_random <- ffs(modeldata[,which(names(modeldata)%in%predictors)],
                         modeldata$Type,
                         method="rf",metric="Kappa",
                         tuneGrid = tuneGrid_ffs,
                         trControl = ctrl_random)
  
  save(ffsmodel_random,file=paste0(modelpath,"/ffsmodel_random.RData"))
  rm(ffsmodel_random)
  gc()
}

if (any(models=="FFS_random_final")){
  set.seed(seed)
  load(paste0(modelpath,"/ffsmodel_random.RData"))
  ffsmodel_random$LLO_final <- train(modeldata[,which(names(modeldata)%in%ffsmodel_random$selectedvars)],
                                     modeldata$Type,
                                     method="rf",metric="Kappa",
                                     importance=TRUE,tuneGrid = tuneGrid,
                                     trControl = ctrl_LLO)
  ffsmodel_random$random_final <- train(modeldata[,which(names(modeldata)%in%ffsmodel_random$selectedvars)],
                                        modeldata$Type,
                                        method="rf",metric="Kappa",
                                        importance=TRUE,tuneGrid = tuneGrid,
                                        trControl = ctrl_random)
  save(ffsmodel_random,file=paste0(modelpath,"/ffsmodel_random_final.RData"))
  rm(ffsmodel_random)
  gc()
}



if (any(models=="RFE_LLO")){
  rtrl_LLO <- rfeControl(functions = caretFuncs,
                         index=ctrl_LLO$index[c(2:20)],
                         indexOut=ctrl_LLO$indexOut[c(2:20)])
  #prob with fold 1 because it is the only one containing larch. NA causes prob here.
  #automatically ignored in train &ffs. manually solved here by removing fold 1.
  
  
  set.seed(seed)
  rfemodel_LLO <- rfe(modeldata[,which(names(modeldata)%in%predictors)],
                      modeldata$Type,
                      sizes = c(2:6,seq(8,length(predictors),2)),
                      method="rf",metric="Kappa",
                      tuneGrid = tuneGrid_ffs,
                      rfeControl = rtrl_LLO,
                      trControl=trainControl(method="none"))
  save(rfemodel_LLO,file=paste0(modelpath,"/rfemodel_LLO.RData"))
  
  rfemodel_LLO$LLO_final <- train(modeldata[,which(names(modeldata)%in%rfemodel_LLO$optVariables)],
                                  modeldata$Type,
                                  method="rf",metric="Kappa",
                                  importance=TRUE,tuneGrid = tuneGrid,
                                  trControl = ctrl_LLO)
  rfemodel_LLO$random_final <- train(modeldata[,which(names(modeldata)%in%rfemodel_LLO$optVariables)],
                                     modeldata$Type,
                                     method="rf",metric="Kappa",
                                     importance=TRUE,tuneGrid = tuneGrid,
                                     trControl = ctrl_random)
  save(rfemodel_LLO,file=paste0(modelpath,"/rfemodel_final.RData"))
  
  
  
}

stopCluster(cl)
warnings()
