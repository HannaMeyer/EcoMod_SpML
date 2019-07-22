rm(list=ls())
library(raster)
library(sf)
library(caret)
library(randomForest)
library(Rsenal)
library(CAST)


mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")
vispath <- paste0(mainpath,"/visualizations")

model_random <- get(load(paste0(modelpath,"/model_random.RData")))
model_LLO <- get(load(paste0(modelpath,"/model_LLO.RData")))
model_ffs_LLO <- get(load(paste0(modelpath,"/ffsmodel_LLO_final.RData")))
model_ffs_random <- get(load(paste0(modelpath,"/ffsmodel_random_final.RData")))
model_rfe <- get(load(paste0(modelpath,"/rfemodel_final.RData")))


modeldata <- get(load(paste0(modelpath,"/modeldata_extended.RData")))


################################################################################
#Plot Varimp
################################################################################
varimps <- varImp(model_random)

cairo_pdf(paste0(vispath,"/varimp.pdf"), width=8,height=9)
plot(varimps,col="black",top=ncol(model_random$trainingData)-1,
     par.settings =list(strip.background=list(col="grey")))
dev.off()

cairo_pdf(paste0(vispath,"/varimp_full.pdf"), width=6,height=5)
varImpPlot(model_random$finalModel,col="black",main="",scale=T,type=1)
dev.off()

plot_ffs(ffsmodel_LLO)
ffsmodel_LLO$selectedvars
ffsmodel_LLO$selectedvars_perf
varperf <-max(ffsmodel_LLO$selectedvars_perf)-ffsmodel_LLO$selectedvars_perf
#varperf <- scale(varperf,0,100,center=FALSE)
varperf <- rescale(varperf, to = c(0, 100))
barplot(varperf,
        horiz=T)
################################################################################
#Compare
################################################################################
kia_LLO <- model_LLO$resample$Kappa
boxplot(kia_random,kia_LLO)



##############################valid global
ffs_sp_sp <- ffsmodel_LLO$LLO_final$pred[
  ffsmodel_LLO$LLO_final$pred$mtry==ffsmodel_LLO$LLO_final$bestTune$mtry,]

ffs_sp_random <- ffsmodel_LLO$random_final$pred[
  ffsmodel_LLO$random_final$pred$mtry==ffsmodel_LLO$random_final$bestTune$mtry,]

ffs_random_random <- model_ffs_random$random_final$pred[
  model_ffs_random$random_final$pred$mtry==model_ffs_random$random_final$bestTune$mtry,]

ffs_random_sp <- ffsmodel_random$LLO_final$pred[
  ffsmodel_random$LLO_final$pred$mtry==ffsmodel_random$LLO_final$bestTune$mtry,]


rfe_spatial <- model_rfe$LLO_final$pred[
  model_rfe$LLO_final$pred$mtry==model_rfe$LLO_final$bestTune$mtry,]

rfe_random <- model_rfe$random_final$pred[
  model_rfe$random_final$pred$mtry==model_rfe$random_final$bestTune$mtry,]


full_spatial <- model_LLO$pred[
  model_LLO$pred$mtry==model_LLO$bestTune$mtry,]

full_random <- model_random$pred[
  model_random$pred$mtry==model_random$bestTune$mtry,]

####

ffstable_sp_sp <- table(ffs_sp_sp$pred,ffs_sp_sp$obs)
sum(diag(ffstable_sp_sp))/sum(ffstable_sp_sp)
kia_ffs_sp_sp <-kstat(ffs_sp_sp$pred,ffs_sp_sp$obs)
kia_ffs_sp_sp

ffstable_random_sp <- table(ffs_random_sp$pred,ffs_random_sp$obs)
sum(diag(ffstable_random_sp))/sum(ffstable_random_sp)
kia_ffs_random_sp <-kstat(ffs_random_sp$pred,ffs_random_sp$obs)
kia_ffs_random_sp


ffstable_sp_random <- table(ffs_sp_random$pred,ffs_sp_random$obs)
sum(diag(ffstable_sp_random))/sum(ffstable_sp_random)
kia_ffs_random_sp <-kstat(ffs_sp_random$pred,ffs_sp_random$obs)

ffstable_random_random <- table(ffs_random_random$pred,ffs_random_random$obs)
sum(diag(ffstable_random_random))/sum(ffstable_random_random)
kia_ffs_random_sp <-kstat(ffs_random_random$pred,ffs_random_random$obs)


rfetable <- table(rfe_spatial$pred,rfe_spatial$obs)
sum(diag(rfetable))/sum(rfetable)
kia_rfe <-kstat(rfe_spatial$pred,rfe_spatial$obs)

rfetable_random <- table(rfe_random$pred,rfe_random$obs)
sum(diag(rfetable_random))/sum(rfetable_random)
kia_rfe <-kstat(rfe_random$pred,rfe_random$obs)



full_spatial_table <- table(full_spatial$pred,full_spatial$obs)
sum(diag(full_spatial_table))/sum(full_spatial_table)
kia_full <-kstat(full_spatial$pred,full_spatial$obs)

full_random_table <- table(full_random$pred,full_random$obs)
sum(diag(full_random_table))/sum(full_random_table)
kia_full <-kstat(full_random$pred,full_random$obs)
