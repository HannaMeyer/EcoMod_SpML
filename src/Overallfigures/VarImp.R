rm(list=ls())
library(caret)
library(randomForest)
library(CAST)


mainpath <- "/home/hanna/Documents/Projects/SpatialCV/"

model_LAI <- get(load(paste0(mainpath,"/MOF_LAI/data/modeldat/LAI_model_random.RData")))
model_LUC <- get(load(paste0(mainpath,"/MOF/data/modeldat/model_random.RData")))

row.names(model_LAI$finalModel$importance) <- gsub("T32UMB_20170510T103031_","",row.names(model_LAI$finalModel$importance))

pdf("/home/hanna/Documents/Projects/SpatialCV/MOF_LAI/visualizations/varImp.pdf",
    width=7,height=5)
par(mfrow=c(1,2))
p_LUC <- varImpPlot(model_LUC$finalModel,type=1,scale=TRUE,main="")
mtext("a", side = 3, line = 0, adj = 0 )
p_LAI <- varImpPlot(model_LAI$finalModel,type=1,scale=TRUE,main="")
mtext("b", side = 3, line = 0, adj = 0 )
dev.off()
######################################################################
# Varimps ffs
######################################################################
model_LAI <- get(load(paste0(mainpath,"/MOF_LAI/data/modeldat/LAI_ffsmodel_LLO_final.RData")))
model_LUC <- get(load(paste0(mainpath,"/MOF/data/modeldat/ffsmodel_LLO_final.RData")))

model_LAI$selectedvars <- gsub("T32UMB_20170510T103031_","",model_LAI$selectedvars)

pdf("/home/hanna/Documents/Projects/SpatialCV/MOF_LAI/visualizations/varImp_ffs.pdf",
    width=7,height=5)
par(mfrow=c(1,2),mar=c(6,4,2,2))
plot_ffs(model_LUC,plotType="selected",type="b")
mtext("a", side = 3, line = 0, adj = 0 )
plot_ffs(model_LAI,plotType="selected",type="b")
mtext("b", side = 3, line = 0, adj = 0 )
dev.off()
