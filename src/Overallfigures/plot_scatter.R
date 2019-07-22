rm(list=ls())
library(lattice)

mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF_LAI/"
datapath <- paste0(mainpath,"/data/")
modelpath <- paste0(datapath,"/modeldat/")
vispath <- paste0(mainpath,"/visualizations/")

#cols <- rainbow(11)
cols <- c("black","green","blue","red","grey30","darkblue","darkgreen","darkred","grey70","orange","brown")

load(paste0(modelpath,"/LAI_ffsmodel_LLO_final.RData"))
load(paste0(modelpath,"/LAI_model_random.RData"))
load(paste0(modelpath,"/LAI_model_LLO.RData"))


model_random <- model_random$pred[model_random$pred$mtry==model_random$bestTune$mtry,]
model_LLO <- model_LLO$pred[model_LLO$pred$mtry==model_LLO$bestTune$mtry,]
model_LLO_ffs <- LAI_ffsmodel_LLO$LLO_final$pred[LAI_ffsmodel_LLO$LLO_final$pred$mtry==LAI_ffsmodel_LLO$LLO_final$bestTune$mtry,]

dat <- data.frame("observed"=c(model_random$obs,model_LLO$obs,model_LLO_ffs$obs),
                  "predicted"=c(model_random$pred,model_LLO$pred,model_LLO_ffs$pred),
                  "type"=c(rep("random",length(model_random$obs)),
                             rep("spatial",length(model_LLO$obs)),
                           rep("FFS_spatial",length(model_LLO_ffs$obs))),
                  "fold"=c(as.numeric(substr(model_random$Resample,5,6)),
                           as.numeric(substr(model_LLO$Resample,9,10)),
                  as.numeric(substr(model_LLO_ffs$Resample,9,10))))
dat$col <- cols[dat$fold]
dat <- dat[dat$type%in%c("random","spatial"),]

pdf(paste0(vispath,"/validationScatter_V2.pdf"),width=6,height=4)
trellis.par.set(strip.background=list(col="grey80"))
xyplot(observed~predicted|type, dat,col=dat$col,
       scales=list(y=list(at=round(seq(min(dat$observed),max(dat$observed),4))),
                   x=list(at=round(seq(min(dat$observed),max(dat$observed),4)))))
dev.off()

