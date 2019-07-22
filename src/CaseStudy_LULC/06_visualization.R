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

predictions_filter <- stack(paste0(predpath,"/predictions_filter.grd"))
model_random <- get(load(paste0(modelpath,"/model_random.RData")))

predictions_filter <-predictions_filter[[which(names(predictions_filter)%in%c(
  "noSelection_random","SpatialSelection","RandomSelection","RFE"))]]
names(predictions_filter)[which(names(predictions_filter)=="noSelection_random")]<- "noSelection"
#reorder:
predictions_filter <- stack(predictions_filter$noSelection,
                            predictions_filter$RandomSelection,
                            predictions_filter$SpatialSelection)

for (i in 1:nlayers(predictions_filter)){
  predictions_filter[[i]] <- ratify(predictions_filter[[i]])
  levels( predictions_filter[[i]]) <- data.frame(ID=1:length(model_random$levels),
                                           class=model_random$levels)
}
names(predictions_filter) <- c("A","B","C")


predictors <- stack(paste0(rasterpath,"/predictors.grd"))
#cols_df <- data.frame("Type_en"=c("Beech","Douglas Fir","Field","Grassland","Larch",      
#                                  "Oak","Road","Settlement","Spruce","Water"),
#                      "col"=c("forestgreen", "darkolivegreen", "wheat","yellowgreen",
#                              "yellow3","darkgreen","grey50","tomato4","darkolivegreen4",
#                              "blue"))

cols_df <- data.frame("Type_en"=c("Beech","Douglas Fir","Field","Grassland","Larch",      
                                  "Oak","Road","Settlement","Spruce","Water"),
                      "col"=c("brown4", "pink", "wheat","yellowgreen",
                              "lightcoral","yellow","grey50","red","purple",
                              "blue"))

rgb_sp <- stack(predictors$blue,predictors$green,predictors$red)
rgb <- rgb2spLayout(rgb_sp, quantiles = c(0.02, 0.98), alpha = 1)

template <- predictions_filter[[1]]
p1 <- spplot(template,maxpixels=9999999,
             col.regions=as.character(cols_df$col),
             scales=list(draw=FALSE),
             par.settings = list(strip.background=list(col="lightgrey")),
             colorkey=TRUE,sp.layout =list(rgb),
             names.attr=c("RGB"))+
  spplot(template,  col.regions="transparent",sp.layout =rgb)


p_fig <- spplot(predictions_filter,maxpixels=9999999,
                scales=list(draw=FALSE),
                par.settings = list(strip.background=list(col="lightgrey")),
                col.regions=as.character(cols_df$col),
                names.attr=c("no selection", "random selection",
                                          "spatial selection"))

P_cbn <- latticeCombineGrid(list(p1,p_fig),layout=c(2,2))
P_cbn$condlevels[[1]][1] <- "RGB"
png(paste0(vispath,"/predictions_filter.png"), 
    width=18,height=14, units = "cm",res=600,type="cairo") 
print(P_cbn) 
dev.off()

