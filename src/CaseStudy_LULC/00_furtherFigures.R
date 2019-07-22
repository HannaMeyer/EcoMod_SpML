rm(list=ls())
library(raster)
library(sf)
library(caret)
library(viridis)


mainpath <- "/home/hanna/Documents/Projects/SpatialCV/MOF/"
datapath <- paste0(mainpath,"/data")
rasterpath <- paste0(datapath,"/raster")
vectorpath <- paste0(datapath,"/vector")
modelpath <- paste0(datapath,"/modeldat")
vispath <- paste0(mainpath,"/visualizations")

predictors <- stack(paste0(rasterpath,"/predictors.grd"))

subs <- stack(predictors$green,predictors$vvi,predictors$pca_9_sd,
              predictors$dem,predictors$lat,predictors$lon)
subs <- stretch(subs,0,255)

names(subs) <- c("Green","VVI","PCA_9_sd","DEM","Lat","Lon")
png(paste0(vispath,"/predictors.png"), width=15,height=10,units="cm",res = 500,type="cairo")
spplot(subs,col.regions=viridis(100),
       maxpixels=99999,
       colorkey=list(height = 1, labels = list(at = c(0,127,255), 
                                               labels = c("low","medium","high"))),
       par.settings =list(strip.background=list(col="grey")))
dev.off()


modeldata <- get(load(paste0(modelpath,"/modeldata_extended.RData")))
predictors <- c("red","green","blue","vvi","tgi","dem","slope","aspect",
                "lat","lon","ngrdi","gli","pca","pca_3_sd","pca_5_sd","pca_9_sd"#,
                # paste0("dist_",
                #  c("topleft","bottomleft","bottomright","topright","center"))
)

modeldata <- modeldata[complete.cases(modeldata[,which(names(modeldata)%in%predictors)]),]

M <- cor(modeldata[,predictors],method="spearman")
pdf(paste0(vispath,"/corr.pdf"), width=8,height=8)
corrplot(M, method = "circle",type = "lower",tl.col = "black")
dev.off()
