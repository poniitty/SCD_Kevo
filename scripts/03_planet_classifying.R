library(tidyverse)
library(sf)
library(randomForest)
library(terra)
library(foreach, quiet = TRUE)
library(doMPI, quiet = TRUE)

setwd("/projappl/project_2007415/repos/SCD_Kevo")
planetdir <- "/scratch/project_2007415/Planet/Kevo"

if(!dir.exists(paste0(planetdir,"/classifications"))){
  dir.create(paste0(planetdir,"/classifications"))
}

img_df <- read_csv("data/selected_planets.csv")

dfall <- read_csv("/projappl/project_2007415/repos/Iceland_SCD/data/Training_points.csv")

layers <- names(dfall)[2:5]

for(i in layers){
  print(i)
  layers <- layers[-1]
  for(ii in layers){
    dfall[,paste(i, ii, sep = "_")] <- (dfall[,i]-dfall[,ii])/(dfall[,i]+dfall[,ii])
  }
}

dfall %>% 
  filter(complete.cases(.)) %>% 
  filter(cirrus == 0 & is_sr == 1) %>% 
  dplyr::select(-cirrus,-is_sr,-scene) %>% 
  # group_by(class) %>% 
  #sample_n(., count(.) %>% as.data.frame() %>% .$n %>% min()) %>% 
  randomForest(as.factor(class) ~ ., data = .,
               ntree = 300, nodesize = 5) -> mod

cl<-startMPIcluster()
registerDoMPI(cl)

foreach(i = img_df$f, .packages = c("terra","randomForest")) %dopar% {
  #for(i in l[which(l == i):length(l)]){
  # i <- img_df$f[3]
  print(i)
  
  r <- rast(paste0(planetdir, "/coregistered/", i))
  
  chm <- rast(paste0(planetdir, "/chm.tif"))
  slo <- rast(paste0(planetdir, "/slope.tif"))
  
  # chm <- project(chm, r)/100
  # plot(chm)
  # writeRaster(chm, paste0(planetdir, "/chm.tif"), overwrite = T)
  
  r <- c(r, resample(chm, r[[1]]))
  r <- c(r, resample(slo, r[[1]]))
  #plot(r)
  names(r) <- c("blue","green","red","nir","chm","slope")
  
  layers <- names(r)[1:4]
  for(j in layers){
    print(j)
    layers <- layers[-1]
    for(jj in layers){
      r[[paste(j, jj, sep = "_")]] <- (r[[j]]-r[[jj]])/(r[[j]]+r[[jj]])
    }
  }
  
  rp <- predict(r, mod, type = "response")
  pr <- predict(r, mod, type = "prob")
  
  pr[[1]][rp %in% c(2,3)] <- NA
  pr[[2]][rp %in% c(1,3)] <- NA
  pr[[3]][rp %in% c(1,2)] <- NA
  #plot(pr)
  pr <- round(mean(pr, na.rm = T)*1000)
  # plot(rp)
  
  writeRaster(rp, paste0(planetdir, "/classifications/", gsub(".tif","_class.tif",i)),
              overwrite = T, datatype = "INT1U")
  writeRaster(pr, paste0(planetdir, "/classifications/", gsub(".tif","_probs.tif",i)),
              overwrite = T, datatype = "INT2U")
  
  
}
closeCluster(cl)
mpi.quit()
