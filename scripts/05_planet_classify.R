library(tidyverse)
library(sf)
library(randomForest)
library(terra)
library(fasterize)
library(foreach, quiet = TRUE)
library(doMPI, quiet = TRUE)

setwd("/projappl/project_2007415/repos/SCD_Kevo/")
planetdir <- "/scratch/project_2007415/Planet/Kevo"

if(!dir.exists(paste0(planetdir,"/classifications"))){
  dir.create(paste0(planetdir,"/classifications"))
}


img_df <- read_csv("data/selected_planets.csv")

if("class" %in% names(img_df)){
  img_df <- img_df %>% 
    filter(class != 2)
}

mod <- readRDS("/projappl/project_2003061/repos/Planet_RF/output/RF_model_all.rds")
shps <- list.files(paste0(planetdir,"/coregistered"), pattern = "_mask.shp", full.names = T)

done <- gsub("_probs.tif",".tif",list.files(paste0(planetdir,"/classifications/"), pattern = "probs.tif$"))
img_df <- img_df %>% filter(!f %in% done)

cl<-startMPIcluster()
registerDoMPI(cl)

# for(i in img_df$f){
foreach(i = img_df$f, .packages = c("terra","tidyverse","sf","fasterize","tidyr","randomForest")) %dopar% {  
  # i <- "2023-03-13_strip_6353882_composite.tif"
  print(i)
  
  r <- rast(paste0(planetdir,"/coregistered/",i))
  
  names(r) <- c("blue","green","red","nir")
  
  layers <- c("blue","green","red","nir")
  for(j in layers){
    print(j)
    layers <- layers[-1]
    for(jj in layers){
      r[[paste(j, jj, sep = "_")]] <- (r[[j]]-r[[jj]])/(r[[j]]+r[[jj]])
    }
  }
  
  chm <- rast(paste0(planetdir,"/predictors/chm.tif"))
  names(chm) <- "chm"
  chm <- project(chm, r)
  
  chm_conif <- rast(paste0(planetdir,"/predictors/chm_conif.tif"))
  names(chm_conif) <- "chm_conif"
  chm_conif <- project(chm_conif, r)
  chm_conif[is.na(chm_conif)] <- 0
  
  slope <- rast(paste0(planetdir,"/predictors/slope.tif"))
  names(slope) <- "slope"
  slope <- project(slope, r)
  
  pisr <- rast(paste0(planetdir,"/predictors/pisr/",i))
  names(pisr) <- "pisr"
  pisr <- project(pisr, r)
  
  r[["blue_min"]] <- focal(r[["blue"]], w = 5, fun = "min", expand = T)
  r[["nir_min"]] <- focal(r[["nir"]], w = 5, fun = "min", expand = T)
  
  r[["blue_max"]] <- focal(r[["blue"]], w = 5, fun = "max", expand = T)
  r[["nir_max"]] <- focal(r[["nir"]], w = 5, fun = "max", expand = T)
  
  if(any(grepl(gsub(".tif","",i), shps))){
    
    msk <- st_read(shps[grepl(gsub(".tif","",i), shps)])
    
    if(class(msk$geometry)[1] == "sfc_GEOMETRYCOLLECTION"){
      clouds <- r[[1]]
      clouds[] <- 0
      names(clouds) <- "clouds"
    } else {
      msk <- msk %>% mutate(X1 = as.numeric(substr(X1, 1, 1))) %>%
        mutate(X1 = ifelse(is.na(X1), 2, X1))
      
      clouds <- fasterize(msk, raster(chm), field = "X1", fun = "max")
      clouds <- rast(clouds)
      names(clouds) <- "clouds"
      clouds[is.na(clouds)] <- 0
    }
  } else {
    clouds <- r[[1]]
    clouds[] <- 0
    names(clouds) <- "clouds"
  }
  
  r_march <- rast(paste0(planetdir,"/predictors/planet_march_median.tif"))
  r_march <- project(r_march, r)
  names(r_march) <- paste0("march_", c("blue","green","red","nir"))
  
  r_july <- rast(paste0(planetdir,"/predictors/planet_july_median.tif"))
  r_july <- project(r_july, r)
  names(r_july) <- paste0("july_", c("blue","green","red","nir"))
  
  rs <- c(r, chm, chm_conif, slope, pisr, clouds, r_march, r_july)
  
  levels(rs[["clouds"]]) <- data.frame(value = c(0:2),
                                       category = c("0","1","2"))
  
  rp <- predict(rs, mod, type = "response")
  pr <- predict(rs, mod, type = "prob", index=1:5)
  pr2 <- pr
  
  pr[[1]][rp %in% c(2:5)] <- NA
  pr[[2]][rp %in% c(1,3:5)] <- NA
  pr[[3]][rp %in% c(1,2,4,5)] <- NA
  pr[[4]][rp %in% c(1:3,5)] <- NA
  pr[[5]][rp %in% c(1:4)] <- NA
  #plot(pr)
  pr <- round(mean(pr, na.rm = T)*1000)
  # plot(rp)
  # plotRGB(r, r=4, g=3, b=2, scale = 8000)
  writeRaster(rp, paste0(planetdir,"/classifications/", gsub(".tif","_class.tif",i)),
              overwrite = T, datatype = "INT1U")
  writeRaster(pr, paste0(planetdir,"/classifications/", gsub(".tif","_probs.tif",i)),
              overwrite = T, datatype = "INT2U")
  
  pr2[[1]] <- pr2[[1]] + pr2[[2]]
  pr2 <- subset(pr2, c(1,3,4,5))
  pr2[[1]][rp %in% c(3:5)] <- NA
  pr2[[2]][rp %in% c(1,2,4,5)] <- NA
  pr2[[3]][rp %in% c(1:3,5)] <- NA
  pr2[[4]][rp %in% c(1:4)] <- NA
  
  pr2 <- round(mean(pr2, na.rm = T)*1000)
  # plot(pr2)
  writeRaster(pr2, paste0(planetdir,"/classifications/", gsub(".tif","_probscomb.tif",i)),
              overwrite = T, datatype = "INT2U")
  
}

closeCluster(cl)
mpi.quit()