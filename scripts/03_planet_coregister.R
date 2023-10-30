library(tidyverse)
library(raster)
library(foreach)
library(sf)
library(doMPI, quiet = TRUE)
library(RStoolbox, lib.loc = "/projappl/project_2003061/Rpackages/")

setwd("/projappl/project_2007415/repos/SCD_Kevo/")
planetdir <- "/scratch/project_2007415/Planet/Kevo"

if(!dir.exists(paste0(planetdir,"/coregistered"))){
  dir.create(paste0(planetdir,"/coregistered"))
}

img_df <- read_csv("data/selected_planets.csv") %>% 
  mutate(x_shift = NA, y_shift = NA)

if("class" %in% names(img_df)){
  img_df <- img_df %>% 
    filter(class != 2)
}

aoi <- st_read("data/aoi.gpkg")

masks <- list.files(paste0(planetdir,"/files"), pattern = "_mask.shp", full.names = T)
tpoints <- list.files(paste0(planetdir,"/files"), pattern = "_trainpoints.shp", full.names = T)

cl<-startMPIcluster()
registerDoMPI(cl)

foreach(i = img_df$f, .packages = c("raster","sf")) %dopar% {
  # for(i in img_df$f){
  # i <- img_df$f[1]
  library(RStoolbox, lib.loc = "/projappl/project_2003061/Rpackages/")
  
  mnth <- img_df %>% filter(f == i) %>% pull(month)
  
  rtemp <- stack(paste0(planetdir,"/files/", i))*1
  
  if(file.exists(paste0(planetdir, "/predictors/planet_",tolower(month.name[mnth]),"_median.tif"))){
    master <- stack(paste0(planetdir, "/predictors/planet_",tolower(month.name[mnth]),"_median.tif"))*1
  } else {
    if(file.exists(paste0(planetdir, "/predictors/planet_",tolower(month.name[mnth-1]),"_median.tif"))){
      master <- stack(paste0(planetdir, "/predictors/planet_",tolower(month.name[mnth-1]),"_median.tif"))*1
    } else {
      if(file.exists(paste0(planetdir, "/predictors/planet_",tolower(month.name[mnth+1]),"_median.tif"))){
        master <- stack(paste0(planetdir, "/predictors/planet_",tolower(month.name[mnth+1]),"_median.tif"))*1
      } else {
        stop("NO SUITABLE MONTHLY MEDIAN RASTER FOUND")
      }
    }
  }
  
  if(any(grepl(gsub(".tif","",i), masks))){
    shp <- st_read(masks[grepl(gsub(".tif","",i), masks)])
    shp <- shp %>% filter(is.na(X1)) %>% st_transform(crs = crs(rtemp))
    try(rtemp <- mask(rtemp, shp, inverse = T))
  }
  
  rtemp <- coregisterImages(rtemp, ref = master, shift = 10, verbose = T,
                            nSamples = 10000, reportStats = TRUE)
  
  img_df[img_df$f == i, "x_shift"] <- rtemp$bestShift$x
  img_df[img_df$f == i, "y_shift"] <- rtemp$bestShift$y
  
  rtemp2 <- stack(paste0(planetdir,"/files/", i))
  rtemp2 <- shift(rtemp2, dx=rtemp$bestShift$x, dy=rtemp$bestShift$y)
  
  rtemp2 <- crop(rtemp2, aoi %>% st_transform(crs = crs(rtemp2)), snap = "out")
  writeRaster(rtemp2, paste0(planetdir,"/coregistered/",i),
              format = "GTiff", datatype = "INT2U", overwrite = T)
  
  if(any(grepl(gsub(".tif","",i), masks))){
    shp <- st_read(masks[grepl(gsub(".tif","",i), masks)]) %>% 
      st_transform(crs = crs(rtemp2))
    shp_crs <- st_crs(shp)
    st_geometry(shp) <- st_geometry(shp) + c(rtemp$bestShift$x, rtemp$bestShift$y)
    shp <- st_set_crs(shp, shp_crs)
    if(any(!is.na(as.numeric(st_bbox(shp))))){
      st_write(shp, gsub("files/","coregistered/",masks[grepl(gsub(".tif","",i), masks)]), append = FALSE)
    }
  }
  
  if(any(grepl(gsub(".tif","",i), tpoints))){
    shp <- st_read(tpoints[grepl(gsub(".tif","",i), tpoints)]) %>% 
      st_transform(crs = crs(rtemp2))
    shp_crs <- st_crs(shp)
    st_geometry(shp) <- st_geometry(shp) + c(rtemp$bestShift$x, rtemp$bestShift$y)
    shp <- st_set_crs(shp, shp_crs)
    if(nrow(shp) > 10){
      st_write(shp, gsub("files/","coregistered/",tpoints[grepl(gsub(".tif","",i), tpoints)]), append = FALSE)
    }
  }
}

write_csv(img_df, "selected_planets_with_shifts.csv")

closeCluster(cl)
mpi.quit()