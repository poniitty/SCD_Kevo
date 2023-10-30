library(terra)
library(tidyverse)
library(sf)

planetdir <- "/scratch/project_2007415/Planet/Kevo"

if(!dir.exists("output")){
  dir.create("output")
}

vars <- unique(unlist(lapply(list.files(paste0(planetdir, "/temp/"), pattern = "tif$"),
                             function(x) str_split(x, "_")[[1]][1])))

for(i in vars){
  print(i)
  # i <- "scd2"
  f <- list.files(paste0(planetdir, "/temp/"), pattern = paste0(i,"_"), full.names = T)
  
  rast.list <- lapply(f, rast)
  rsrc <- sprc(rast.list)
  rast.mosaic <- mosaic(rsrc, fun = "mean")
  
  if(i == "nobs"){
    rast.mosaic[rast.mosaic == 0] <- NA
  }
  
  plot(rast.mosaic, main = i, col = rev(topo.colors(100)))
  
  writeRaster(rast.mosaic, paste0("output/",i,".tif"),
              datatype = "INT2U", overwrite = T)
  
}
