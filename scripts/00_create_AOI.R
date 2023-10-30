library(tidyverse)
library(sf)
library(terra)

planetdir <- "/scratch/project_2007415/Planet/Kevo"
tifs <- list.files(paste0(planetdir, "/files"), pattern = "_composite.tif$", full.names = T)

r <- rast(tifs[1])

p <- r %>% 
  st_bbox

p <- st_as_sf(st_as_sfc(p+c(-500,-500,500,500))) %>% 
  st_transform(crs = st_crs(r))

st_write(p, "data/aoi.gpkg")
