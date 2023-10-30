# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# install.packages("terra", lib = "/projappl/project_2003061/Rpackages")
library(terra)
library(Rsagacmd)
library(tidyverse)
library(lubridate)
library(sf)
library(lidR)

# study area polygon
aoi <- st_read("data/aoi.gpkg")

planetdir <- "/scratch/project_2007415/Planet/Kevo/"

if(!dir.exists(paste0(planetdir,"/temp"))){
  dir.create(paste0(planetdir,"/temp"))
}
if(!dir.exists(paste0(planetdir,"/predictors"))){
  dir.create(paste0(planetdir,"/predictors"))
}
if(!dir.exists(paste0(planetdir,"/predictors/pisr"))){
  dir.create(paste0(planetdir,"/predictors/pisr"))
}

# FIRST CREATE DEM & CANOPY COVER ETC...

latest <- st_read("/appl/data/geo/mml/dem2m/2008_latest/dem2m.shp")

aoi <- aoi %>%
  st_transform(crs = st_crs(latest))

roi_t <- aoi %>% 
  st_buffer(500)

latest_t <- latest[roi_t,]

#################################################################
# Merge files

if(nrow(latest_t) > 0){
  
  f <- latest_t$path
  
  rast.list <- lapply(f, rast)
  rast.list <- sprc(rast.list)
  rast.mosaic <- mosaic(rast.list, fun = "mean")
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  
  plot(rast.mosaic)
  
  writeRaster(round(rast.mosaic*100), paste0(planetdir,"/predictors/dem2.tif"),
              filetype = "GTiff", datatype = "INT4U", overwrite = T)
  
}

unlink(list.files(tempdir(), full.names = T, recursive = T))

# initiate a saga object
saga <- saga_gis(cores = 8)

dem <- rast(paste0(planetdir,"/predictors/dem2.tif"))/100

lat <- st_coordinates(st_transform(st_centroid(st_as_sf(st_as_sfc(st_bbox(dem)))),4326))[1,"Y"]

svf <- saga$ta_lighting$sky_view_factor(dem = dem, svf = "svf.sgrd", radius = 1000)
writeRaster(svf$svf, paste0(planetdir,"/predictors/svf.tif"),
            filetype = "GTiff", overwrite = T)

# Slope

slp <- saga$ta_morphometry$slope_aspect_curvature(elevation = dem)
writeRaster(slp$slope, paste0(planetdir,"/predictors/slope.tif"),
            filetype = "GTiff", overwrite = T)

# chm

get_lidr_threads()
set_lidr_threads(future::availableCores())
get_lidr_threads()

max_z <- 40 # in meters

latest <- st_read("/appl/data/geo/mml/laserkeilaus/2008_latest/2008_latest.shp")

aoi <- aoi %>%
  st_transform(crs = st_crs(latest))

pols <- st_make_grid(aoi, cellsize = 3000) %>% 
  st_as_sf() %>% 
  mutate(id = 1:nrow(.))

for(i in pols$id){
  # i <- 6
  print(i)
  
  roi_t <- pols %>% filter(id == i) %>% 
    st_buffer(10)
  
  latest_t <- latest[roi_t,]
  
  if(nrow(latest_t) > 0){
    lass <- readLAS(latest_t$path)
    
    # CLIP THE LAS
    lass <- clip_roi(lass, roi_t)
    
    if(lass@header@PHB$`Number of point records` > 1000){
      
      dtm <- grid_terrain(lass, 2, tin())
      # plot(dtm)
      
      lass <- normalize_height(lass, tin(), na.rm = TRUE)
      
      lass <- filter_poi(lass, Z <= max_z)
      
      chm <- grid_canopy(lass, res = 2, algorithm = dsmtin(max_edge = 5))
      
      chm[chm < 0] <- 0
      chm[is.na(chm)] <- 0
      
      roi_t <- pols %>% filter(id == i)
      
      chm <- crop(chm, roi_t, snap = "out")
      
      writeRaster(round(chm*100), paste0(planetdir,"/temp/chm_",i,".tif"),
                  format = "GTiff", datatype = "INT2U", overwrite = T)
    }
  }
}

#################################################################
# Merge files

# vars <- unique(unlist(lapply(list.files(paste0(planetdir,"/temp/"), pattern = "tif$"), function(x) str_split(x, "_")[[1]][1])))
vars <- "chm"
for(i in vars){
  print(i)
  
  f <- list.files(paste0(planetdir,"/temp"), pattern = paste0(i,"_"), full.names = T)
  
  rast.list <- lapply(f, rast)
  rast.list <- sprc(rast.list)
  rast.mosaic <- mosaic(rast.list, fun = "mean")
  
  plot(rast.mosaic, main = i)
  
  rast.mosaic <- crop(rast.mosaic, aoi)
  rast.mosaic <- mask(rast.mosaic, aoi)
  
  writeRaster(rast.mosaic, paste0(planetdir,"/predictors/",i,".tif"),
              datatype = dataType(raster(f[1])), overwrite = T)
  
}

unlink(list.files(paste0(planetdir,"/temp/"), full.names = T))

# LUKE

tifs <- list.files("/appl/data/geo/luke/vmi/2021", pattern = "img$", full.names = T)

r1 <- rast("/appl/data/geo/luke/vmi/2021/latvuspeitto_vmi1x_1721.img")
r2 <- rast("/appl/data/geo/luke/vmi/2021/lehtip_latvuspeitto_vmi1x_1721.img")
crs(r1) <- "+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs"
crs(r2) <- "+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs"

r1 <- crop(r1, aoi, snap = "out")
r2 <- crop(r2, aoi, snap = "out")
r1[r1 > 100] <- NA
r2[r2 > 100] <- NA

r1 <- r1-r2

chm <- rast(paste0(planetdir,"/predictors/chm.tif"))

plot(chm)
chm2 <- project(chm, r1)

plot(chm2)

r1[chm2 < 100] <- 0
r1 <- resample(r1, chm)
r1 <- mask(raster(r1), aoi)
plot(r1)

writeRaster(r1, paste0(planetdir,"/predictors/chm_conif.tif"),
            filetype = "GTiff", overwrite = T)
