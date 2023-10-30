library(Rsagacmd)
library(lubridate)
library(sf)
library(terra)
library(tidyverse)

setwd("/projappl/project_2007415/repos/SCD_Kevo/")
planetdir <- "/scratch/project_2007415/Planet/Kevo"

f <- read_csv("data/selected_planets.csv") %>% 
  mutate(utc_time = hms(utc_time)) %>% 
  mutate(datetime = ymd_hms(paste(date, utc_time))) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2"))

if("class" %in% names(f)){
  f <- f %>% 
    filter(class != 2)
}

aoi <- st_read("data/aoi.gpkg")

# initiate a saga object
saga <- saga_gis(raster_backend = "terra", vector_format = "GeoPackage")

r <- rast(paste0(planetdir,"/files/",f$f[1]))[[1]]
# plot(r)

dem <- rast(paste0(planetdir,"/predictors/dem2.tif"))/100
dem <- project(dem, crs(r))
svf <- rast(paste0(planetdir,"/predictors/svf.tif"))
svf <- project(svf, crs(r))
# plot(svf)

lat <- st_coordinates(st_transform(st_centroid(aoi),4326))[1,"Y"]

for(i in 1:nrow(f)){
  print(i)
  
  if(!file.exists(paste0(planetdir,"/predictors/pisr/", f$f[i]))){
    
    r <-  rast(paste0(planetdir,"/files/",f$f[i]))[[1]]
    
    dem2 <- resample(dem, r)
    svf2 <- resample(svf, r)
    
    pisr <- saga$ta_lighting$potential_incoming_solar_radiation(grd_dem = dem2, grd_svf = svf2,
                                                                grd_total = "pisr.sgrd",
                                                                units = 1, latitude = lat, 
                                                                period = 0,
                                                                day = as.character(f$date[i]),
                                                                moment = hour(f$datetime[i]) + minute(f$datetime[i])/60, 
                                                                .all_outputs = F)
    
    # plot(pisr)
    # plot(r)
    writeRaster(round(pisr), paste0(planetdir,"/predictors/pisr/", f$f[i]),
                overwrite = T, datatype = "INT2U")
    
    saga_remove_tmpfiles(h = 0.1)
  }
}
