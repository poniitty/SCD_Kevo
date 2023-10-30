# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# install.packages("terra", lib = "/projappl/project_2003061/Rpackages")
library(terra)
library(tidyverse)
library(lubridate)

planetdir <- "/scratch/project_2007415/Planet/Kevo"

if(!dir.exists(paste0(planetdir,"/predictors"))){
  dir.create(paste0(planetdir,"/predictors"))
}

# MEDIAN PLANETS

img_df <- read_csv("data/selected_planets.csv") %>% 
  mutate(utc_time = hms(utc_time)) %>% 
  mutate(datetime = ymd_hms(paste(date, utc_time))) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2"))

if("class" %in% names(img_df)){
  img_df <- img_df %>% 
    filter(class == 0)
}

mnths <- unique(month(img_df$date)) %>% sort

for(i in mnths){
  
  f <- img_df %>% 
    filter(month %in% i) %>% 
    filter(hour(datetime) %in% 11:13) %>% pull(f)
  
  if(length(f) > 3){
    print(i)
    rast.list <- lapply(f, function(x) { rast(paste0(planetdir, "/files/", x)) })
    rast.list <- sprc(rast.list)
    rast.mosaic <- mosaic(rast.list, fun = "median")
    names(rast.mosaic) <- c("blue","green","red","nir")
    # plot(rast.mosaic)
    writeRaster(round(rast.mosaic), paste0(planetdir, "/predictors/planet_",tolower(month.name[i]),"_median.tif"),
                overwrite = T)
    
  }
}
