library(tidyverse, quiet = TRUE)
library(raster, quiet = TRUE)
# install.packages("RStoolbox", lib ="/projappl/project_2003061/Rpackages")
library(RStoolbox, lib.loc = "/projappl/project_2003061/Rpackages")
library(foreach, quiet = TRUE)
library(doMPI, quiet = TRUE)

setwd("/projappl/project_2007415/repos/SCD_Kevo/")
planetdir <- "/scratch/project_2007415/Planet/Kevo"

img_df <- read_csv("data/selected_planets.csv")

if(!dir.exists(paste0(planetdir,"/coregistered"))){
  dir.create(paste0(planetdir,"/coregistered"))
}

# monthly medians

master_snowy <- stack(paste0(planetdir, "/files/2023-04-16_strip_6439339_composite.tif"))*1
master_snowfree <- stack(paste0(planetdir, "/files/2023-05-22_strip_6525993_composite.tif"))*1

master_snowy <- coregisterImages(master_snowy, ref = master_snowfree, shift = 5, verbose = F,
                           nSamples = 20000, reportStats = F)

img_df %>% mutate(exsts = f %in% list.files(paste0(planetdir, "/files/")))

tifs <- img_df %>% filter((!grepl(substr(names(master_snowy)[1],2,30), f)) &
                            (!grepl(substr(names(master_snowfree)[1],2,30), f))) %>% pull(f)


cl<-startMPIcluster()
registerDoMPI(cl)
foreach(i = tifs, .packages = c("raster")) %dopar% {
# for(i in tifs){
  # i <- tifs[2]
  # i <- "planet_2019-4-25_12-05-00_1013.tif"
  # print(i)
  if(!file.exists(paste0(planetdir, "/coregistered/", i))){
    
    rtemp <- stack(paste0(planetdir, "/files/",i))*1
    # rtemp <- crop(rtemp, bbox(rtemp)-1000)
    # plot(rtemp)
    
    library(RStoolbox, lib.loc = "/projappl/project_2003061/Rpackages")
    
    meanref <- mean(values(rtemp[[1]]), na.rm = T)
    
    if(meanref > 1500){
      rtemp2 <- coregisterImages(rtemp, ref = master_snowy, shift = 10, verbose = F,
                                nSamples = 20000, reportStats = F)
    } else (
      rtemp2 <- coregisterImages(rtemp, ref = master_snowfree, shift = 10, verbose = F,
                                nSamples = 20000, reportStats = F)
    )
    
    writeRaster(rtemp2, paste0(planetdir, "/coregistered/", i),
                datatype = "INT2U", overwrite = T)
    
  }
  
}

writeRaster(master_snowy, paste0(planetdir, "/coregistered/2023-04-16_strip_6439339_composite.tif"),
            datatype = "INT2U", overwrite = T)

writeRaster(master_snowfree, paste0(planetdir, "/coregistered/2023-05-22_strip_6525993_composite.tif"),
            datatype = "INT2U", overwrite = T)
