library(tidyverse)
library(terra)
library(lubridate)
library(scales)
library(foreach)
library(sf)
library(fasterize)
library(doMPI, quiet = TRUE)

setwd("/projappl/project_2007415/repos/SCD_Kevo")
planetdir <- "/scratch/project_2007415/Planet/Kevo/"

if(!dir.exists(paste0(planetdir,"/temp"))){
  dir.create(paste0(planetdir,"/temp"))
}

img_df <- read_csv("data/selected_planets.csv")

if(!"date" %in% names(img_df)){
  img_df <- img_df %>% 
    mutate(date = as_date(paste(year,month,day,sep = "-")))
}

if(grepl("mosaiced",img_df$f[1])){
  img_df <- img_df %>% 
    mutate(f = gsub("mosaiced/","",f))
}

if("class" %in% names(img_df)){
  img_df <- img_df %>% 
    filter(class != 2)
}

shps <- list.files(paste0(planetdir, "/coregistered/"), pattern = "_mask.shp$", full.names = T)

master <- rast(paste0(planetdir, "/classifications/", gsub(".tif","_probs.tif",img_df$f[1])))

aoi <- st_bbox(master) %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_transform(crs = crs(master, proj = T))

pols <- st_make_grid(aoi, cellsize = 2000) %>% 
  st_as_sf() %>% 
  mutate(id = 1:nrow(.))

pols <- pols[aoi,]

cl<-startMPIcluster()
registerDoMPI(cl)
foreach(pol_i = seq_len(nrow(pols)), .packages = c("foreach","fasterize","scales",
                                                   "lubridate","tidyverse","sf","terra")) %dopar% {
# for(pol_i in seq_len(nrow(pols))){
  # pol_i <- 16
  print(pol_i)
  pol <- pols[pol_i,]
  if(!file.exists(paste0("temp/scd_",pol_i,".tif"))){
    
    master <- crop(rast(paste0(planetdir, "/classifications/", gsub(".tif","_probs.tif",img_df$f[3]))),
                   pol, snap = "out")
    
    st_class <- master[[1]]
    st_probs <- master[[1]]
    images <- c()
    for(i in img_df$f){
      # i <- "planet_2019-4-9_12-06-00_0f34.tif"
      print(i)
      
      cl <- rast(paste0(planetdir, "/classifications/", gsub(".tif","_class.tif",i)))
      pr <- rast(paste0(planetdir, "/classifications/", gsub(".tif","_probs.tif",i)))
      
      if(relate(ext(cl), ext(master), relation = "intersects")){
        cl <- crop(cl, master, snap = "out")
        pr <- crop(pr, master, snap = "out")
        
        cl[] <- as.numeric(values(cl, mat = F))
        
        cl[cl %in% c(0,4,5)] <- NA
        cl[cl < 3] <- 0
        cl[cl == 3] <- 1
        
        if(any(grepl(gsub(".tif","",i), shps))){
          
          msk <- st_read(shps[grepl(gsub(".tif","",i), shps)])
          
          if(class(msk$geometry)[1] == "sfc_GEOMETRYCOLLECTION"){
            clouds <- cl
            clouds[] <- 0
            names(clouds) <- "clouds"
          } else {
            msk <- msk %>% mutate(X1 = as.numeric(substr(X1, 1, 1))) %>%
              mutate(X1 = ifelse(is.na(X1), 2, X1))
            
            clouds <- fasterize(msk, raster(cl), field = "X1", fun = "max")
            clouds <- rast(clouds)
            names(clouds) <- "clouds"
            clouds[is.na(clouds)] <- 0
          }
          
          cl[clouds == 2] <- NA
          pr[clouds == 2] <- NA
        }
        
        # cl2 <- pr
        # cl2[] <- as.numeric(values(cl))
        
        if(class(try(compareGeom(master, cl))) == "try-error") {
          
          cl <- extend(cl, master)
          pr <- extend(pr, master)
          
          cl <- crop(cl, master, snap = "out")
          pr <- crop(pr, master, snap = "out")
        }
        
        if(sum(!is.na(values(cl)), na.rm = T) > 0){
          st_class <- c(st_class, cl)
          st_probs <- c(st_probs, pr)
          images <- c(images, i)
        } else {
          print("NO VALUES")
        }
      }
    }
    
    if(nlyr(st_class) >= 10){
      st_class <- st_class[[-1]]
      st_probs <- st_probs[[-1]]
      img_df2 <- img_df %>% filter(f %in% images)
      
      names(st_class) <- gsub("-","",as.character(img_df2$date))
      names(st_probs) <- gsub("-","",as.character(img_df2$date))
      
      dates <- unique(gsub("-","",as.character(img_df2$date)))
      which.max2 <- function(x, ...) ifelse( length(x) ==sum(is.na(x) ), 0, which.max(x))
      
      for(i in as.character(dates)){
        print(i)
        
        tcl <- st_class[[which(grepl(i, names(st_class)))]]
        tpr <- st_probs[[which(grepl(i, names(st_probs)))]]
        
        if(nlyr(tcl) > 1){
          
          wm <- app(tpr, which.max2)
          
          ovc <- selectRange(tcl, wm)
          
          ovp <- max(tpr, na.rm = T)
          
          names(ovc) <- i
          st_class <- c(ovc, subset(st_class, which(!grepl(i, names(st_class)))))
          
          names(ovp) <- i
          st_probs <- c(ovp, subset(st_probs, which(!grepl(i, names(st_probs)))))
        }
      }
      
      df <- terra::as.data.frame(st_class, na.rm = F)
      rm(st_class)
      dfp <- terra::as.data.frame(st_probs, na.rm = F)
      rm(st_probs)
      
      dfp <- dfp/1000
      
      dfp <- as.data.frame(apply(dfp, 2, function(x) round(rescale(x, to = c(0,1), from = c(0.333, 1)),3)))
      dfp[dfp < 0] <- 0
      
      doys <- yday(ymd(names(df)))
      years <- year(ymd(names(df)))
      
      results <- foreach(rown = 1:nrow(df), .combine=rbind) %do% {
        # rown <- 1
        # if(rown %% 100000==0) {
        #   # Print on the screen some message
        #   cat(paste0("iteration: ", rown, "\n"))
        # }
        
        results <- data.frame(id = rown,
                              n = NA,
                              scd = NA,
                              scd2 = NA)
        
        modd <- data.frame(class = as.numeric(df[rown,]),
                           doy = doys,
                           year = years,
                           prob = as.numeric(dfp[rown,]))
        
        modd <- modd[complete.cases(modd),]
        
        results[, "n"] <- NROW(modd)
        
        if(NROW(modd) > 20){
          if(min(modd$class) == 1){
            results[, "scd"] <- 175
            results[, "scd2"] <- 175
          } else {
            if(max(modd$class) == 0){
              results[, "scd"] <- 49
              results[, "scd2"] <- 49
            } else {
              
              mod <- glm(class ~ doy, weights = prob, data = modd, family = "binomial")
              
              pred_data <- expand.grid(doy = 49:175)
              
              pred <- cbind(pred_data, predict(mod, pred_data, type = "response"))
              
              results[, "scd"] <- as.numeric(pred[which.min(abs(pred[,2]-0.5)),1])
              results[, "scd2"] <- round(sum(pred[,2])+49)
            }
          }
        } else {
          # results[, "mdoy"] <- NA
        }
        return(results)
      }
      
      # write_csv(results, "Snow_mod_df.csv")
      
      master <- master[[1]]
      master[] <- results$scd
      writeRaster(master, paste0(planetdir, "/temp/scd_",pol_i,".tif"),
                  overwrite = T, datatype = "INT2U")
      
      master[] <- results$scd2
      writeRaster(master, paste0(planetdir, "/temp/scd2_",pol_i,".tif"),
                  overwrite = T, datatype = "INT2U")
      
      master[] <- results$n
      writeRaster(master, paste0(planetdir, "/temp/nobs_",pol_i,".tif"),
                  overwrite = T, datatype = "INT2U")
      
    }
  }
}

closeCluster(cl)
mpi.quit()
