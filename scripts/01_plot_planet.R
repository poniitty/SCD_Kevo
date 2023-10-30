###########################################################################################################
## PLOT ALL PLANETS

library(raster, quiet = TRUE)
library(sf, quiet = TRUE)
library(tidyverse, quiet = TRUE)
library(lubridate, quiet = TRUE)
library(jsonlite)

planetdir <- "/scratch/project_2007415/Planet/Kevo"

tifs <- list.files(paste0(planetdir, "/files"), pattern = "_composite.tif$", full.names = T)
tifs <- sort(tifs)

pdf("visuals/PlanetScenes.pdf", 8, 8)
for(rasters in tifs){
  print(which(tifs == rasters))
  stemp <- stack(rasters)
  stemp[stemp > 8000] <- 8000
  plotRGB(stemp, r=4, g=3, b=2, scale = 8000)
  legend("top", legend = NA, title = rasters, bty = "n", cex = 1.3)
}
dev.off()

# Go through the plots

# Completely cloud free

sels <- c("2017-03-22_strip_441742_composite",
          "2017-05-15_strip_510767_composite",
          "2017-06-07_strip_536006_composite",
          "2017-06-09_strip_538931_composite",
          "2017-06-11_strip_543140_composite",
          "2017-06-12_strip_544265_composite",
          "2017-06-14_strip_549726_composite",
          "2017-07-23_strip_640224_composite",
          "2017-09-03_strip_730020_composite",
          "2018-02-20_strip_1194219_composite",
          "2018-03-03_strip_1230877_composite",
          "2018-03-15_strip_1270543_composite",
          "2018-04-08_strip_1336414_composite",
          "2018-05-01_strip_1393021_composite",
          "2018-05-08_strip_1410443_composite",
          "2018-05-10_strip_1415873_composite",
          "2018-05-11_strip_1418423_composite",
          "2018-05-15_strip_1427337_composite",
          "2018-05-16_strip_1430451_composite",
          "2018-05-18_strip_1434876_composite",
          "2018-05-25_strip_1453028_composite",
          "2018-06-16_strip_1504163_composite",
          "2018-07-03_strip_1540255_composite",
          "2018-07-14_strip_1564244_composite",
          "2018-07-19_strip_1574891_composite",
          "2018-07-28_strip_1593361_composite",
          "2018-08-24_strip_1652654_composite",
          "2018-09-09_strip_1687759_composite",
          "2019-03-02_strip_2165851_composite",
          "2019-03-11_strip_2193733_composite",
          "2019-03-17_strip_2210292_composite",
          "2019-03-22_strip_2223292_composite",
          "2019-03-22_strip_2223329_composite",
          "2019-04-04_strip_2256661_composite",
          "2019-04-25_strip_2311533_composite",
          "2019-05-06_strip_2340079_composite",
          "2019-05-09_strip_2348491_composite",
          "2019-06-14_strip_2443702_composite",
          "2019-06-15_strip_2446296_composite",
          "2019-06-17_strip_2451592_composite",
          "2019-06-20_strip_2459788_composite",
          "2019-07-07_strip_2502095_composite",
          "2019-07-11_strip_2510981_composite",
          "2019-08-23_strip_2621235_composite",
          "2019-09-05_strip_2652888_composite",
          "2020-03-04_strip_3198799_composite",
          "2020-03-14_strip_3228602_composite",
          "2020-03-27_strip_3265900_composite",
          "2020-03-27_strip_3265907_composite",
          "2020-04-01_strip_3280415_composite",
          "2020-04-05_strip_3291644_composite",
          "2020-04-20_strip_3332761_composite",
          "2020-05-06_strip_3374898_composite",
          "2020-05-10_strip_3384607_composite",
          "2020-05-10_strip_3385594_composite",
          "2020-05-22_strip_3418157_composite",
          "2020-05-22_strip_3418260_composite",
          "2020-05-23_strip_3421820_composite",
          "2020-06-01_strip_3448393_composite",
          "2020-06-02_strip_3450214_composite",
          "2020-06-10_strip_3473916_composite",
          "2020-06-13_strip_3481628_composite",
          "2020-06-17_strip_3492947_composite",
          "2020-06-20_strip_3500755_composite",
          "2020-06-23_strip_3508830_composite",
          "2020-07-03_strip_3537209_composite",
          "2020-07-15_strip_3571784_composite",
          "2020-07-27_strip_3602721_composite",
          "2020-07-29_strip_3607885_composite",
          "2020-08-04_strip_3623785_composite",
          "2020-08-10_strip_3638754_composite",
          "2020-09-17_strip_3734227_composite",
          "2021-03-09_strip_4245738_composite",
          "2021-03-17_strip_4273673_composite",
          "2021-03-22_strip_4292643_composite",
          "2021-03-28_strip_4314521_composite",
          "2021-04-08_strip_4353306_composite",
          "2021-04-08_strip_4353341_composite",
          "2021-04-12_strip_4368830_composite",
          "2021-04-16_strip_4381174_composite",
          "2021-04-19_strip_4391606_composite",
          "2021-04-23_strip_4404828_composite",
          "2021-04-27_strip_4418488_composite",
          "2021-05-04_strip_4442197_composite",
          "2021-05-09_strip_4460127_composite",
          "2021-05-19_strip_4493721_composite",
          "2021-05-25_strip_4513901_composite",
          "2021-06-02_strip_4542459_composite",
          "2021-06-03_strip_4546261_composite",
          "2021-06-06_strip_4556587_composite",
          "2021-06-07_strip_4560599_composite",
          "2021-06-12_strip_4576898_composite",
          "2021-06-24_strip_4619672_composite",
          "2021-07-02_strip_4648444_composite",
          "2021-08-07_strip_4771945_composite",
          "2021-08-27_strip_4839382_composite",
          "2021-09-20_strip_4919376_composite",
          "2021-09-29_strip_4948932_composite",
          "2022-03-16_strip_5492208_composite",
          "2022-03-26_strip_5519904_composite",
          "2022-03-27_strip_5522519_composite",
          "2022-04-08_strip_5554920_composite",
          "2022-04-20_strip_5584513_composite",
          "2022-05-07_strip_5621550_composite",
          "2022-05-09_strip_5626451_composite",
          "2022-05-15_strip_5638119_composite",
          "2022-05-26_strip_5663422_composite",
          "2022-05-30_strip_5672647_composite",
          "2022-05-31_strip_5674959_composite",
          "2022-05-31_strip_5674961_composite",
          "2022-05-31_strip_5675314_composite",
          "2022-06-01_strip_5677003_composite",
          "2022-06-01_strip_5677244_composite",
          "2022-06-10_strip_5700634_composite",
          "2022-06-26_strip_5740773_composite",
          "2022-06-29_strip_5747915_composite",
          "2022-07-02_strip_5755290_composite",
          "2022-08-14_strip_5857494_composite",
          "2023-03-02_strip_6324806_composite",
          "2023-03-05_strip_6332558_composite",
          "2023-03-08_strip_6342667_composite",
          "2023-03-10_strip_6345331_composite",
          "2023-03-13_strip_6353882_composite",
          "2023-03-16_strip_6360308_composite",
          "2023-03-24_strip_6381177_composite",
          "2023-03-28_strip_6389806_composite",
          "2023-04-10_strip_6423876_composite",
          "2023-04-13_strip_6431461_composite",
          "2023-04-16_strip_6439339_composite",
          "2023-04-25_strip_6459444_composite",
          "2023-04-27_strip_6464280_composite",
          "2023-04-28_strip_6466850_composite",
          "2023-05-06_strip_6488088_composite",
          "2023-05-15_strip_6509649_composite",
          "2023-05-22_strip_6525993_composite",
          "2023-06-11_strip_6571112_composite",
          "2023-06-11_strip_6571805_composite",
          "2023-06-14_strip_6578197_composite",
          "2023-06-19_strip_6589561_composite",
          "2023-06-22_strip_6595827_composite",
          "2023-06-25_strip_6602800_composite",
          "2023-07-12_strip_6643745_composite",
          "2023-09-08_strip_6759777_composite")

sels <- paste0(sels, ".tif")


df <- tibble(f = unlist(lapply(tifs, function(x) tail(str_split(x,"/")[[1]], 1)))) %>% 
  mutate(year = as.numeric(substr(f, 1, 4)),
         month = as.numeric(substr(f, 6, 7)),
         day = as.numeric(substr(f, 9, 10)),
         utc_time = unlist(lapply(tifs, function(x) { format(as.POSIXct(as_datetime(fromJSON(gsub("_composite.tif","_composite_metadata.json",x))$properties$acquired)), format = "%H:%M:%S") })))

df <- df %>% 
  filter(f %in% sels)

df <- df %>% 
  mutate(date = as_date(paste(year,month,day,sep = "-")))

write_csv(df, "data/selected_planets.csv")

# For cloud masking

