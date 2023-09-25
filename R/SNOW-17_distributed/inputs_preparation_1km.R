library(terra)
library(sf)
library(lubridate)
library(tidyr)
source("R/SNOW-17_point/water_year.R")
files <- list.files("/media/vlad/Expansion/nc/observate/zilnice", pattern = ".nc", full.names = T)
tavg.gr <- grep("tavg", files, value = T)[3:4]
precip.gr <- grep("precip",files, value = T)[3:4]
sf <- read_sf("shp/ROU_adm0.shp")%>%st_set_crs(4326)

temp   <- terra::rast(tavg.gr[1])
temp <- crop(x = temp, y = sf)
precip <- rast(precip.gr[1])
precip <- crop(x = precip, y = sf)

for (l in 2:length(precip.gr)){
  
  tavg.r   <-   rast(tavg.gr[l])

  precip.r <- rast(precip.gr[l])
  
  tavg.r1   <- crop(tavg.r,temp)
  precip.r1 <- crop(precip.r,precip)
  
  ext(tavg.r1)   <- ext(temp)
  ext(precip.r1) <- ext(precip)
  
  temp <-   c(temp,tavg.r1)
  precip <- c(precip,precip.r1)
  
}

ani <- unique(year(time(temp)))
ani

temp_wy <- wateryeargrid(data = temp)
temp.f <- temp_wy[[which(month(time(temp_wy)) %in% c("1","2","3","4","5","11","12"))]]

precip_wy <- wateryeargrid(data = precip)
precip.f <- precip_wy[[which(month(time(precip_wy)) %in% c("1","2","3","4","5","11","12"))]]

temp.arr <- as.array(temp.f)
precip.arr <- as.array(precip.f)#%>%na.exclude()
saveRDS(temp.arr,"tabs/arrays/tair_2016_wy.rds")
saveRDS(precip.arr,"tabs/arrays/precip_2016_wy.rds")

##time
timp <- as.array(terra::time(temp.f))
saveRDS(timp,"tabs/arrays/timp_2014_wy.rds")

##### elevation 
elev <- terra::rast("grids/inputs/mnt_proj4326.tif")
newproj1 <- "+init=epsg:3844"
elev <- terra::project(elev, newproj1)

elev <- terra::resample(elev, temp, method = "near")
elev[is.na(temp)] <- NA
elev[elev < 1] <- 1
elev <- as.array(elev)#%>%na.omit()
saveRDS(elev,"tabs/arrays/ro_elevation_1km.rds")


### t
g <- terra::rast(files[2])
g
