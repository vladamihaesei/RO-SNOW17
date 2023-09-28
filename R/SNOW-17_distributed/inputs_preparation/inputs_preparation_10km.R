library(terra)
library(sf)
library(lubridate)
library(tidyr)
source("R/water_year.R")

files <- list.files("/media/vlad/Z_vld/rocada/zilnic", pattern = ".nc", full.names = T)
files <- list.files("~/Z/rocada", pattern = ".nc", full.names = T)

tavg.gr <- grep("Tmean.nc", files, value = T)
precip.gr <- grep("Precipitation.nc", files, value = T)[1]
sf <- read_sf("shp/ROU_adm0.shp")%>%st_set_crs(4326)

temp   <- terra::rast(tavg.gr)
temp   <- crop(x = temp, y = sf)

precip <- rast(precip.gr)
precip <- crop(x = precip, y = sf)

time(temp)   <- as.Date(seq(as.Date("1961-01-01"), as.Date("2013-12-31"), "days"))
time(precip) <- as.Date(seq(as.Date("1961-01-01"), as.Date("2013-12-31"), "days"))

ani <- unique(year(time(temp)))

temp_wy <- wateryeargrid(data = temp)
temp.f <- temp_wy[[which(month(time(temp_wy)) %in% c("1","2","3","4","5","10","11","12","6"))]]

precip_wy <- wateryeargrid(data = precip)
precip.f <- precip_wy[[which(month(time(precip_wy)) %in% c("1","2","3","4","5","10","11","12","6"))]]

writeCDF(precip_wy, "grids/inputs/10km/precip_daily_1961-2013_wy.nc")
writeCDF(temp_wy, "grids/inputs/10km/tair_daily_1961-2014_wy.nc")

temp.arr <- as.array(temp.f)
dimnames(temp.arr)[3] <- list(as.character(time(temp.f)))
precip.arr <- as.array(precip.f)#%>%na.exclude()
dimnames(precip.arr)[3] <- list(as.character(time(precip.f)))

saveRDS(temp.arr,"tabs/arrays/tair_1961-2013_10_km_wy.rds")
saveRDS(precip.arr,"tabs/arrays/precip_1961-2013_10km_wy.rds")

##time
timp <- as.array(terra::time(temp.f))
saveRDS(timp,"tabs/arrays/timp_1961-2013_wy.rds")

##### elevation 
elev <- terra::rast("grids/inputs/mnt_proj4326.tif")
#newproj1 <- "+init=epsg:3844"
#elev <- terra::project(elev, newproj1)

elev <- terra::resample(elev, temp.f[[1]], method = "near")
elev[is.na(temp[[1]])] <- NA
elev[elev < 1] <- 1
elev <- as.array(elev)#%>%na.omit()
saveRDS(elev,"tabs/arrays/ro_elevation_10km.rds")

### t
g <- terra::rast(files[2])
g
