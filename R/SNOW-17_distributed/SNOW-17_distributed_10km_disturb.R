### distributed snow-17 
## the following script is written in R programming language based on the Mark Raleigh MATLAB code of SNOW-17.
library(terra)
library(stringr)
source("R/SNOW-17_distributed/snow17_rasterfunction.R")
rs <- terra::rast("grids/inputs/tair_1961-01-01_10km.nc")

TIME <- readRDS("tabs/arrays/timp_1961-2013_wy.rds")
Temp <- readRDS("tabs/arrays/tair_1961-2013_10_km_wy.rds")
Temp <- Temp[,,ymd(TIME)>= "1992-10-15"]
Prec <- readRDS("tabs/arrays/precip_1961-2013_10km_wy.rds")
Prec <- Prec[,,ymd(TIME)>= "1992-10-15"]
TIME <- TIME[ymd(TIME)>= "1992-10-15"]
elev <- readRDS("tabs/arrays/ro_elevation_10km.rds")

sINPUTS <- list(Temp = Temp, Prec = Prec, elev = elev, TIME = TIME)
sIC <- list(ATI = 0, W_q = 0, W_i = 0, Deficit = 0, SNDEN = 0, SNTMP = 0, SGMLOS = 0, SRFRZ = 0)
sSettings  <- list(RvS = 0)

PARAMS <- expand.grid(SCF = seq(0.4,1.8,.1), UADJ = 0.04, MBASE = seq(-.4,1.4,.2), MFMAX = 1.05, MFMIN = 0.60,TIPM = 0.1,NMF = 0.15, PLWHC = 0.04,
                      PXTEMP = seq(-1.5,1.5,.5), C1 = 0.026, C2 = 0.005, C3 = 0.005,C4 = 0.10, THRESD = 0.15, C5 = 2.0, CX = 23.0)
nrow(PARAMS)

for(p in 2:nrow(PARAMS)){
  
  print(PARAMS[p,])
  s17_10km <- snow17dc(sINPUTS = sINPUTS,sIC = sIC, sSETTINGS = sSettings,sPARAMS = PARAMS[p,])
  
  ###SWE
  SWE  <- terra::rast(s17_10km$SWE)
  ext(SWE) <- ext(rs) 
  crs(SWE) <- "+init=epsg:4326"
  writeCDF(SWE, paste0("~/D/2023/nc/10km/SWE/swe_", str_c(as.character(PARAMS[p,]),sep = "_", collapse = "_"),"_1991-2013.nc"), overwrite = T)
  
  ###SD
  SD  <- terra::rast(s17_10km$Hn)
  ext(SD) <- ext(rs) 
  crs(SD) <- "+init=epsg:4326"
  writeCDF(SD, paste0("~/D/2023/nc/10km/SD/sd_", str_c(as.character(PARAMS[p,]),sep = "_", collapse = "_"),"_1991-2013.nc"),overwrite = T)
  
  ##Sfall
  Sfall  <- terra::rast(s17_10km$Sfall)
  ext(Sfall) <- ext(rs) 
  crs(Sfall) <- "+init=epsg:4326"
  writeCDF(Sfall, paste0("~/D/2023/nc/10km/Sfall/sfall_", str_c(as.character(PARAMS[p,]),sep = "_", collapse = "_"),"_1991-2013.nc"),overwrite = T)
  gc()
}

plot(SWE)
