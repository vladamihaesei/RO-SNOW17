### distributed snow-17 
## the following script is written in R programming language based on the Mark Raleigh MATLAB code of SNOW-17.
source("R/SNOW-17_distributed/snow17_rasterfunction.R")
rs <- terra::rast("grids/inputs/tair_1961-01-01_10km.nc")

Temp <- readRDS("tabs/arrays/tair_1961-2013_10_km_wy.rds")
Prec <- readRDS("tabs/arrays/precip_1961-2013_10km_wy.rds")
TIME <- readRDS("tabs/arrays/timp_1961-2013_wy.rds")
elev <- readRDS("tabs/arrays/ro_elevation_10km.rds")

sINPUTS <- list(Temp = Temp, Prec = Prec, elev = elev, TIME = TIME)
sIC <- list(ATI = 0, W_q = 0, W_i = 0, Deficit = 0, SNDEN = 0, SNTMP = 0, SGMLOS = 0, SRFRZ = 0)
sSettings  <- list(RvS = 0)

PARAMS <- expand.grid(SCF = seq(0.4,1.8,.1), UADJ = 0.04, MBASE = seq(-.4,1.4,.2), MFMAX = 1.05, MFMIN = 0.60,TIPM = 0.2, NMF = 0.1, PLWHC = 0.04,
                      PXTEMP = seq(-1.5,1.5,.5), PXTEMP1 = -1, PXTEMP2 = -3, Tt = 2.8, Tr = 13)
nrow(PARAMS)

for(p in 1:nrow(PARAMS)){
  
  s17_1km <- snow17dc(sINPUTS = sINPUTS,sIC = sIC, sSETTINGS = sSettings,sPARAMS = PARAMS[p,])
  
  ###SWE
  SWE  <- terra::rast(s17_1km$SWE)
  ext(SWE) <- ext(rs) 
  crs(SWE) <- "+init=epsg:4326"
  writeCDF(SWE, paste0("~/D/2023/nc/10km/SWE/swe_", str_c(as.character(PARAMS[p,]),sep = "_", collapse = "_"),"_1961-2013.nc"))
  ###SD
  SD  <- terra::rast(s17_1km$SD)
  ext(SD) <- ext(rs) 
  crs(SD) <- "+init=epsg:4326"
  writeCDF(SD, paste0("~/D/2023/nc/10km/SD/sd_", str_c(as.character(PARAMS[p,]),sep = "_", collapse = "_"),"_1961-2013.nc"))
  
  Sfall  <- terra::rast(s17_1km$Sfall)
  ext(Sfall) <- ext(rs) 
  crs(Sfall) <- "+init=epsg:4326"
  writeCDF(Sfall, paste0("~/D/2023/nc/10km/Sfall/sfall_", str_c(as.character(PARAMS[i,]),sep = "_", collapse = "_"),"_1961-2013.nc"))
  
}

plot(SWE)
