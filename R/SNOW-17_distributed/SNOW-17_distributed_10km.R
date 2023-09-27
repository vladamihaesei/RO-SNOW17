### distributed snow-17 

## the following script is written in R programming language based on the Mark Raleigh MATLAB code of SNOW-17.
source("R/SNOW-17_distributed/snow17_rasterfunction.R")

#### read the inputs
Temp <- readRDS("inputs/grids/tair_2010-2013_10_km_wy.rds")
Prec <- readRDS("inputs/grids/precip_2010-2013_10km_wy.rds")
TIME <- readRDS("inputs/grids/timp_2010-2013_wy.rds")
elev <- readRDS("inputs/grids/ro_elevation_10km.rds")

sINPUTS <- list(Temp = Temp, Prec = Prec, elev = elev, TIME = TIME)
sIC <- list(ATI =0, W_q = 0, W_i = 0, Deficit = 0, SNDEN = 0, SNTMP = 0, SGMLOS = 0, SRFRZ = 0)
sSettings  <- list(RvS = 0)
PARAMS <- list(SCF = 1, UADJ = 0.04, MBASE = 1, MFMAX = 1.05, MFMIN = 0.60,TIPM = 0.1,NMF = 0.15, PLWHC = 0.04,
               PXTEMP = 1, C1 = 0.026, C2 = 0.005, C3 = 0.005,C4 = 0.10, THRESD = 0.15, C5 = 2.0, CX = 23.0)

snow17_10km <- snow17dc(sINPUTS = sINPUTS,sIC = sIC, sSETTINGS = sSettings,sPARAMS = PARAMS)
## required for raster projection
rs <- terra::rast("inputs/grids/tair_1961-01-01_10km.nc")

###SWE
SWE  <- terra::rast(snow17_10km$SWE)
ext(SWE) <- ext(rs) 
crs(SWE) <- "+init=epsg:4326"
#writeCDF(SWE, paste0("~/D/2023/nc/10km/SWE/swe_2010-2013.nc"))
time(SWE) <- TIME
SWE.f <- SWE[[which(year(time(SWE)) == 2011)]]
SWE.f <- mean(SWE.f)

###SD
SD  <- terra::rast(snow17_10km$Hs)
ext(SD) <- ext(rs) 
crs(SD) <- "+init=epsg:4326"
#writeCDF(SD, paste0("~/D/2023/nc/10km/SD/sd_", str_c(as.character(PARAMS[p,]),sep = "_", collapse = "_"),"_1961-2013.nc"))
time(SD) <- TIME
SD.f <- SD[[which(year(time(SD)) == 2011)]]
SD.f <- mean(SD.f, na.rm = T)
plot(SD.f)

Sfall  <- terra::rast(s17_1km$Sfall)
ext(Sfall) <- ext(rs) 
crs(Sfall) <- "+init=epsg:4326"
writeCDF(Sfall, paste0("~/D/2023/nc/10km/Sfall/sfall_", str_c(as.character(PARAMS[i,]),sep = "_", collapse = "_"),"_1961-2013.nc"))



plot(SWE)
