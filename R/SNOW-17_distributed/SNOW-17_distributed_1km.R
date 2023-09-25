### distributed snow-17 
## the following script is written in R programming language based on the Mark Raleigh's MATLAB code of SNOW-17.
library(terra)
source("R/water_year.R")
source("R/SNOW-17_distributed/snow17_rasterfunction.R")

temp.ls <- list.files("~/Y/snowball/nc/observatii/tavg", full.names = T, pattern = ".nc")
dat <-  sort(as.numeric(substr(temp.ls,nchar(temp.ls) - 10,nchar(temp.ls) - 7)))
temp.ls <- temp.ls[dat >= 2018]
temp.ls <- wateryearlist(data = temp.ls)

prec.ls <- list.files("~/Y/snowball/nc/observatii/prec", full.names = T, pattern = ".nc")
dat <-  sort(as.numeric(substr(prec.ls,nchar(prec.ls) - 10,nchar(prec.ls) - 7)))
prec.ls <- prec.ls[dat >= 2018]
prec.ls <- wateryearlist(data = prec.ls)

elev <- as.array(readRDS("tabs/arrays/ro_elevation_1km.rds"))

SWE.r <- rast()
### calculate for each day
for( i in 1:length(temp.ls)){
  
  temp <- terra::rast(temp.ls[i])
  Temp <- as.array(temp)
  
  prec <- terra::rast(prec.ls[i])
  Prec <- as.array(prec)
 
  time(temp) <- as.Date(strsplit(temp.ls[i],"/|_|.nc")[[1]][10], format = "%Y%m%d")
  TIME <- as.array(time(temp))
  #TIME1 <- as.array(time(prec))
  #if(TIME != TIME1)
   # stop("The temperature and precipitation time doesn't match!")
  
  sINPUTS <- list(Temp = Temp, Prec = Prec, elev = elev, TIME = TIME)
  sIC <- list(ATI =0, W_q = 0, W_i = 0, Deficit = 0, SNDEN = 0, SNTMP = 0, SGMLOS = 0, SRFRZ = 0)
  sSettings  <- list(RvS = 0)
  PARAMS <- list(SCF = 1, UADJ = 0.04, MBASE = 1, MFMAX = 1.05, MFMIN = 0.60,TIPM = 0.1,NMF = 0.15, PLWHC = 0.04,
                 PXTEMP = 1, C1 = 0.026, C2 = 0.005, C3 = 0.005,C4 = 0.10, THRESD = 0.15, C5 = 2.0, CX = 23.0)
  
  s17 <- snow17dc(sINPUTS = sINPUTS, sSETTINGS = sSettings, sIC = sIC,sPARAMS = PARAMS)
  
  ### SWE
  SWE  <- terra::rast(s17$SWE)
  ext(SWE) <- ext(temp) 
  crs(SWE) <- "+init=epsg:3844"
  
  writeCDF(SWE,paste0("~/D/2023/nc/SWE/", "swe_", as.Date(TIME,format = "%Y%m%d"), ".nc"), overwrite = T)
  
  ### SD
  SD  <- terra::rast(s17$Hs)
  ext(SD) <- ext(temp) 
  crs(SD) <- "+init=epsg:3844"
  
  writeCDF(SD,paste0("~/D/2023/nc/SD/", "sd_", as.Date(TIME,format = "%Y%m%d"), ".nc"), overwrite = T)
  
}

SWE.r  <- terra::rast(s17$SWE)
ext(SWE.r) <- ext(temp) 
crs(SWE.r) <- "+init=epsg:3844"

SD.r  <- terra::rast(s17$Hs)
ext(SD.r) <- ext(temp) 
crs(SD.r) <- "+init=epsg:3844"


Sfall.r  <- terra::rast(s17$Sfall)
ext(Sfall.r) <- ext(temp) 
crs(Sfall.r) <- "+init=epsg:3844"
plot(Sfall.r)


