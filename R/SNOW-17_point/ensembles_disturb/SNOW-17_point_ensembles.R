library(dplyr)
library(lubridate)
library(stringr)

source("R/SNOW-17_point/time_builder.R")
source("R/SNOW-17_point/elev2press.R")
source("R/SNOW-17_point/meltfunction.R")
source("R/SNOW-17_point/RvsS.R")
source("R/SNOW-17_point/RvsS2.R")
source("R/SNOW-17_point/snowt.R")
source("R/SNOW-17_point/snowpack.R")
source("R/water_year.R")
source("R/SNOW-17_point/snow17_function.R")

tab <- readRDS("tabs/daily_vars_snow17_2001-2021.rds")
tab <- wateryear(tab)
nume <- unique(tab$NUME)

for(s in 46:length(nume)){
  
  print(nume[s])
  tt <- tab%>%filter(NUME == nume[s])
  
  if(nrow(tt) == 0 ) next
  ##### convert to water year
  
  Temp <- array(tt$TMED, dim = c(nrow(tt), 1))
  Prec <- array(tt$PRECIP, dim = c(nrow(tt), 1))
  elev <- array(tt$Z)
  TIME <- time_builder(time = tt$DAT)
  
  ### the inputs for the model run
  sINPUTS <- list(Prec = Prec, Temp = Temp, elev = elev, TIME = TIME)
  ### the initial conditions, no snow at the time step the model starts running
  sIC <- list(ATI = 0,  W_q = 0, W_i = 0,  Deficit = 0,  
              SNDEN = 0,SNTMP = 0, SRFRZ = 0, SGMLOS = 0)
  ### the input parameters for the model
  PARAMS <- expand.grid(SCF = seq(0.4,1.8,.1), UADJ = 0.04, MBASE = seq(-.4,1.4,.2), MFMAX = 1.05, MFMIN = 0.60,TIPM = 0.2, NMF = 0.1, PLWHC = 0.04,
                        PXTEMP = seq(-1.5,2,.5), PXTEMP1 = -1, PXTEMP2 = -3, Tt = 2.8, Tr = 13)
  print(nrow(PARAMS))
  #### the input parameters for snow densification
  PARAMS1 <- expand.grid(C1 = 0.026,C2 = 21.0,C3 = 0.005,C4 = 0.10,THRESD = 0.15,C5 = 2.0,CX = 23.0,
                         newSnowDenMin = 0.06792, newSnowDenMult = 0.05125, newSnowDenScal = 2.59, rhon0 = 0.1,
                         rho_max_grav = 0.35, rho_max_melt = 0.55,tau_half_max_dens = 10, B_dens_rate = 0.4,  
                         rho0 = 0.3, rmlt = 0.5, rcld = 0.3, trho = 200)
  
  #### new settings for rain vs snow and for snow compaction, metamorphism and densification
  sSETTINGS <- list(RvS = 0, mDEC_RHO_COMPACTION = 1, mDEC_RHO_DM_METAMORPH = 1, mDEC_RHO_NEW = 0, mDEC_RHO_METHOD = 0) 
  
  ### time steps- 24 for daily 
  dt <- 24
  
  ## Constants
  stefan <- 6.12 * (10^(-10))  # Stefan-Boltzman constant (mm/K/hr)
  df1 <- NULL
  
  for(i in 1:nrow(PARAMS)){
    
    print(PARAMS[i,])
    df <- snow17c(sINPUTS = sINPUTS, sIC = sIC, PARAMS = PARAMS[i,],PARAMS1 = PARAMS1, sSETTINGS = sSETTINGS)
    tt$SWE <- df$SWE
    tt$SD <- df$Hs
    tt$Dens <- df$RHO
    tt$Rain <- df$RAIN
    tt$Snow <- df$SNOW
    tt$model <- str_c(as.character(PARAMS[i,]),sep = "_", collapse = "_")
    df1 <- rbind(df1,tt)
  }
  
  saveRDS(df1, paste0("tabs_export/",nume[s],".rds"))
  gc()
}

##### verificare 

tt <- readRDS("tabs_export/Barlad.rds")
head(tt)

