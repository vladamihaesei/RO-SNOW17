library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
source("R/SNOW-17_point/time_builder.R")
source("R/SNOW-17_point/elev2press.R")
source("R/SNOW-17_point/meltfunction.R")
source("R/SNOW-17_point/RvsS.R")
source("R/SNOW-17_point/RvsS2.R")
source("R/SNOW-17_point/snowt.R")
source("R/SNOW-17_point/snowpack.R")
source("R/SNOW-17_point/snow17_function.R")

statii <- read.csv("~/D/2021/snow_clim/tavg_ws_inventory_selection.csv")
names(statii)[5] <- "COD"
statii$COD <- as.character(statii$COD)
cods <- unique(statii$COD)
nume <- unique(statii$NUME)
#tab <- read.csv(file = "tabs/Vf.Omu.csv")
tab <- readRDS("~/D/2023/Date_climatice/Statii/Zilnice/params_1961-2022.rds")%>%filter(COD %in% cods)
tab$DAT <- as.Date(tab$DAT)
tab <- tab%>%filter(year(DAT) > 2000 & year(DAT) < 2021)
tab.f <- tab%>%drop_na(c(TMED))%>%left_join(statii)%>%mutate_at(c("TMED","PRECIP","ECHIVZAP","GROSZ","DENSZAP"),as.numeric)
tab.f$PRECIP[is.na(tab.f$PRECIP)] <- 0

for(s in 1:length(cods)){
  
  print(nume[s])
  tab.f1 <- tab.f%>%filter(NUME == nume[s])

  if(nrow(tab.f1) == 0 ) next
  ##### convert to water year
  tt <- wateryear(tab.f1)
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
  PARAMS <- expand.grid(SCF = 1, UADJ = 0.02, MBASE = 0, MFMAX = 1.02, MFMIN = 0.6,TIPM = 0.2, NMF = 0.1, PLWHC = seq(0,2,.1),
                        PXTEMP = 1, PXTEMP1 = -1, PXTEMP2 = -3, Tt = 2.8, Tr = 13)
  
  nrow(PARAMS)
  #### the input parameters for snow densification
  PARAMS1 <- expand.grid(C1 = 0.026,C2 = 21.0,C3 = 0.005,C4 = 0.10,THRESD = 0.15,C5 = 2.0,CX = 23.0,
                         newSnowDenMin = 0.06792, newSnowDenMult = 0.05125, newSnowDenScal = 2.59, rhon0 = 0.1,
                         rho_max_grav = 0.35, rho_max_melt = 0.55,tau_half_max_dens = 10, B_dens_rate = 0.4,  
                         rho0 = 0.3, rmlt = 0.5, rcld = 0.3, trho = 200)
  
  #### new settings for rain vs snow and for snow compaction, metamorphism and densification
  sSETTINGS <- expand.grid(RvS = 0, mDEC_RHO_COMPACTION = 1, mDEC_RHO_DM_METAMORPH = 1, mDEC_RHO_NEW = 0, mDEC_RHO_METHOD = 0) 
  
  ### time steps- 24 for daily 
  dt <- 24
  ## Constants
  stefan <- 6.12 * (10^(-10))  # Stefan-Boltzman constant (mm/K/hr)
  ## Snow-17 Default Parameters and Settings
  
  df1 <- NULL
  
  for(i in 1:nrow(PARAMS)){
    
    #print(PARAMS[i,])
    df <- snow17c(sINPUTS = sINPUTS, sIC = sIC, PARAMS = PARAMS[i,],PARAMS1 = PARAMS1, sSETTINGS = sSETTINGS)
    tt$SWE <- df$SWE
    tt$SD <- df$Hs
    tt$Dens <- df$RHO
    tt$Rainfall <- df$RAIN
    tt$Snowfall <- df$SNOW
    tt$SD_new <- df$Hn
    tt$model <- str_c(as.character(PARAMS[i,]),sep = "_", collapse = "_")
    df1 <- rbind(df1,tt)
    
  }
  
  write.csv(df1, paste0("tabs_export/ensembles/PLWHC_",nume[s],".csv"))
  library(ggplot2)
  gg <- ggplot2::ggplot(df1)+
    geom_line(aes(x = DAT, y = SD, col = model), show.legend = F)+
    geom_point(aes(x = DAT, y = GROSZ),size = .5)
  
  png(paste0("png/ensembles/SD_PLWHC_",nume[s],".png"), width = 2400, height = 1600, res = 220)
  print(gg)
  dev.off()
  
  gg1 <- ggplot2::ggplot(df1)+
    geom_line(aes(x = DAT, y = SWE, col = model))+
    geom_point(aes(x = DAT, y = ECHIVZAP), size = .5)
  
  png(paste0("png/ensembles/SWE_PLWHC_",nume[s],".png"), width = 2400, height = 1600, res = 220)
  print(gg1)
  dev.off()
  
}
