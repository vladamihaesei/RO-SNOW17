library(dplyr)
library(lubridate)

source("R/SNOW-17_point/time_builder.R")
source("R/SNOW-17_point/elev2press.R")
source("R/SNOW-17_point/meltfunction.R")
source("R/SNOW-17_point/RvsS.R")
source("R/SNOW-17_point/RvsS2.R")
source("R/SNOW-17_point/snowt.R")
source("R/SNOW-17_point/snowpack.R")
source("R/water_year.R")
source("R/SNOW-17_point/snow17_function.R")

tab <- readRDS(file = "tabs/stations/Vf. Omu_snow17_vars_2001-2020.rds")

## calculate for water year ( year starts on August first and ends 31 July)
tab <- wateryear(tab)

#### tranform into array
Temp <- array(tab$TMED, dim = c(nrow(tab), 1))
Prec <- array(tab$PRECIP, dim = c(nrow(tab), 1))
elev <- array(tab$Z)
TIME <- time_builder(time = tab$DAT)

### the inputs for the model run
sINPUTS <- list(Prec = Prec, Temp = Temp, elev = elev, TIME = TIME)
### the initial conditions, now snow at the time step the model starts running
sIC <- list(ATI = 0,  W_q = 0, W_i = 0,  Deficit = 0,  
            SNDEN = 0,SNTMP = 0, SRFRZ = 0, SGMLOS = 0 )

### the input parameters for the model
PARAMS <- list(SCF = 1, UADJ = 0.04, MBASE = 0, MFMAX = 1.05, MFMIN = 0.60, TIPM = 0.1, NMF = 0.15, PLWHC = 0.04,
               PXTEMP = 1, PXTEMP1 = -1, PXTEMP2 = -3, Tt = 2.8, Tr = 13)

#### the input parameters for snow densification
PARAMS1 <- list(C1 = 0.026,C2 = 21.0,C3 = 0.005,C4 = 0.10,THRESD = 0.15,C5 = 2.0,CX = 23.0,
                newSnowDenMin = 0.06792, newSnowDenMult = 0.05125, newSnowDenScal = 2.59, rhon0 = 0.1,
                rho_max_grav = 0.35, rho_max_melt = 0.55,tau_half_max_dens = 10, B_dens_rate = 0.4,  
                rho0 = 0.3, rmlt = 0.5, rcld = 0.3, trho = 200)

#### new settings for rain vs snow and for snow compaction, metamorphism and densification
sSETTINGS <- list(RvS = 1, mDEC_RHO_COMPACTION = 1, mDEC_RHO_DM_METAMORPH = 1, mDEC_RHO_NEW = 0, mDEC_RHO_METHOD = 0) 
##time step
dt = 24


tab <- readRDS("tabs/daily_vars_snow17_2001-2021.rds")
tab <- wateryear(tab)
nume <- unique(tab$NUME)

out <- snow17c(sINPUTS = sINPUTS, sIC = sIC, sSETTINGS = sSETTINGS, PARAMS = PARAMS, PARAMS1 = PARAMS1)

#### put the some outputs  
tab$SWE <- out$SWE
tab$SD <- out$Hs
tab$RHO <- out$RHO

#### plot the outputs (SWE and SD)
library(ggplot2)
gg <- ggplot(tab)+
       geom_line(mapping = aes(x= as.Date(DAT), y = GROSZ), color = "black")+
       geom_line(mapping = aes(x = as.Date(DAT), y = SD), color  = "red")
gg
gg1 <-  ggplot(tab)+
  geom_point(mapping = aes(x= as.Date(DAT), y = ECHIVZAP), color = "black")+
  geom_line(mapping = aes(x = as.Date(DAT), y = SWE), color  = "red")

gg1
