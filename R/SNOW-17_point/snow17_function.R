snow17c <- function(sINPUTS, sIC, sSETTINGS, PARAMS, PARAMS1) {
  ## Checking whether or not  is all properly set  
  
  if (nrow(sINPUTS$Temp) != nrow(sINPUTS$Prec) || nrow(sINPUTS$Temp) != nrow(sINPUTS$TIME)) {
    stop("Error - temperature, precip, and time inputs must have the same number of rows")
  }
  
  if (max(is.na(sINPUTS$Temp)) || max(is.na(sINPUTS$Prec))) {
    stop("NaN values detected in Temp or Prec data. Can't run the code")
  }
  
  #if(as.numeric(sINPUTS$Temp) == F || as.numeric(sINPUTS$Prec) == F){
   # stop("non numeric values for Precip and Temp")
    
  #}
  ## Constants
  
  stefan <- 6.12 * (10^(-10))  # Stefan-Boltzman constant (mm/K/hr)
  
  ## Snow-17 Default Parameters and Settings
  
  ### will be overwritten in the next section if specified in inputs
  
  
  ### traditional
  SCF <- PARAMS$SCF
  UADJ <- PARAMS$UADJ  # average wind function during rain on snow (mm/mb) - 0.04 value for the American River Basin from Shamir & Georgakakos 2007
  MBASE <- PARAMS$MBASE  # base temperature above which melt typically occurs (deg C) - ****must be greater than 0 deg C****- value of 1 for the American River Basin from Shamir & Georgakakos 2007
  MFMAX <- PARAMS$MFMAX # maximum melt factor during non-rain periods (mm/deg C 6 hr) - in western-facing slope assumed to occur on June 21 - value of 1.05 for the American River Basin from Shamir & Georgakakos 2007
  MFMIN <- PARAMS$MFMIN  # minimum melt factor during non-rain periods (mm/deg C 6 hr) - in western-facing slope assumed to occur on December 21 - value of 0.60 for the American River Basin from Shamir & Georgakakos 2007
  TIPM <- PARAMS$TIPM  # model parameter (>0.0 and <1.0) - Anderson Manual recommends 0.1 to 0.2 for deep snowpack areas
  NMF <- PARAMS$NMF # maximum negative melt factor (mm/deg C 6 hr) - value of 0.15 for the American River Basin from Shamir & Georgakakos 2007
  PLWHC <- PARAMS$PLWHC # percent liquid water holding capacity of the snowpack - max is 0.4 - value of 0.04 for the American River Basin from Shamir & Georgakakos 2007
  
  # Used if RvS = 0
  PXTEMP <- PARAMS$PXTEMP  # Temperature dividing rain from snow, deg C - if temp is less than or equal to PXTEMP, all precip is snow. Otherwise, it is rain.
  
  # Used if RvS = 1
  PXTEMP1 <- PARAMS$PXTEMP1  # Lower Limit Temperature dividing transition from snow, deg C - if temp is less than or equal to PXTEMP1, all precip is snow. Otherwise, it is mixed linearly.
  PXTEMP2 <- PARAMS$PXTEMP2  # Upper Limit Temperature dividing rain from transition, deg C - if temp is greater than or equal to PXTEMP2, all precip is rain. Otherwise, it is mixed linearly.
  
  # Used if RvS = 3
  Tt <- PARAMS$Tt
  Tr <- PARAMS$Tr
  
  # default settings
  RvS <- sSETTINGS$RvS
  mDEC_RHO_COMPACTION <- sSETTINGS$mDEC_RHO_COMPACTION
  mDEC_RHO_DM_METAMORPH <- sSETTINGS$mDEC_RHO_DM_METAMORPH
  mDEC_RHO_NEW <- sSETTINGS$mDEC_RHO_NEW
  mDEC_RHO_METHOD <- sSETTINGS$mDEC_RHO_METHOD
  
  # default parameters for snow density / compaction routines
  C1 <- PARAMS1$C1
  C2 <- PARAMS1$C2
  C3 <- PARAMS1$C3
  C4 <- PARAMS1$C4
  THRESD <- PARAMS1$THRESD
  C5 <- PARAMS1$C5
  CX <- PARAMS1$CX
  
  newSnowDenMin <- PARAMS1$newSnowDenMin
  newSnowDenMult <- PARAMS1$newSnowDenMult
  newSnowDenScal <- PARAMS1$newSnowDenScal
  rhon0 <- PARAMS1$rho0
  
  # snobal density params
  rho_max_grav <- PARAMS1$rho_max_grav 
  rho_max_melt <- PARAMS1$rho_max_melt
  tau_half_max_dens <- PARAMS1$tau_half_max_dens 
  B_dens_rate <- PARAMS1$B_dens_rate
  
  rho0 <- PARAMS1$rho0
  rmlt <- PARAMS1$rmlt
  rcld <- PARAMS1$rcld
  trho <- PARAMS1$trho  # compaction time scale (hr)
  
  # default initial conditions
  ATI <- sIC$ATI  # Antecedent Temperature Index, deg C
  W_q <- sIC$W_q  # Liquid water held by the snow (mm)
  W_i <- sIC$W_i  # W_i = accumulated water equivalent of the ice portion of the snow cover (mm)
  Deficit <- sIC$Deficit  # Heat Deficit, also known as NEGHS, Negative Heat Storage
  SNDEN <- sIC$SNDEN
  SNTMP <- sIC$SNTMP
  SRFRZ <- sIC$SRFRZ
  
  # misc
  SGMLOS <- sIC$SGMLOS  # ground melt, mm/dt (constant per time step)
  
  ### convert snow density from kg/m3 to g/cm3
  SNDEN <- SNDEN / 1000
  
  ### calculate snow depth initial conditions from SWE and density
  SNDPT <- ((W_q + W_i) / 10) * SNDEN
  
  ### save snow densification parameters and settings to a list
  SPACK <- list(
    C1 = C1,
    C2 = C2,
    C3 = C3,
    C4 = C4,
    THRESD = THRESD,
    C5 = C5,
    CX = CX,
    mDEC_RHO_COMPACTION = mDEC_RHO_COMPACTION,
    mDEC_RHO_DM_METAMORPH = mDEC_RHO_DM_METAMORPH
  )
  
  ## Initialization
  
  #L <- length(sINPUTS$TIME)  # number of time steps
  L <- nrow(TIME)  # number of time steps
  
  SNOWout <- list()
  SNOWout$TIME <- TIME
  SNOWout$SWE <- rep(NA, L)
  SNOWout$Hs <- rep(NA, L)
  SNOWout$RHO <- rep(NA, L)
  SNOWout$SNOW <- rep(NA, L)
  SNOWout$RAIN <- rep(NA, L)
  SNOWout$Hn <- rep(NA, L)
  SNOWout$RHOn <- rep(NA, L)
  SNOWout$SWEq <- rep(NA, L)
  SNOWout$SWEi <- rep(NA, L)
  SNOWout$M_nr <- rep(NA, L)
  SNOWout$M_ros <- rep(NA, L)
  SNOWout$MF <- rep(NA, L)
  SNOWout$SRFRZ <- rep(NA, L)
  SNOWout$ATI <- rep(NA, L)
  SNOWout$T_pack <- rep(NA, L)
  SNOWout$T_snow <- rep(NA, L)
  SNOWout$T_rain <- rep(NA, L)
  SNOWout$T_prcp <- rep(NA, L)
  SNOWout$Deficit <- rep(NA, L)
  SNOWout$outflow <- rep(NA, L)
  
  ## Calculations outside of loop
  
  ### Calculate atm. pressure (mb) based on sta_elev
  P_atm <- elev2press(elev)
  
  TIPM_dtt <- 1.0 - ((1.0 - TIPM)^(dt/6))
  
  # Divide Rain and Snow at all time steps
  if (RvS == 0) {
    result <- RvsS(Temp, Prec, RvS, PXTEMP)
    Rfall <- result$Rfall
    Sfall <- result$Sfall
  } else if (RvS == 1) {
    result <- RvsS(Temp, Prec, RvS, PXTEMP1, PXTEMP2)
    Rfall <- result$Rfall
    Sfall <- result$Sfall
  } else if (RvS == 2) {
    result <- RvsS(Temp, Prec, RvS)
    Rfall <- result$Rfall
    Sfall <- result$Sfall
  } else if (RvS == 3) {
    result <- RvsS2(TIME, Temp, Prec, Tt, Tr)
    Rfall <- result$Rfall
    Sfall <- result$Sfall
  } else {
    stop('Invalid rain vs snow option')
  }
  
  SNOWout$SNOW <- Sfall
  SNOWout$RAIN <- Rfall
  
  # calculate fraction of rain and snow at each time step
  fracrain <- Rfall / Prec
  fracsnow <- Sfall / Prec
  
  # set fracrain and fracsnow to 0 at any timesteps with 0 precip
  fracrain[Prec == 0] <- 0
  fracsnow[Prec == 0] <- 0
  
  # new snowfall density and depth of new snow
  if (mDEC_RHO_NEW == 0) {
    # ANDERSON METHOD
    Tpp <- Temp
    Tpp[Tpp > 0] <- 0
    SNOWout$RHOn[Tpp <= -15] <- 0.05  # g/cm3
    SNOWout$RHOn[Tpp > -15] <- 0.05 + 0.0017 * ((Tpp[Tpp > -15] + 15)^1.5)
  } else if (mDEC_RHO_NEW == 1) {
    # HEDSTROM AND POMEROY METHOD
    SNOWout$RHOn <- newSnowDenMin + newSnowDenMult * exp(T / newSnowDenScal)
    SNOWout$RHOn[SNOWout$RHOn > 0.15] <- 0.15
  } else if (mDEC_RHO_NEW == 2) {
    # CONSTANT VALUE
    SNOWout$RHOn <- rep(rhon0, L)
  }
  
  SNOWout$Hn <- (Sfall * SCF / 10) / SNOWout$RHOn  # thickness of new snow (cm)
  
  if (mDEC_RHO_METHOD == 1) {
    dt_tau <- dt / trho
  }
  
  # create two melt factor (Mf) matrices (MxNx365 or 366) as function of day-of-year and whether it is a leap year
  Mf_year <- rep(NA, 365)
  Mf_leap <- rep(NA, 366)
  
  for (j in 1:366) {
    if (j < 366) {
      
      Mf_year[j] <- meltfunction("2003-01-01", j, dt, MFMAX, MFMIN)  # arbitrary normal year (2003)
    }
    
    Mf_leap[j] <- meltfunction("2004-01-01", j, dt, MFMAX, MFMIN)  # arbitrary leap year (2004)
  }
  
  # set initial flag for ripeness (not ripe)
  flag_ripe <- 3
  
  # Model Execution
  for (i in 1:L) {
    # store previous conditions
    p_ATI <- ATI
    p_Wq <- W_q
    p_Wi <- W_i
    p_Deficit <- Deficit
    
    # get melt factor for today
    if (leap_year(TIME[i, 5]) == T) {
      Mf <- Mf_leap
    } else {
      Mf <- Mf_year
    }
    
    Mf <- Mf[as.numeric(TIME[i,4])]
    
    T_air_meanC <- Temp[i]  # air temperature at this time step (deg C)
    precip <- Prec[i]  # precipitation at this time step (mm)
    
    # Snow Accumulation
    Pn <- precip * fracsnow[i] * SCF  # water equivalent of new snowfall (mm)
    SFALL <- Pn
    W_i <- W_i + Pn  # W_i = accumulated water equivalent of the ice portion of the snow cover (mm)
    E <- 0
    RAIN <- fracrain[i] * precip  # amount of precip (mm) that is rain during this time step
    
    # Temperature and Heat Deficit from new Snow
    if (T_air_meanC < 0) {
      T_snow_new <- T_air_meanC
      delta_HD_snow <- -(T_snow_new * Pn) / (80 / 0.5)  # delta_HD_snow = change in the heat deficit due to snowfall (mm)
      T_rain <- PXTEMP
    } else {
      T_snow_new <- 0
      delta_HD_snow <- 0
      T_rain <- T_air_meanC
    }
    
    # take TSNEW as weighted avg of new snowfall temp and rainfall temp
    TSNEW <- (T_rain * RAIN + T_snow_new * Pn) / (Pn + RAIN)
    
    if (Pn + RAIN == 0) {
      TSNEW <- 0
    }
    
    # Antecedent temperature Index
    if (Pn > (1.5 * dt)) {
      ATI <- T_snow_new
    } else {
      ATI <- ATI + TIPM_dtt * (T_air_meanC - ATI)  # Antecedent temperature index
    }
    
    if (ATI > 0) {
      ATI <- 0
    }
    
    # Heat Exchange when no Surface Melt
    delta_HD_T <- NMF * (dt / 6) * (Mf / MFMAX) * (ATI - T_snow_new)  # delta_HD_T = change in heat deficit due to a temperature gradient (mm)
    
    # Rain-on-Snow Melt
    if (RvS == 2) {
      # do not compute rain-on-snow melt if all precip is considered snowfall
      M_RoS <- 0
    } else {
      e_sat <- 2.7489 * (10^8) * exp((-4278.63 / (T_air_meanC + 242.792)))  # saturated vapor pressure at T_air_meanC (mb)
      
      if (RAIN > (0.25 * dt)) {  # 1.5 mm/6 hrs
        # Melt (mm) during rain-on-snow periods is:
        M_RoS1 <- max(stefan * dt * (((T_air_meanC + 273)^4) - (273^4)), 0)
        M_RoS2 <- max((0.0125 * RAIN * T_rain), 0)
        M_RoS3 <- max((8.5 * UADJ * (dt / 6) * (((0.9 * e_sat) - 6.11) + (0.00057 * P_atm * T_air_meanC))), 0)
        M_RoS <- M_RoS1 + M_RoS2 + M_RoS3
      } else {
        M_RoS <- 0
      }
    }
    # Non-Rain Melt
    if (RAIN <= (0.25 * dt) && (T_air_meanC > MBASE)) {
      # Melt during non-rain periods is:
      M_NR <- (Mf * (T_air_meanC - MBASE) * (dt / dt)) + (0.0125 * RAIN * T_rain)
    } else {
      M_NR <- 0
    }
    
    # Ripeness of the snow cover
    Melt <- M_RoS + M_NR
    
    if (Melt <= 0) {
      Melt <- 0
    }
    
    if (Melt < W_i) {
      W_i <- W_i - Melt
    } else {
      Melt <- W_i + W_q
      W_i <- 0
    }
    
    Qw <- Melt + RAIN  # Qw = liquid water available melted/rained at the snow surface (mm)
    W_qx <- PLWHC * W_i  # W_qx = liquid water capacity (mm)
    Deficit <- Deficit + delta_HD_snow + delta_HD_T  # Deficit = heat deficit (mm)
    
    if (Deficit < 0) {  # limits of heat deficit
      Deficit <- 0
    } else if (Deficit > 0.33 * W_i) {
      Deficit <- 0.33 * W_i
    }
    
    # In SNOW-17 the snow cover is ripe when both (Deficit=0) & (W_q = W_qx)
    if (W_i > 0) {
      if ((Qw + W_q) > ((Deficit * (1 + PLWHC)) + W_qx)) {  # THEN the snow is RIPE
        E <- Qw + W_q - W_qx - (Deficit * (1 + PLWHC))  # Excess liquid water (mm)
        W_q <- W_qx  # fills liquid water capacity
        W_i <- W_i + Deficit  # W_i increases because water refreezes as heat deficit is decreased
        SRFRZ <- Deficit  # refrozen water
        Deficit <- 0
        flag_ripe <- 1
      } else if ((Qw >= Deficit)) {  # & ((Qw + W_q) <= ((Deficit*(1+PLWHC)) + W_qx))  # THEN the snow is NOT yet ripe, but ice is being melted
        E <- 0
        W_q <- W_q + Qw - Deficit
        W_i <- W_i + Deficit  # W_i increases because water refreezes as heat deficit is decreased
        SRFRZ <- Deficit  # refrozen water
        Deficit <- 0
        flag_ripe <- 2
      } else if ((Qw < Deficit)) {  # elseif ((Qw + W_q) < Deficit)  # THEN the snow is NOT yet ripe
        E <- 0
        W_i <- W_i + Qw  # W_i increases because water refreezes as heat deficit is decreased
        SRFRZ <- Qw  # refrozen water
        Deficit <- Deficit - Qw
        flag_ripe <- 3
      }
      
      SWE <- W_i + W_q  # + E
    } else {
      # then no snow exists!
      E <- Qw
      SWE <- 0
      W_q <- 0
    }
    
    if (Deficit == 0) {
      ATI <- 0
    }
    
    # Depth and density module
    ##########################
    ##########################
    
    # Save a copy of bulk snow density
    SNDEN_copy <- SNDEN
    
    if (mDEC_RHO_METHOD == 0 || mDEC_RHO_METHOD == 4) {
      # Need snow temp for method 4... so running this part
      
      # Copy variables
      SMELT <- Melt
      WE <- SWE
      SLIQ <- W_q
      
      # Change in temp
      if (i == 1) {
        DTA <- 0
      } else {
        DTA <- Temp[i] - Temp[i - 1]
        if (Temp[i - 1] > 0 && Temp[i] > 0) {
          DTA <- abs(DTA)
        } else if (Temp[i - 1] > 0 && Temp[i] < 0) {
          DTA <- Temp[i]
        }
      }
      
      if (p_Wi == 0) {
        # no existing snowpack at start of time step
        
        if (SFALL > 0) {
          # then it snowed during the time step
          DENNEW <- SNOWout$RHOn[i]
          SFALLX <- SFALL - SMELT - SGMLOS
          if (SRFRZ > 0) {
            # account for any refreeze of new snowfall - increases density - recompute depth
            DCM <- (SFALLX + SRFRZ) / SFALLX
            DENNEW <- DCM * DENNEW
            DPTNEW <- (0.1 * WE) / DENNEW
          } else {
            DPTNEW <- SNOWout$Hn[i]
          }
          
          SNDPT <- DPTNEW
          SNDEN <- DENNEW
          SNTMP <- min(0, TSNEW)
        }
      } else {
        # compute net melt during period
        PLOSS <- SMELT + SGMLOS
        WE1 <- WE - SFALL + PLOSS - SRFRZ
        
        # cases
        if (PLOSS < WE1) {
          # amount of melt is less than existing snow at start of period
          # assume all melt came from existing snow - typical case
          
          # check for any new snowfall during period
          if (SFALL > 0) {
            #%% call SNEW
            DPTNEW <- SNOWout$Hn[i]
            DENNEW <- SNOWout$RHOn[i]
          } else {
            DPTNEW <- 0
            DENNEW <- 0
          }
          
          # COMPUTE WATER EQUIVALENT OF EXISTING SNOW AT END OF PERIOD
          # - PRIOR TO ANY REFREEZE
          WEX <- WE1 - PLOSS
          
          # COMPUTE DEPTH OF REMAINING OLD SNOW - ASSUME DENSITY OF
          # MELTED SNOW WAS THE SAME AS THE AVERAGE DENSITY OF THE
          # TOTAL OLD SNOW COVER
          DPTOLD <- (0.1 * WEX) / SNDEN
          
          # USE DENSITY OF COMBINED OLD AND NEW SNOW TO COMPUTE THE
          # THERMAL CONDUCTIVITY TO USE FOR CALCULATING THE CHANGE
          # IN THE TEMPERATURE OF THE SNOW COVER.
          TSNDPT <- DPTOLD + DPTNEW
          TSNDEN <- (DPTOLD * SNDEN + DPTNEW * DENNEW) / TSNDPT
          
          # COMPUTE NEW TEMPERATURE OF THE OLD SNOW
          SNTMP <- SNOWT(DPTNEW, TSNDEN, TSNDPT, WE, SLIQ, SNTMP, DTA, dt)
          
          # COMPUTE NEW DEPTH AND DENSITY OF OLD SNOW AFTER EFFECT OF
          # COMPACTION AND DESTRUCTIVE METAMORPHISM ARE TAKEN INTO
          # ACCOUNT.
          if (mDEC_RHO_COMPACTION == 1 || mDEC_RHO_DM_METAMORPH == 1) {
            results1 <- SNOWPACK(SNDEN, WE, SLIQ, SNTMP, dt, SPACK)
            SNDEN <- results1$SNDEN
            SNDPT <- results1$SNDPT
          } else {
            # no densification rountine enabled. average new and old rho
            SNDEN <- (SNDEN * SNDPT + DPTNEW + DENNEW) / (DPTNEW + SNDPT)
            SNDPT <- DPTNEW + SNDPT
          }
        } else {
          SFALLX <- SFALL - PLOSS
          
          if (SFALLX > 0) {
            # MELT DURING THE PERIOD WAS GREATER THAN THE AMOUNT OF SNOW
            # AT THE START OF THE PERIOD AND NEW SNOWFALL EXCEEDS THE
            # AMOUNT OF MELT DURING THE PERIOD - ASSUME MELT COMES FROM
            # NEW SNOWFALL
            
            # GET DEPTH AND DENSITY OF NEW SNOW
            #%% call SNEW
            DPTNEW <- SNOWout$Hn[i]
            DENNEW <- SNOWout$RHOn[i]
            
            # USE DENSITY OF COMBINED OLD AND NEW SNOW TO COMPUTE THE
            # THERMAL CONDUCTIVITY TO USE FOR CALCULATING THE CHANGE
            # IN THE TEMPERATURE OF THE SNOW COVER.
            TSNDPT <- SNDPT + DPTNEW
            TSNDEN <- (SNDPT * SNDEN + DPTNEW * DENNEW) / TSNDPT
            
            # COMPUTE NEW TEMPERATURE OF THE OLD SNOW
            SNTMP <- SNOWT(DPTNEW, TSNDEN, TSNDPT, WE, SLIQ, SNTMP, DTA, dt)
            
            # COMPUTE NEW DEPTH AND DENSITY OF OLD SNOW AFTER EFFECT OF
            # COMPACTION AND DESTRUCTIVE METAMORPHISM ARE TAKEN INTO
            # ACCOUNT.
            if (mDEC_RHO_COMPACTION == 1 || mDEC_RHO_DM_METAMORPH == 1) {
              result1<- SNOWPACK(SNDEN, WE, SLIQ, SNTMP, dt, SPACK)
              SNDEN <- result1$SNDEN
              SNDPT <- result1$SNDPT
            } else {
              # no densification rountine enabled. average new and old rho
              SNDEN <- (SNDEN * SNDPT + DPTNEW + DENNEW) / (DPTNEW + SNDPT)
              SNDPT <- DPTNEW + SNDPT
            }
          } else {
            DPTNEW <- 0
            
            # SFALLX IS .LE. 0.0, THUS MELT EXCEEDS BOTH EXISTING SNOW
            # AT THE START OF THE PERIOD AND THE NEW SNOWFALL, BUT NOT
            # THE TOTAL OF THE TWO QUANTITIES. ASSUME ALL NEW SNOW
            # MELTS AND ONLY PART OF THE EXISTING PACK REMAINS.
            WEX <- WE - SRFRZ
            
            # DEPTH OF EXISTING SNOW THAT REMAINS
            TSNDPT <- (0.1 * WEX) / SNDEN
            
            # COMPUTE NEW TEMPERATURE OF THE REMAINING OLD SNOW
            SNTMP <- SNOWT(DPTNEW, SNDEN, TSNDPT, WE, SLIQ, SNTMP, DTA, dt)
            
            # COMPUTE NEW DEPTH AND DENSITY OF REMAINING OLD SNOW AFTER
            # EFFECT OF COMPACTION AND DESTRUCTIVE METAMORPHISM ARE
            # TAKEN INTO ACCOUNT.
            if (mDEC_RHO_COMPACTION == 1 || mDEC_RHO_DM_METAMORPH == 1) {
              results1 <- SNOWPACK(SNDEN, WE, SLIQ, SNTMP, dt, SPACK)
              SNDEN <- results1$SNDEN
              SNDPT <- results1$SNDPT
            } else {
              # no densification rountine enabled. average new and old rho
              SNDEN <- (SNDEN * SNDPT + DPTNEW + DENNEW) / (DPTNEW + SNDPT)
              SNDPT <- DPTNEW + SNDPT
              
            }
          }
        }
        # IN ALL CASES WHEN SNOW EXISTED AT THE START OF THE PERIOD
        # COMPUTE NEW TOTAL DEPTH AND TEMPERATURE BY WEIGHING BOTH
        # THE OLD AND NEW SNOW.
        if (TSNEW > 0) {
          TSNEW <- 0
        }
        
        SNTMP <- (SNTMP*SNDPT+TSNEW*DPTNEW)/(SNDPT+DPTNEW)
        SNDPT <- SNDPT+DPTNEW
        SNDEN <- (0.1*(WE-SRFRZ))/SNDPT
        # COMPUTE EFFECT OF MELT METAMORPHISM - REFREEZING OF LIQUID
        # WATER WITHIN THE SNOW COVER.
        if (SRFRZ != 0) {
          # COMPUTE INCREASE IN DENSITY DUE TO REFREEZING
          DCM <- WE / (WE - SRFRZ)
          SNDEN <- SNDEN * DCM
        }
        
      }
      
    }else if(mDEC_RHO_METHOD==1){
      #%%% time-density curve (From Essery's FSM, 2015 paper)
      if (p_Wi == 0) {
        # no existing snowpack at start of time step
        if (SFALL > 0) {
          SNDEN <- SNOWout$RHOn[i]
        }
      } else {
        if (flag_ripe <= 2) {
          #%% then snowpack is in melt state. use density curve for
          #%% melting case
          if (SNDEN < rmlt) {
            SNDEN <- rmlt + (SNDEN - rmlt) * exp(-1 * dt_tau)
          }
        } else {
          #%% use density curve for cold case
          if (SNDEN < rcld) {
            SNDEN <- rcld + (SNDEN - rcld) * exp(-1 * dt_tau)
          }
        }
        
        #%% if new snow, need to weight bulk density based on old vs. new
        if (SFALL > 0) {
          SWE_ex <- max(SWE - SFALL, 0)
          SF2 <- SWE - SWE_ex
          
          SNDEN <- ((SNDEN * SWE_ex) + (SNOWout$RHOn[i] * SF2)) / (SWE_ex + SF2)
        }
      }
      
    }else if(mDEC_RHO_METHOD==2){
      
      if (p_Wi == 0) {
        # no existing snowpack at start of time step
        if (SFALL > 0) {
          SNDEN <- SNOWout$RHOn[i]
        }
      } else {
        
        #%% compact with time
        if (SNDEN < rho_max_grav) {
          time_solve <- (tau_half_max_dens * 24) / ((rho_max_grav / SNDEN) - 1)  # in hours
          SNDEN <- rho_max_grav / (1 + (24 * tau_half_max_dens) / (time_solve + dt))
        }
        
        #%% if new snow, need to weight bulk density based on old vs. new
        if (SFALL > 0) {
          SWE_ex <- max(SWE - SFALL, 0)
          SF2 <- SWE - SWE_ex
          SNDEN <- ((SNDEN * SWE_ex) + (SNOWout$RHOn[i] * SF2)) / (SWE_ex + SF2)
        }
        
        #%% compact with melt (if any)
        if (Qw > 0 && SNDEN < rho_max_melt) {
          #%% Qw is rain + melt...
          h2o_added <- Qw / SWE
          if (h2o_added > 0.000001) {
            denDiff <- rho_max_melt - SNDEN  # "A" in snobal code
            SNDEN <- SNDEN + (denDiff / (1 + B_dens_rate / h2o_added))
          }
        }
        
      }
      
    }else if(mDEC_RHO_METHOD==3){
      
      SNDEN = rho0
    }
    
    if(mDEC_RHO_METHOD==4){
      
      #new snobal
      #snobal does density in this order (see file  _mass_bal.c):
      # (1) time-compaction (overburden and dest. temp metamorphism)
      # (2) new snowfall addition
      # (3) compaction due to liquid water (rain + melt)
      
      if (p_Wi == 0) {
        # no existing snowpack at start of time step
        if (SFALL > 0) {
          SNDEN <- SNOWout$RHOn[i]
        }
      } else {
        #%% take snow temp from Anderson approach (mDEC_RHO_METHOD = 0)
        T_s <- SNTMP  # deg C
        
        #%% variables (params and renamed copies)
        SWE_MAX <- 2000.0
        water <- 1000.0
        m_s <- SWE
        R <- 48  # not used??
        R1 <- 23.5
        R2 <- 24.5
        rho <- SNDEN_copy * 1000  # convert g/cm3 to kg/m3
        
        #%% Calculate rate at which compaction will be applied per time step.
        #%% Rate will be adjusted as time step varies.
        if (m_s >= SWE_MAX) {
          rate <- 1.0
        } else {
          rate <- R1 * cos((pi * m_s) / SWE_MAX) + R2
          rate <- rate / dt
        }
        
        #%% Proportional Destructive Temperature Metamorphism (d_rho_m)
        if (rho < 100) {
          c11 <- 1.0
        } else {
          c11 <- exp(-0.046 * (rho - 100))
        }
        
        d_rho_m <- 0.01 * c11 * exp(-0.04 * -1 * T_s)
        d_rho_m <- d_rho_m / rate
        
        #%% Proportional Overburden Compaction (d_rho_c)
        d_rho_c <- (0.026 * exp(-0.08 * -1 * T_s) * m_s * exp(-21.0 * (rho / water)))
        d_rho_c <- d_rho_c / rate
        
        #%% Compute New snow density
        rho <- rho + ((d_rho_m + d_rho_c) * rho)
        
        #%% if new snow, need to weight bulk density based on old vs. new
        if (SFALL > 0) {
          SWE_ex <- max(SWE - SFALL, 0)
          SF2 <- SWE - SWE_ex
          SNDEN <- rho / 1000  # convert from kg/m3 to g/cm3
          rho <- ((SNDEN * SWE_ex) + (1000 * SNOWout$RHOn[i] * SF2)) / (SWE_ex + SF2)
        }
        
        #%% h2o compaction
        MAX_DENSITY <- 550
        
        A <- MAX_DENSITY - rho
        B <- 0.4
        
        h2o_added <- Qw / m_s
        
        if (h2o_added > 0.000001) {
          rho <- rho + (A / (1 + B / h2o_added))
        }
        
        #%% assign back to variables used
        SNDEN <- rho / 1000  # convert from kg/m3 to g/cm3
      }
      
    }
    
    # CHECK THAT DENSITY IS WITHIN THE RANGE OF 0.05 TO 0.60
    SNDEN <- min(SNDEN, 0.6)
    SNDEN <- max(SNDEN, 0.05)
    
    # COMPUTE SNOW DEPTH AT THE END OF THE PERIOD
    if (SWE == 0) {
      SNDPT <- 0
      SNDEN <- NaN
    } else {
      SNDPT <- (0.1 * SWE) / SNDEN
    }
    
    # save variables
    SNOWout$SWE[i] <- SWE
    SNOWout$Hs[i] <- SNDPT
    SNOWout$RHO[i] <- SNDEN
    SNOWout$SWEq[i] <- W_q
    SNOWout$SWEi[i] <- W_i
    SNOWout$M_nr[i] <- M_NR
    SNOWout$M_ros[i] <- M_RoS
    SNOWout$MF[i] <- Mf
    SNOWout$SRFRZ[i] <- SRFRZ
    SNOWout$ATI[i] <- ATI
    SNOWout$T_pack[i] <- SNTMP
    SNOWout$T_snow[i] <- T_snow_new
    SNOWout$T_rain[i] <- T_rain
    SNOWout$T_prcp[i] <- TSNEW
    SNOWout$Deficit[i] <- Deficit
    SNOWout$outflow[i] <- E
    
  }
  
  # # post-processing... unit conversion etc.
  # SNOWout$RHO <- SNOWout$RHO * 1000
  # SNOWout$RHOn <- SNOWout$RHOn * 1000
  # 
  # # screen out density and bulk temp when we don't have a snowpack
  # a <- which(SNOWout$SWE == 0 | SNOWout$Hs == 0)
  # SNOWout$RHO[a] <- NaN
  # SNOWout$T_pack[a] <- NaN
  # 
  # # screen out new snowfall density and new snow thickness when we don't have snowfall
  # a <- which(SNOWout$SNOW == 0)
  # SNOWout$RHOn[a] <- NaN
  # SNOWout$Hn[a] <- NaN
  # SNOWout$T_snow[a] <- NaN
  # 
  # # screen out rainfall temperature in steps we don't have rain
  # a <- which(SNOWout$RAIN == 0)
  # SNOWout$T_rain[a] <- NaN
  # 
  # # screen out precip temperature in steps we dont have any precip
  # a <- which(sINPUTS$Prec == 0)
  # SNOWout$T_prcp[a] <- NaN
  # SNOWout$T_rain[a] <- NaN
  # SNOWout$T_snow[a] <- NaN
  
  return(SNOWout)
  
}
