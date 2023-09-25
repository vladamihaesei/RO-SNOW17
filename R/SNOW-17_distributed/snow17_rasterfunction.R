library(lubridate)
source("R/SNOW-17_distributed/elev2press.R")
source("R/SNOW-17_distributed/isleapyear.R")
source("R/SNOW-17_distributed/meltfunction.R")
source("R/SNOW-17_distributed/RVsS.R")
source("R/SNOW-17_distributed/snowT.R")
source("R/SNOW-17_distributed/snowpack.R")

snow17dc <- function(sINPUTS, sIC, sSETTINGS, sPARAMS){
  
  # gather size information
  # M <- dim(sINPUTS$T)[1]  # row dimension
  # N <- dim(sINPUTS$T)[2]  # col dimension
  # t <- dim(sINPUTS$T)[3]  # time dimension
  # 
  
  M <- dim(sINPUTS$Temp)[1]
  N <- dim(sINPUTS$Temp)[2]
  t <- dim(sINPUTS$Temp)[3]
  
  time_series <- data.frame(Year = year(TIME),Month = month(TIME),Day = day(TIME),
                            Julian = yday(TIME), Date= as.Date(TIME))
  
  # find time step 
  dt <- 24#detect_timestep(sINPUTS$TIME) ## de modificat 
  
  ## Snow-17 Default Parameters and Settings
  
  ## will be overwritten in next section if specified in inputs
  
  ### traditional
  SCF <- sPARAMS$SCF
  UADJ <- sPARAMS$UADJ  # average wind function during rain on snow (mm/mb) - 0.04 value for the American River Basin from Shamir & Georgakakos 2007
  MBASE <- sPARAMS$MBASE  # base temperature above which melt typically occurs (deg C) - ****must be greater than 0 deg C****- value of 1 for the American River Basin from Shamir & Georgakakos 2007
  MFMAX <- sPARAMS$MFMAX # maximum melt factor during non-rain periods (mm/deg C 6 hr) - in western-facing slope assumed to occur on June 21 - value of 1.05 for the American River Basin from Shamir & Georgakakos 2007
  MFMIN <- sPARAMS$MFMIN  # minimum melt factor during non-rain periods (mm/deg C 6 hr) - in western-facing slope assumed to occur on December 21 - value of 0.60 for the American River Basin from Shamir & Georgakakos 2007
  TIPM <- sPARAMS$TIPM  # model parameter (>0.0 and <1.0) - Anderson Manual recommends 0.1 to 0.2 for deep snowpack areas
  NMF <- sPARAMS$NMF # maximum negative melt factor (mm/deg C 6 hr) - value of 0.15 for the American River Basin from Shamir & Georgakakos 2007
  PLWHC <- sPARAMS$PLWHC # percent liquid water holding capacity of the snowpack - max is 0.4 - value of 0.04 for the American River Basin from Shamir & Georgakakos 2007
  
  # Used if RvS = 0
  PXTEMP <- sPARAMS$PXTEMP  # Temperature dividing rain from snow, deg C - if temp is less than or equal to PXTEMP, all precip is snow. Otherwise, it is rain.
  
  # default settings
  RvS <- sSettings$Rvs
  # default parameters for snow density / compaction routines
  C1 <- sPARAMS$C1
  C2 <- sPARAMS$C2
  C3 <- sPARAMS$C3
  C4 <- sPARAMS$C4
  THRESD <- sPARAMS$THRESD
  C5 <- sPARAMS$C5
  CX <- sPARAMS$CX
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
  
  
  if (dim(sINPUTS$TIME) != t) {
    stop("TIME must be tx7 time matrix (per time_builder.m)")
  }
  
  if (dim(sINPUTS$Prec)[1] != M || dim(sINPUTS$Prec)[2] != N || dim(sINPUTS$Prec)[3] != t) {
    stop("Temp and Precip must be same size")
  }
  
  if (dim(sINPUTS$elev)[1] != M || dim(sINPUTS$elev)[2] != N) {
    stop("elevation must be MxN")
  }
  
  # Final Checks
  
  if (max(is.na(Temp)) || max(is.na(Prec))) {
    cat('Warning: NaN values detected in Temp or Prec data at some locations.  These grid cells will have missing or incomplete simulations\n')
  }
  
  if (length(RvS) > 1 && max(is.na(RvS))) {
    cat('Warning: NaN values detected in RvS matrix at some locations.  These grid cells will have missing or incomplete simulations\n')
  }
  
  if (max(is.na(ATI)) || max(is.na(W_q)) || max(is.na(W_i)) || max(is.na(Deficit))) {
    cat('Warning: NaN values detected in initial conditions data at some locations.  These grid cells will have missing simulations\n')
  }
  
  if (max(is.na(elev))) {
    cat('Warning: NaN values detected in elevation data at some locations.  These grid cells will have missing simulations\n')
  }
  
  ###check if we need matrix versions of variables
  
  vLIST <- c('SCF', 'UADJ', 'MBASE', 'MFMAX', 'MFMIN', 'TIPM', 'NMF', 'PLWHC', 'PXTEMP',
             'C1', 'C2', 'C3', 'C4', 'THRESD', 'C5', 'CX', 'ATI', 'W_q', 'W_i', 'Deficit',
             'SNDEN', 'SNTMP', 'SRFRZ', 'SGMLOS')
  
  if (M > 1 || N > 1) {
    for (v in seq_along(vLIST)) {
      if (exists(vLIST[v])) {
        if (is.matrix(get(vLIST[v]))) {
          if (dim(get(vLIST[v]))[1] == M && dim(get(vLIST[v]))[2] == N) {
            # do nothing... we are good
          } else {
            stop('inconsistent variable size')
          }
        } else {
          assign(vLIST[v], matrix(rep(get(vLIST[v]), M*N), nrow=M, ncol=N, byrow=TRUE))
        }
      } else {
        stop(paste(vLIST[v], 'is a missing variable!'))
      }
    }
  }
  
  ### convert snow density from kg/m3 to g/cm3
  SNDEN <- SNDEN/1000
  
  ### calculate snow depth initial conditions from SWE and density
  SNDPT <- ((W_q+W_i)/10) * SNDEN
  
  ### save snow densification parameters and settings to a structure
  SPACK <- list()
  SPACK$C1 <- C1
  SPACK$C2 <- C2
  SPACK$C3 <- C3
  SPACK$THRESD <- THRESD
  SPACK$C4 <- C4
  SPACK$C5 <- C5
  SPACK$CX <- CX
  
  invWq <- which(W_q > W_i)
  
  if (length(invWq) > 0) {
    cat("Warning: Found Wq>Wi in the initial conditions.\n")
    cat("...resetting these initial Wq values to PLWHC*Wi\n")
    
    W_q[invWq] <- PLWHC[invWq] * W_i[invWq]
  }
  
  #### make sure MFMIN<=MFMAX
  a <- which(MFMIN > MFMAX)
  if (length(a) > 0) {
    cat("Found MFMIN > MFMAX. Resetting so MFMIN=MFMAX in these cases\n")
    MFMIN[a] <- MFMAX[a]
  }
  
  ### Initialization
  ### initialize state variables/outputs
  SNOWout <- list()
  SNOWout$SWE <- array(NA, c(M, N, t))
  #SNOWout$outflow <- array(NA, c(M, N, t))
  #SNOWout$ATI <- array(NA, c(M, N, t))
  #SNOWout$SWEq <- array(NA, c(M, N, t))
  #SNOWout$SWEi <- array(NA, c(M, N, t))
  #SNOWout$Deficit <- array(NA, c(M, N, t))
  SNOWout$RHO <- array(NA, c(M, N, t))
  SNOWout$Hs <- array(NA, c(M, N, t))
  SNOWout$Hn <- array(NA,c(M, N, t))
  #SNOWout$SNTMP <- array(NA, c(M, N, t))
  
  ### initialize density, depth
  SNDEN <- matrix(0, M, N)
  SNDPT <- matrix(0, M, N)
  
  ### Constants
  stefan <- 6.12 * 10^(-10)     ### Stefan-Boltzman constant (mm/K/hr)
  
  ### Calculate atm. pressure (mb) based on elevation
  P_atm <- elev2press(elev)
  
  TIPM_dt <- 1.0 - ((1.0 - TIPM)^(dt/6))
  
  # Divide Rain and Snow at all time steps (yielding MxNxt matrices)
  # if (length(RvS) == 1) {
  #   out <- RvsS(Temp, Prec, RvS)
  #   Rfall <- out[[1]]
  #   Sfall <- out[[2]]
  # } else {
  #   
  #   # compute snowfall and rainfall based on precip and snowfall fraction
  #   Sfall <- RvS * Prec
  #   Rfall <- Prec - Sfall
  # }
  out <- RvsS(Temp,Prec,1)## the last number is RvS = 1 
  Rfall <- out[[1]]
  Sfall <- out[[2]]
  # Calculate fraction of rain and snow at each time step
  fracrain <- Rfall / Prec
  fracsnow <- Sfall / Prec
  
  # Set fracrain and fracsnow to 0 at any timesteps with 0 precip
  fracrain[Prec == 0] <- 0
  fracsnow[Prec == 0] <- 0
  
  # Create two melt factor (Mf) matrices (MxNx365 or 366) as function of day-of-year and whether it is a leap year
  Mf_year <- array(NA, dim = c(M, N, 365))
  Mf_leap <- array(NA, dim = c(M, N, 366))
  # for (doy in 1:365) {
  #   Mf_year[, , doy] <- Mf(doy, leap = FALSE)
  # }
  # for (doy in 1:366) {
  #   Mf_leap[, , doy] <- Mf(doy, leap = TRUE)
  # }
  
  for (g in 1:366) {
    if (g < 366) {
      Mf_year[,,g] <- meltfunction(2003, g, dt, MFMAX, MFMIN) # arbitrary normal year (2003)
    }
    
    Mf_leap[,,g] <- meltfunction(2004, g, dt, MFMAX, MFMIN) # arbitrary leap year (2004)
  }
  
  ## Model Execution
  
  for (j in 1:length(TIME)) {
    
    print(TIME[j])
    
    # if(j==1){
    #   ATI = IC_ATI;
    #   W_i = IC_Wi;
    #   W_q = IC_Wq;
    #   Deficit = IC_Deficit
    # }
    # 
    # save copy of previous (or initial) conditions
    p_ATI = ATI
    p_Wq = W_q
    p_Wi = W_i
    # p_Deficit = Deficit
    
    ## Snow Accumulation and Precip
    
    # temp this time step
    T_air_meanC = Temp[,,j]
    
    Pn = Prec[,,j] * fracsnow[,,j] * SCF           # water equivalent of new snowfall (mm)
    SFALL = Pn
    
    W_i = W_i + Pn     # incorporate into snowpack
    
    RAIN = Prec[,,j] * fracrain[,,j]         # amount of precip (mm) that is rain during this time step
    
    ## new snow density and depth
    
    DENNEW <- array(NA, dim = c(M, N))
    DENNEW[,-which(is.na(T_air_meanC))] <- 0.05
    DENNEW[which(T_air_meanC > -15 & T_air_meanC < 0, arr.ind = TRUE)] <- 0.05 + 0.0017 * ((T_air_meanC[which(T_air_meanC > -15 & T_air_meanC < 0, arr.ind = TRUE)] + 15) ^ 1.5)
    DENNEW[which(T_air_meanC >= 0, arr.ind = TRUE)] <- 0.05 + 0.0017 * (15 ^ 1.5)
    
    DPTNEW <- (SFALL / 10) / DENNEW # thickness of new snow (cm)
    
    DENNEW[SFALL == 0] <- 0
    DPTNEW[SFALL == 0] <- 0
    
    ## Temperature and Heat Deficit from new Snow
    
    T_snow_new = T_air_meanC          # assume snow temp is air temp
    delta_HD_snow = - (T_snow_new*Pn)/(80/0.5)      # delta_HD_snow = change in the heat deficit due to snowfall (mm)
    T_rain = T_snow_new*0 + PXTEMP
    
    ### special case at locations with T>0
    Twarm <- which(T_air_meanC >= 0)
    T_snow_new[Twarm] <- 0
    delta_HD_snow[Twarm] <- 0
    T_rain[Twarm] <- T_air_meanC[Twarm]
    
    ### take TSNEW as weighted avg of new snowfall temp and rainfall temp
    TSNEW <- (T_rain * RAIN + T_snow_new * Pn) / (Pn + RAIN)
    TSNEW[Pn + RAIN == 0] <- 0
    
    ### Antecedent temperature Index
    moresnow <- which(Pn > (1.5 * dt))
    littlesnow <- which(Pn <= (1.5 * dt))
    
    ATI[moresnow] <- T_snow_new[moresnow]
    ATI[littlesnow] <- ATI[littlesnow] + TIPM_dt[littlesnow] * (T_air_meanC[littlesnow] - ATI[littlesnow])
    
    ATI[ATI > 0] <- 0
    
    # Heat Exchange when no Surface Melt occurs
    
    if (isLeapYear(year(TIME[j]))==1) {
      Mf <- Mf_leap
    } else {
      Mf <- Mf_year
    }
    
    Mf <- Mf[, , time_series[j,]$Julian]
    
    delta_HD_T <- NMF * (dt/6) * ((Mf)/MFMAX) * (ATI - T_snow_new) # delta_HD_T = change in heat deficit due to a temperature gradient (mm)
    
    ### Rain-on-Snow Melt
    M_RoS <- matrix(0, M, N)
    
    if (length(RvS) == 1 && RvS[1] == 2) {
      # do not compute rain-on-snow melt if all precip is considered snowfall
    } else {
      e_sat <- 2.7489 * 10^8 * exp(-4278.63 / (T_air_meanC + 242.792))  # saturated vapor pressure at T_air_meanC (mb)
      
      M_RoS1 <- matrix(0, M, N)
      M_RoS2 <- matrix(0, M, N)
      M_RoS3 <- matrix(0, M, N)
      
      RoS <- which(RAIN > (0.25 * dt))  # 1.5 mm/ 6 hrs
      
      M_RoS1[RoS] <- stefan * dt * (((T_air_meanC[RoS] + 273)^4) - (273^4))
      M_RoS1[M_RoS1 < 0] <- 0
      
      M_RoS2[RoS] <- 0.0125 * RAIN[RoS] * T_rain[RoS]
      M_RoS2[M_RoS2 < 0] <- 0
      
      M_RoS3[RoS] <- (8.5 * UADJ[RoS] * (dt / 6) * (((0.9 * e_sat[RoS]) - 6.11) + (0.00057 * P_atm[RoS] * T_air_meanC[RoS])))
      M_RoS3[M_RoS3 < 0] <- 0
      
      M_RoS <- M_RoS1 + M_RoS2 + M_RoS3
      M_RoS[M_RoS < 0] <- 0
    }
    
    ### Non-Rain Melt
    M_NR <- matrix(0, M, N)
    
    nrm <- which(RAIN <= (0.25 * dt) & (T_air_meanC > MBASE))  # indices of non-rain melt
    
    M_NR[nrm] <- (Mf[nrm] * (T_air_meanC[nrm] - MBASE[nrm])) + (0.0125 * RAIN[nrm] * T_rain[nrm])
    M_NR[M_NR < 0] <- 0
    
    ### Melt, Ripeness of the snow cover and SWE
    # total melt
    Melt <- M_RoS + M_NR
    Melt[Melt < 0] <- 0
    
    # find locations where SWE (ice) is greater than melt
    s_melt <- which(W_i > Melt)
    W_i[s_melt] <- W_i[s_melt] - Melt[s_melt]
    
    # find locations where SWE (ice) is less than or equal to melt
    s_melt_all <- which(W_i <= Melt)
    Melt[s_melt_all] <- W_i[s_melt_all] + W_q[s_melt_all]
    W_i[s_melt_all] <- 0
    
    # Qw = liquid water available melted/rained at the snow surface (mm)
    Qw <- Melt + RAIN
    
    # Liquid water content
    W_qx <- PLWHC * W_i  # liquid water capacity (mm)
    Deficit <- Deficit + delta_HD_snow + delta_HD_T  # heat deficit (mm)
    
    # Limits of heat deficit
    Deficit[Deficit < 0] <- 0
    def_hi <- which(Deficit > 0.33 * W_i, arr.ind = TRUE)
    Deficit[def_hi] <- 0.33 * W_i[def_hi]
    
    ## In SNOW-17 the snow cover is ripe when both (Deficit=0) & (W_q = W_qx)
    E <- matrix(0, nrow=M, ncol=N)     # initialize outflow for this step
    SRFRZ <- matrix(0, nrow=M, ncol=N) # initialize
    
    #snow present and different cases for melting/ripeness conditions
    snow_check <- matrix(0, nrow=M, ncol=N) # use snow_check to keep track so we don't apply multiple snow cases to each location
    snow_case1 <- which(W_i>0 & ((Qw + W_q) > ((Deficit*(1+PLWHC)) + W_qx))) # Case 1: the snow is RIPE
    snow_check[snow_case1] <- 1
    snow_case2 <- which(W_i>0 & (Qw >= Deficit) & snow_check==0)              # Case 2: the snow is NOT yet ripe, but ice is being melted
    snow_check[snow_case2] <- 1
    snow_case3 <- which(W_i>0 & (Qw < Deficit) & snow_check==0)               # Case 3: the snow is NOT yet ripe
    snow_case4 <- which(W_i==0)                                              # Case 4: no snow
    
    ###case 1
    E[snow_case1] <- Qw[snow_case1] + W_q[snow_case1] - W_qx[snow_case1] - (Deficit[snow_case1]*(1+PLWHC[snow_case1])) # Excess liquid water (mm)
    W_q[snow_case1] <- W_qx[snow_case1]     # fills liquid water capacity
    W_i[snow_case1] <- W_i[snow_case1] + Deficit[snow_case1]   # W_i increases because water refreezes as heat deficit is decreased
    Deficit[snow_case1] <- 0
    SRFRZ[snow_case1] <- Deficit[snow_case1]
    
    ###case 2
    E[snow_case2] <- 0
    W_q[snow_case2] <- W_q[snow_case2] + Qw[snow_case2] - Deficit[snow_case2]
    W_i[snow_case2] <- W_i[snow_case2] + Deficit[snow_case2]               # W_i increases because water refreezes as heat deficit is decreased
    Deficit[snow_case2] <- 0
    SRFRZ[snow_case2] <- Deficit[snow_case2]
    
    # case 3
    E[snow_case3] <- 0
    W_i[snow_case3] <- W_i[snow_case3] + Qw[snow_case3]
    Deficit[snow_case3] <- Deficit[snow_case3] - Qw[snow_case3]
    SRFRZ[snow_case3] <- Qw[snow_case3]
    
    # case 4
    E[snow_case4] <- Qw[snow_case4]
    W_i[snow_case4] <- 0
    W_q[snow_case4] <- 0
    
    # calculate SWE from ice and liquid water in snowpack
    SWE <- W_i + W_q
    
    ATI[Deficit == 0] <- 0
    
    ### depth and density module
    
    # initialize internal variables
    SFALLX <- matrix(numeric(M*N), M, N)
    
    # save a copy of bulk snow density
    SNDEN_copy <- SNDEN
    
    # save copies of other variables
    SMELT <- Melt
    WE <- SWE
    SLIQ <- W_q
    
    # change in temp
    if (j==1) {
      DTA <- matrix(0, M, N)
    } else {
      DTA <- Temp[, , j] - Temp[, , j-1]
      
      a1 <- which(Temp[, , j-1] > 0 & Temp[, , j] > 0, arr.ind = TRUE)
      a2 <- which(Temp[, , j-1] < 0 & Temp[, , j] > 0, arr.ind = TRUE)
      
      DTA[a1] <- abs(DTA[a1])
      DTA[a2] <- T_air_meanC[a2]  # T_air_meanC = T[, , j]
    }
    
    # compute net melt during period
    PLOSS <- SMELT + SGMLOS
    WE1 <- WE - SFALL + PLOSS - SRFRZ
    
    # find cases of different snow conditions
    
    # all places with snow at start of time step
    snoden_case0 <- which(p_Wi > 0)
    
    # no existing snowpack at start of time step, and it snowed during time step
    snoden_case1 <- which(p_Wi == 0 & SFALL > 0)
    
    # existing snowpack at start of time step, and ...
    snoden_case2 <- which(p_Wi > 0 & PLOSS < WE1) # amount of melt is less than existing snow at start of period
    
    snoden_case3 <- which(p_Wi > 0 & PLOSS >= WE1) # amount of melt is more than existing snow at start of period
    
    if (length(snoden_case1) > 0) {
      # no existing snowpack at start of time step but it snowed
      SFALLX[snoden_case1] <- SFALL[snoden_case1] - SMELT[snoden_case1] - SGMLOS[snoden_case1]
      
      # account for any refreeze of new snowfall - increases density - recompute depth
      ss1 <- which(SRFRZ[snoden_case1] > 0)
      DENNEW[snoden_case1[ss1]] <- ((SFALLX[snoden_case1[ss1]] + SRFRZ[snoden_case1[ss1]]) / SFALLX[snoden_case1[ss1]]) * DENNEW[snoden_case1[ss1]]
      DPTNEW[snoden_case1[ss1]] <- (0.1 * WE[snoden_case1[ss1]]) / DENNEW[snoden_case1[ss1]]
      
      SNDPT[snoden_case1] <- DPTNEW[snoden_case1]
      SNDEN[snoden_case1] <- DENNEW[snoden_case1]
      SNTMP[snoden_case1] <- TSNEW[snoden_case1]
    }
    
    WEX <- matrix(NA, nrow = M, ncol = N)
    DPTOLD <- matrix(NA, nrow = M, ncol = N)
    TSNDPT <- matrix(NA, nrow = M, ncol = N)
    TSNDEN <- matrix(NA, nrow = M, ncol = N)
    
    if(length(snoden_case2) !=0){
      # amount of melt is less than existing snow at start of period
      # assume all melt came from existing snow - typical case
      
      # COMPUTE WATER EQUIVALENT OF EXISTING SNOW AT END OF PERIOD
      # - PRIOR TO ANY REFREEZE
      WEX[snoden_case2] <- WE1[snoden_case2] - PLOSS[snoden_case2]
      
      # COMPUTE DEPTH OF REMAINING OLD SNOW - ASSUME DENSITY OF
      # MELTED SNOW WAS THE SAME AS THE AVERAGE DENSITY OF THE
      # TOTAL OLD SNOW COVER
      DPTOLD[snoden_case2] <- (0.1 * WEX[snoden_case2]) / SNDEN[snoden_case2]
      
      # USE DENSITY OF COMBINED OLD AND NEW SNOW TO COMPUTE THE
      # THERMAL CONDUCTIVITY TO USE FOR CALCULATING THE CHANGE
      # IN THE TEMPERATURE OF THE SNOW COVER.
      TSNDPT[snoden_case2] <- DPTOLD[snoden_case2] + DPTNEW[snoden_case2]
      TSNDEN[snoden_case2] <- (DPTOLD[snoden_case2] * SNDEN[snoden_case2] + DPTNEW[snoden_case2] * DENNEW[snoden_case2]) / TSNDPT[snoden_case2]
      
      # COMPUTE NEW TEMPERATURE OF THE OLD SNOW
      SNTMPsc <- SNOWT(DPTNEW, TSNDEN, TSNDPT, WE, SLIQ, SNTMP, DTA, dt)
      SNTMP[snoden_case2] <- SNTMPsc[snoden_case2]
      
      # COMPUTE NEW DEPTH AND DENSITY OF OLD SNOW AFTER EFFECT OF
      # COMPACTION AND DESTRUCTIVE METAMORPHISM ARE TAKEN INTO
      # ACCOUNT.
      SNDsc <- SNOWPACK(SNDEN, WE, SLIQ, SNTMP, dt, SPACK)
      SNDENsc <- SNDsc$SNDEN
      SNDPTsc <- SNDsc$SNDPT
      SNDEN[snoden_case2] <- SNDENsc[snoden_case2]
      SNDPT[snoden_case2] <- SNDPTsc[snoden_case2]
    }
    
    SFALLX <- SFALL - PLOSS
    
    if (length(snoden_case3) != 0) {
      case3a <- which(SFALLX[snoden_case3] > 0)
      case3b <- which(SFALLX[snoden_case3] <= 0)
      
      snoden_case3a <- snoden_case3[case3a]
      snoden_case3b <- snoden_case3[case3b]
      
      if (length(snoden_case3a) != 0) {
        #%% case 3a
        # MELT DURING THE PERIOD WAS GREATER THAN THE AMOUNT OF SNOW
        # AT THE START OF THE PERIOD AND NEW SNOWFALL EXCEEDS THE
        # AMOUNT OF MELT DURING THE PERIOD - ASSUME MELT COMES FROM
        # NEW SNOWFALL
        # USE DENSITY OF COMBINED OLD AND NEW SNOW TO COMPUTE THE
        # THERMAL CONDUCTIVITY TO USE FOR CALCULATING THE CHANGE
        # IN THE TEMPERATURE OF THE SNOW COVER.
        TSNDPT[snoden_case3a] <- SNDPT[snoden_case3a] + DPTNEW[snoden_case3a]
        TSNDEN[snoden_case3a] <- (SNDPT[snoden_case3a] * SNDEN[snoden_case3a] + DPTNEW[snoden_case3a] * DENNEW[snoden_case3a]) / TSNDPT[snoden_case3a]
        
        # COMPUTE NEW TEMPERATURE OF THE OLD SNOW
        SNTMPsc <- SNOWT(DPTNEW, TSNDEN, TSNDPT, WE, SLIQ, SNTMP, DTA, dt)
        SNTMP[snoden_case3a] <- SNTMPsc[snoden_case3a]
        
        # COMPUTE NEW DEPTH AND DENSITY OF OLD SNOW AFTER EFFECT OF
        # COMPACTION AND DESTRUCTIVE METAMORPHISM ARE TAKEN INTO
        # ACCOUNT.
        SNDsc <- SNOWPACK(SNDEN, WE, SLIQ, SNTMP, dt, SPACK)
        SNDENsc <- SNDsc$SNDEN
        SNDPTsc <- SNDsc$SNDPT
        SNDEN[snoden_case3a] <- SNDENsc[snoden_case3a]
        SNDPT[snoden_case3a] <- SNDPTsc[snoden_case3a]
      }
      
      if (length(snoden_case3b) !=0) {
        #%% case 3b
        # SFALLX IS .LE. 0.0, THUS MELT EXCEEDS BOTH EXISTING SNOW
        # AT THE START OF THE PERIOD AND THE NEW SNOWFALL, BUT NOT
        # THE TOTAL OF THE TWO QUANTITIES. ASSUME ALL NEW SNOW
        # MELTS AND ONLY PART OF THE EXISTING PACK REMAINS.
        DPTNEW <- matrix(0, nrow = M, ncol = N)
        
        WEX[snoden_case3b] <- WE[snoden_case3b] - SRFRZ[snoden_case3b]
        
        # DEPTH OF EXISTING SNOW THAT REMAINS
        TSNDPT[snoden_case3b] <- (0.1 * WEX[snoden_case3b]) / SNDEN[snoden_case3b]
        
        # COMPUTE NEW TEMPERATURE OF THE REMAINING OLD SNOW
        SNTMPsc <- SNOWT(DPTNEW, SNDEN, TSNDPT, WE, SLIQ, SNTMP, DTA, dt)
        SNTMP[snoden_case3b] <- SNTMPsc[snoden_case3b]
        
        # COMPUTE NEW DEPTH AND DENSITY OF REMAINING OLD SNOW AFTER
        # EFFECT OF COMPACTION AND DESTRUCTIVE METAMORPHISM ARE
        # TAKEN INTO ACCOUNT.
        SNDsc <- SNOWPACK(SNDEN, WE, SLIQ, SNTMP, dt, SPACK)
        SNDENsc <- SNDsc$SNDEN
        SNDPTsc <- SNDsc$SNDPT
        SNDEN[snoden_case3b] <- SNDENsc[snoden_case3b]
        SNDPT[snoden_case3b] <- SNDPTsc[snoden_case3b]
      }
      
    }
    
    if (length(snoden_case0) != 0) {
      # IN ALL CASES WHEN SNOW EXISTED AT THE START OF THE PERIOD
      # COMPUTE NEW TOTAL DEPTH AND TEMPERATURE BY WEIGHING BOTH
      # THE OLD AND NEW SNOW.
      TSNEW[TSNEW>0] <- 0
      
      SNTMP <- (SNTMP*SNDPT+TSNEW*DPTNEW)/(SNDPT+DPTNEW)
      
      # ensure no positive temp snowpack
      SNTMP[SNTMP>0] <- 0
      
      SNDPT <- SNDPT+DPTNEW
      SNDEN <- (0.1*(WE-SRFRZ))/SNDPT
      
      # COMPUTE EFFECT OF MELT METAMORPHISM - REFREEZING OF LIQUID
      # WATER WITHIN THE SNOW COVER.
      f1 <- which(SRFRZ!=0)
      
      # COMPUTE INCREASE IN DENSITY DUE TO REFREEZING
      DCM <- matrix(NA, nrow = M, ncol = N)
      DCM[f1] <- WE[f1]/(WE[f1]-SRFRZ[f1])
      SNDEN[f1] <- SNDEN[f1]*DCM[f1]
    }
    
    # CHECK THAT DENSITY IS WITHIN THE RANGE OF 0.05 TO 0.60
    SNDEN[SNDEN < 0.05] <- 0.05
    SNDEN[SNDEN > 0.6] <- 0.6
    
    # RE-COMPUTE SNOW DEPTH AT THE END OF THE PERIOD (IN CASE DENSITY WAS OUT OF RANGE)
    SNDPT <- (0.1 * SWE) / SNDEN
    SNDEN[SWE == 0] <- NaN
    
    ## Save state variables
    SNOWout$Sfall <- Sfall
    SNOWout$Rainfall <- Rfall
    SNOWout$Fracsnow <- fracsnow
    SNOWout$Fracrain <- fracrain
    
    SNOWout$SWE[,,j] <- SWE
    #SNOWout$outflow[,,j] <- E
    #SNOWout$ATI[,,j] <- ATI
    #SNOWout$SWEq[,,j] <- W_q
    #SNOWout$SWEi[,,j] <- W_i
    #SNOWout$Deficit[,,j] <- Deficit
    SNOWout$RHO[,,j] <- SNDEN
    SNOWout$Hs[,,j] <- SNDPT
    #SNOWout$SNTMP[,,j] <- SNTMP
    
  }
  
  #post-processing... unit conversion etc.
  #SNOWout$RHO <- SNOWout$RHO * 1000 # convert to kg/m3
  
  # rr <- terra::rast(SNOWout$SWE)
  # time(rr) <- TIME
  # plot(rr) ### 2010-12-17
  # 
  #screen out density when we don't have a snowpack
  a <- which(SNOWout$SWE == 0 | SNOWout$Hs == 0)
  SNOWout$RHO[a] <- NA
  
  #SWE.r <- terra::rast(SNOWout$SWE)
  return(SNOWout)
  
}
