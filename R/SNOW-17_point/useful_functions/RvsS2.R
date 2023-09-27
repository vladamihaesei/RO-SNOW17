RvsS2 <- function(TIME, Temp, Prec, Tt, Tr, ...) {
  # Parse the Data
  
  if (length(list(...)) == 0) {
    # then parameters Tr and Tt were not specified. Use "default" values
    # from the Kienzle paper
    Tt <- 2.8
    Tr <- 13
  }
  
  # Checks
  
  if (ncol(TIME) != 5) {
    if (ncol(TIME) > 1 && nrow(TIME) == 1) {
      TIME <- t(TIME)
    }
    
    if (ncol(TIME) == 1) {
      TIME <- time_builder(TIME)
    } else {
      stop("Invalid TIME input")
    }
  }
  
  if (length(Temp) != length(Prec)) {
    stop("Error - temperature and precip arrays must be the same size")
  }
  
  if (nrow(Temp) != 1) {
    if (ncol(Temp) != 1) {
      stop("Error - T must be an array")
    }
  }
  
  if (nrow(Prec) != 1) {
    if (ncol(Prec) != 1) {
      stop("Error - P must be an array")
    }
  }
  
  # Code
  
  L <- length(Temp)
  Rfall <- rep(0, L) * NaN
  Sfall <- rep(0, L) * NaN
  
  Tmts <- rep(0, 12) * NaN
  Tmrs <- rep(0, 12) * NaN
  
  # Monthly Adjustments
  for (i in 1:12) {
    Tmts[i] <- Tt + (Tt * sin((i + 2) / 1.91))
    Tmrs[i] <- Tr * (0.55 + (sin(i + 4)) * 0.6)
  }
  
  for (i in 1:L) {
    T_air <- Temp[i]  # air temperature at this time step (deg C)
    precip <- Prec[i]  # precipitation at this time step (mm)
    
    if (!is.na(T_air) && !is.na(precip)) {
      # Divide Rain and Snow
      M <- TIME[i, 2]
      Tmt <- Tmts[M]
      Tmr <- Tmrs[M]
      
      if (T_air < Tmt && (T_air > (Tmt - (Tmr / 2)))) {
        # More snow than rain
        fracrain <- 5 * (((T_air - Tmt) / (1.4 * Tmr))^3) + 6.76 * (((T_air - Tmt) / (1.4 * Tmr))^2) + 3.19 * (((T_air - Tmt) / (1.4 * Tmr))^1) + 0.5
      } else if (T_air > Tmt && (T_air < (Tmt + (Tmr / 2)))) {
        # More rain than snow
        fracrain <- 5 * (((T_air - Tmt) / (1.4 * Tmr))^3) - 6.76 * (((T_air - Tmt) / (1.4 * Tmr))^2) + 3.19 * (((T_air - Tmt) / (1.4 * Tmr))^1) + 0.5
      } else if (T_air <= Tmt - (Tmr / 2)) {
        # All snow
        fracrain <- 0
      } else if (T_air >= Tmt + (Tmr / 2)) {
        # All rain
        fracrain <- 1
      } else if (T_air == Tmt) {
        fracrain <- 0.5
      }
      
      if (fracrain > 1) {
        fracrain <- 1
      }
      
      if (fracrain < 0) {
        fracrain <- 0
      }
      
      fracsnow <- 1 - fracrain
      
      Rfall[i] <- fracrain * precip
      Sfall[i] <- fracsnow * precip
    }
  }
  
  return(list(Rfall = Rfall, Sfall = Sfall))
}

