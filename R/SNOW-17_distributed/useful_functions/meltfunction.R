#Seasonal variation calcs - indexed for Non-Rain melt
meltfunction <- function(yearrun, jday, dt, MFMAX, MFMIN) {
  leap <- as.integer(isLeapYear(yearrun))
  
  days <- leap * 0 + 365 # create days matrix (same size as leap)
  N_Mar21 <- jday - 81
  days[leap == 1] <- 366
  N_Mar21[leap == 1] <- jday[leap == 1] - 80
  
  Sv <- (0.5 * sin((N_Mar21 * 2 * pi) / days)) + 0.5 # seasonal variation
  Av <- 1.0 # latitude parameter, Av=1.0 when lat < 54 deg N ... code needs to change for high latitude sites!!!
  meltf <- (dt / 6) * ((Sv * Av * (MFMAX - MFMIN)) + MFMIN) # non-rain melt factor, seasonally varying
  
  return(meltf)
}


