meltfunction <- function(yearrun, jday, dt, MFMAX, MFMIN) {
  leap <- ifelse(leap_year(yearrun), 1, 0)
  
  if (leap == 1) {
    days <- 366
    N_Mar21 <- jday - 81  # day of year since March 21 (leap)
  } else {
    days <- 365
    N_Mar21 <- jday - 80  # day of year since March 21 (non-leap)
  }
  
  Sv <- (0.5 * sin((N_Mar21 * 2 * pi) / days)) + 0.5  # seasonal variation
  Av <- 1.0  # latitude parameter, Av=1.0 when lat < 54 deg N
  meltf <- (dt / 6) * ((Sv * Av * (MFMAX - MFMIN)) + MFMIN)  # non-rain melt factor, seasonally varying
  
  return(meltf)
}


