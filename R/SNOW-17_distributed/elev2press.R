
elev2press <- function(elev) {
  if (min(elev, na.rm = TRUE) < 0) {
    stop("This equation is only applied to elevations above 0 m")
  }
  
  elev <- elev/100  # Elevation for P_atm equation needs to be in hundreds of meters
  p_atm <- 33.86 * (29.9 - (0.335 * elev) + (0.00022 * (elev^2.4)))  # atmospheric pressure (mb) where elevation is in HUNDREDS of meters
  
  return(p_atm)
}
