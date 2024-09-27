RvsS <- function(Temp, Prec, RvS, v1 = NULL, v2 = NULL, ...) {
  
  ## Default Parameters
  if (missing(v1) && missing(v2)) {
    if (length(list(...)) == 0) {
      # Used if RvS = 0
      PXTEMP <- 0  # Temperature dividing rain from snow, deg C - if temp is less than or equal to PXTEMP, all precip is snow. Otherwise, it is rain.
      
      # Used if RvS = 1
      PXTEMP1 <- -1  # Lower Limit Temperature dividing transition from snow, deg C - if temp is less than or equal to PXTEMP1, all precip is snow. Otherwise, it is mixed linearly.
      PXTEMP2 <- 3   # Upper Limit Temperature dividing rain from transition, deg C - if temp is greater than or equal to PXTEMP2, all precip is rain. Otherwise, it is mixed linearly.
    }
  } else if (!is.null(v1) && is.null(v2)) {
    RvS <- 0
    PXTEMP <- v1
    PXTEMP1 <- NULL
    PXTEMP2 <- NULL
  } else if (!is.null(v1) && !is.null(v2)) {
    RvS <- 1
    PXTEMP <- NULL
    PXTEMP1 <- v1
    PXTEMP2 <- v2
  } else {
    stop("Invalid number of inputs")
  }
  
  if ((!is.null(PXTEMP1) && length(PXTEMP1) != 1) || (!is.null(PXTEMP2) && length(PXTEMP2) != 1) || (!is.null(PXTEMP) && length(PXTEMP) != 1)) {
    stop("Invalid size of PXTEMP values")
  }
  
  if (!is.null(PXTEMP1) && PXTEMP1 == PXTEMP2) {
    PXTEMP <- PXTEMP1
    RvS <- 0
  } else if (!is.null(PXTEMP1) && !is.null(PXTEMP2) && PXTEMP1 > PXTEMP2) {
    warning("PXTEMP1 was greater than PXTEMP2. Reversing them now")
    PXTEMP1a <- PXTEMP1
    PXTEMP1 <- PXTEMP2
    PXTEMP2 <- PXTEMP1a
  }
  
  ## Checks
  if (length(Temp) != length(Prec)) {
    stop("Error - temperature and precip datasets must be the same size")
  }
  
  if (max(abs(length(Temp) - length(Prec))) != 0) {
    stop("T and P must be the same size")
  }
  
  ## Code
  transitionx <- c(PXTEMP1, PXTEMP2)
  transitiony <- c(1, 0)
  
  # Initialize fracsnow
  fracsnow <- rep(0, length(Prec))
  
  if (RvS == 0) {
    a <- Temp <= PXTEMP
    fracsnow[a] <- 1
  } else if (RvS == 1) {
    a <- Temp <= PXTEMP1
    fracsnow[a] <- 1
    
    a <- Temp >= PXTEMP2
    fracsnow[a] <- 0
    
    a <- which(Temp > PXTEMP1 & Temp < PXTEMP2)
    fracsnow[a] <- approx(transitionx, transitiony, Temp[a])$y
  } else if (RvS == 2) {
    fracsnow <- fracsnow + 1
  } else {
    stop("Invalid rain vs snow option")
  }
  
  # Rain fraction
  fracrain <- 1 - fracsnow
  
  # Calculate rain and snowfall based on fractions
  Rfall <- fracrain * Prec
  Sfall <- fracsnow * Prec
  
  return(list(Rfall = Rfall, Sfall = Sfall))
}
