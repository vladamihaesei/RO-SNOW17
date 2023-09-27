
RvsS <- function(Temp, Prec, RvS, v1=NULL, v2=NULL, ...) {
  
  ## Default Parameters
  if(missing(v1) & missing(v2)) {
    PXTEMP <- 0
    PXTEMP1 <- -1
    PXTEMP2 <- 3
  } else if(missing(v2)) {
    RvS <- 0
    PXTEMP <- v1
    PXTEMP1 <- NULL
    PXTEMP2 <- NULL
  } else if(!missing(v1) & !missing(v2)) {
    RvS <- 1
    PXTEMP <- NULL
    PXTEMP1 <- v1
    PXTEMP2 <- v2
  } else {
    stop("Invalid number of inputs")
  }
  
  if(!is.null(PXTEMP1) & length(PXTEMP1) != 1 & !is.null(PXTEMP1)) {
    stop("Invalid size of PXTEMP values")
  }
  
  if(PXTEMP1 == PXTEMP2) {
    PXTEMP <- PXTEMP1
    RvS <- 0
  } else {
    if(PXTEMP1 > PXTEMP2) {
      warning("PXTEMP1 was greater than PXTEMP2. Reversing them now")
      PXTEMP1a <- PXTEMP1
      PXTEMP1 <- PXTEMP2
      PXTEMP2 <- PXTEMP1a
      rm(PXTEMP1a)
    }
  }
  
  ## Checks
  if(length(Temp) != length(Prec)) {
    stop("Error - temperature and precip datasets must be the same size")
  }
  
  if(any(dim(Temp) != dim(Prec))) {
    stop("T and P must be same size")
  }
  
  ## Code
  transitionx <- c(PXTEMP1, PXTEMP2)
  transitiony <- c(1, 0)
  
  ### initialize fracsnow
  fracsnow <- numeric(length(Prec))
  
  if(RvS == 0) {
    a <- Temp <= PXTEMP
    fracsnow[a] <- 1
  } else if(RvS == 1) {
    a <- Temp <= PXTEMP1
    fracsnow[a] <- 1
    
    a <- Temp >= PXTEMP2
    fracsnow[a] <- 0
    
    a <- which(Temp > PXTEMP1 & Temp < PXTEMP2)
    fracsnow[a] <- approx(transitionx, transitiony, xout=Temp[a])$y
  } else if(RvS == 2) {
    fracsnow <- fracsnow + 1
  } else {
    stop("Invalid rain vs snow option")
  }
  
  ### rain fraction
  fracrain <- 1 - fracsnow
  
  ### calculate rain and snowfall based on fractions
  Rfall <- fracrain * Prec
  Sfall <- fracsnow * Prec
  
  return(list(Rfall=Rfall, Sfall=Sfall))
}

#ls <- RvsS(Temp,Prec, RvS = 1)

# pp <- terra::rast(ls$Rfall)
# plot(sum(pp))
# sn <- terra::rast(ls$Sfall)
# plot(sum(sn))
# tot <- pp+sn
# tot1 <- terra::rast(Prec)
# plot(sum(tot))
# plot(sum(tot1))
