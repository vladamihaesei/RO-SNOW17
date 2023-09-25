
SNOWT <- function(DPTNEW, SNDEN, SNDPT, WE, SLIQ, SNTMP, DTA, dt) {
  # parameters
  CICE <- 2.1E06
  CH2O <- 4.2E06
  CAIR <- 1E03
  
  # HEAT WAVE LENGTH, IN SECONDS
  WAVEL <- 2.0 * 3600 * dt
  
  # CONVERT DEPTHS TO METERS
  SNDPTM <- 0.01 * SNDPT
  DPTNWM <- 0.01 * DPTNEW
  
  # GET TOTAL WATER EQUIVALENT AND DENSITY (SOLID PLUS LIQUID WATER)
  WETOT <- WE + SLIQ
  DENTOT <- 0.1 * WETOT / SNDPT
  
  # COMPUTE THERMAL CONDUCTIVITY
  TC <- 0.0442 * exp(5.181 * DENTOT)
  
  # COMPUTE FRACTION LIQUID
  FL <- SLIQ / WETOT
  
  # COMPUTE EFFECTIVE SPECIFIC VOLUMETRIC HEAT CAPACITY
  C <- CICE * SNDEN + CAIR * (1 - SNDEN - FL) + CH2O * FL
  
  # COMPUTE NEW SNOW COVER TEMPERATURE
  ALP <- sqrt(pi * C / (WAVEL * TC))
  
  # calculate snow temperature
  SNTMP <- SNTMP + DTA * ((1 - exp(-ALP * SNDPTM)) / (ALP * SNDPTM))
  
  # modify snow temperature in cases with new snow
  gt0 <- which(DPTNEW > 0)
  SNTMP[gt0] <- SNTMP[gt0] + DTA[gt0] * ((exp(-ALP[gt0] * DPTNWM[gt0]) - exp(-ALP[gt0] * SNDPTM[gt0])) / (ALP[gt0] * (SNDPTM[gt0] - DPTNWM[gt0])))
  
  # enforce upper limit to snow temperature
  SNTMP[SNTMP > 0] <- 0
  
  return(SNTMP)
}






