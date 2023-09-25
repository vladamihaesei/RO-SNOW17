jul2greg <- function(year, juld) {
  # Code
  L <- nrow(year)
  w <- ncol(year)
  
  if (nrow(juld) != L || ncol(juld) != w) {
    stop("year and juld matrices must be the same size")
  }
  
  gmonth <- matrix(NA, nrow = L, ncol = w)
  gday <- matrix(NA, nrow = L, ncol = w)
  
  for (y in 1:L) {
    for (z in 1:w) {
      jd <- floor(juld[y, z])
      yr <- year[y, z]
      
      if (yr %% 4 == 0) { # test for leap year
        jul_index <- matrix(0, nrow = 366, ncol = 3)
        
        jul_index[1:31, 2] <- 1 # January
        jul_index[32:60, 2] <- 2 # February
        jul_index[61:91, 2] <- 3 # March
        jul_index[92:121, 2] <- 4 # April
        jul_index[122:152, 2] <- 5 # May
        jul_index[153:182, 2] <- 6 # June
        jul_index[183:213, 2] <- 7 # July
        jul_index[214:244, 2] <- 8 # August
        jul_index[245:274, 2] <- 9 # September
        jul_index[275:305, 2] <- 10 # October
        jul_index[306:335, 2] <- 11 # November
        jul_index[336:366, 2] <- 12 # December
        
        jul_index[1:31, 3] <- 1:31
        jul_index[32:60, 3] <- 1:29
        jul_index[61:91, 3] <- 1:31
        jul_index[92:121, 3] <- 1:30
        jul_index[122:152, 3] <- 1:31
        jul_index[153:182, 3] <- 1:30
        jul_index[183:213, 3] <- 1:31
        jul_index[214:244, 3] <- 1:31
        jul_index[245:274, 3] <- 1:30
        jul_index[275:305, 3] <- 1:31
        jul_index[306:335, 3] <- 1:30
        jul_index[336:366, 3] <- 1:31
      } else {
        jul_index <- matrix(0, nrow = 365, ncol = 3)
        
        jul_index[1:31, 2] <- 1 # January
        jul_index[32:59, 2] <- 2 # February
        jul_index[60:90, 2] <- 3 # March
        jul_index[91:120, 2] <- 4 # April
        jul_index[121:151, 2] <- 5 # May
        jul_index[152:181, 2] <- 6 # June
        jul_index[182:212, 2] <- 7 # July
        jul_index[213:243, 2] <- 8 # August
        jul_index[244:273, 2] <- 9 # September
        jul_index[274:304, 2] <- 10 # October
        jul_index[305:334, 2] <- 11 # November
        jul_index[335:365, 2] <- 12 # December
        
        jul_index[1:31, 3] <- 1:31
        jul_index[32:59, 3] <- 1:28
        jul_index[60:90, 3] <- 1:31
        jul_index[91:120, 3] <- 1:30
        jul_index[121:151, 3] <- 1:31
        jul_index[152:181, 3] <- 1:30
        jul_index[182:212, 3] <- 1:31
        jul_index[213:243, 3] <- 1:31
        jul_index[244:273, 3] <- 1:30
        jul_index[274:304, 3] <- 1:31
        jul_index[305:334, 3] <- 1:30
        jul_index[335:365, 3] <- 1:31
      }
      
      gmonth[y, z] <- jul_index[jd, 2]
      gday[y, z] <- jul_index[jd, 3]
    }
  }
  
  return(list(gmonth = gmonth, gday = gday))
}
