isLeapYear <- function(year) {
  if ((year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0) {
    leap <- 1
  } else {
    leap <- 0
  }
  return(leap)
}


