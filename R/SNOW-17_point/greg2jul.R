greg2jul <- function(yr, mo, da, hr, minu) {
  # checks
  if (mo < 1 || mo > 12) {
    stop("Invalid month")
  }
  
  if (da < 1) {
    stop("Invalid day")
  }
  
  if (hr < 0 || hr > 23) {
    stop("Hour must be between 0 and 23")
  }
  
  if (minu < 0 || minu > 59) {
    stop("Minute must be between 0 and 59")
  }
  
  leap_year <- (yr %% 4 == 0) & ((yr %% 100 != 0) | (yr %% 400 == 0))
  
  if (leap_year) {
    month_days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  } else {
    month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }
  
  if (da > month_days[mo]) {
    stop("Invalid days in the selected month")
  }
  
  # CODE
  cumd <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
  cumd[3:12] <- cumd[3:12] + as.integer(leap_year)
  
  days <- numeric(length(yr))
  for (yri in min(yr):max(yr)) {
    yrkp <- which(yr == yri)
    days[yrkp] <- cumd[mo[yrkp]] + da[yrkp]
  }
  
  return(days)
}


dd <- greg2jul(yr = 2010,mo =10, da=20, hr=12, minu =10)
dd

julian.Date(as.Date("2011-04-20"), origin = as.Date("2010-01-01"))[1]


