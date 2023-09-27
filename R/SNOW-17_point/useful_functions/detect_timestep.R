detect_timestep <- function(TIME) {
  # Checks
  if (ncol(TIME) != 1) {
    } else {
      stop("TIME must be an Nx1 array or Nx7 time matrix")
    }
  
  if (any(is.na(TIME))) {
    stop("NaN values exist in TIME")
  }
  
  # if (any(sort(TIME) - TIME != 0)) {
  #   stop("TIME must be sorted in chronological order first")
  # }
  # 
  # Codes
  dt_all <- diff(TIME) * 24  # find time step in hours
  dt <- stats::mode(dt_all)  # find most common time step in hours
  
  dt_all <- dt_all - dt
  a <- which(dt_all > 0.00000001)  # check against a tolerance
  
  if (length(a) > 0) {
    cat("Note: TIME steps are not consistent! (detect_timestep)\n")
  }
  
  return(dt)
}


time_series <- data.frame(Year = year(tab$dat), Month = month(tab$dat), Day = day(tab$dat), Date = tab$dat)

time_series

detect_timestep(TIME = data_array)

data_array <- array(c(time_series$Year, time_series$Month, time_series$Day, time_series$Julian, as.character(time_series$Date)), dim = c(nrow(time_series), ncol(time_series)))
data_array[,4]
