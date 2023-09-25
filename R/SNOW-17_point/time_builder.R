### function to convert and splite the time usinglubridate function in r 
library(lubridate)
time_builder <- function(time){
  
  if(length(time) == 0)
    stop("Error- no time input available")
  
  time <- data.frame(year = year(time), month = month(time), day = day(time),jul = format(as.Date(time), "%j"), data = as.Date(time, "%Y-%m-%d"))
  time <- time %>% mutate_if(is.character, as.numeric)
  time.arr <- array(c(as.numeric(time$year), as.numeric(time$month), as.numeric(time$day), as.numeric(time$jul), time$data), dim = c(nrow(time), ncol(time)))
  
  return(time)
}



# time_builder <- function(arg01, arg02, arg03, arg04, arg05, arg06, arg07, arg08, arg09, arg10, arg11, ...) {
#   # Parse Input Data
#   if (length(list(...)) == 11) {
#     # then the input variables are assumed to be in Gregorian
#     yr_i <- arg01
#     month_i <- arg02
#     day_i <- arg03
#     hr_i <- arg04
#     min_i <- arg05
#     yr_f <- arg06
#     month_f <- arg07
#     day_f <- arg08
#     hr_f <- arg09
#     min_f <- arg10
#     timestep <- arg11
#     jd_i <- greg2jul(yr_i, month_i, day_i, hr_i, min_i)
#     jd_f <- greg2jul(yr_f, month_f, day_f, hr_f, min_f)
#   } else if (length(list(...)) == 9) {
#     # then the input variables are assumed to be in Julian
#     yr_i <- arg01
#     jd_i <- arg02
#     hr_i <- arg03
#     min_i <- arg04
#     yr_f <- arg05
#     jd_f <- arg06
#     hr_f <- arg07
#     min_f <- arg08
#     timestep <- arg09
#   } else if (length(list(...)) == 7) {
#     # then the input variables are assumed to be in Gregorian, without hr and min
#     yr_i <- arg01
#     month_i <- arg02
#     day_i <- arg03
#     hr_i <- 0
#     min_i <- 0
#     yr_f <- arg04
#     month_f <- arg05
#     day_f <- arg06
#     hr_f <- 0
#     min_f <- 0
#     timestep <- arg07
#     jd_i <- greg2jul(yr_i, month_i, day_i, hr_i, min_i)
#     jd_f <- greg2jul(yr_f, month_f, day_f, hr_f, min_f)
#   } else if (length(list(...)) == 5) {
#     # then the input variables are assumed to be in Julian, without hr and min
#     yr_i <- arg01
#     jd_i <- arg02
#     yr_f <- arg03
#     jd_f <- arg04
#     timestep <- arg05
#   } else if (length(list(...)) == 6) {
#     # then the input varia}bles are assumed to be with starting Greg. and # of steps
#     yr_i <- arg01
#     month_i <- arg02
#     day_i <- arg03
#     hr_i <- arg04
#     timestep <- arg05
#     num_timesteps <- arg06
#     min_i <- 0
#     jd_i <- greg2jul(yr_i, month_i, day_i, hr_i, min_i)
#   } else if (length(list(...)) == 3) {
#     timestep <- arg03
#     
#     if (arg01 > 100000 && arg02 > 100000) {
#       start_serial <- arg01
#       end_serial <- arg02
#     } else {
#       wy1 <- arg01
#       wy2 <- arg02
#       
#       start_serial <- as.Date(paste0(wy1 - 1, "-10-01"))  # October 1st of the water year before wy1
#       end_serial <- as.Date(paste0(wy2, "-10-01")) - (timestep / 24)  # October 1st of wy2 minus one timestep
#     }
#     
#   } else if (length(list(...)) == 4) {
#     yr_i <- arg01
#     month_i <- arg02
#     yr_f <- arg03
#     month_f <- arg04
#     
#     if (yr_f < yr_i || (yr_f == yr_i && month_f < month_i)) {
#       stop("Invalid yr and month. Make sure final occurs after initial")
#     }
#   } else if (length(list(...)) == 1) {
#     sdates <- arg01
#   } else {
#     stop("Invalid number of input arguments. See time_builder.m for SYNTAX")
#   }
#   
#   if (length(list(...)) == 9 || length(list(...)) == 5) {
#     # then the input variables are assumed to be in Julian
#     month_i <- jul2greg(yr_i, jd_i)[[1]]
#     day_i <- jul2greg(yr_i, jd_i)[[2]]
#     month_f <- jul2greg(yr_f, jd_f)[[1]]
#     day_f <- jul2greg(yr_f, jd_f)[[2]]
#   }
#   
#   # Rest of the code...
#   # Parse Input Data
#   if (length(list(...)) > 1 && length(list(...)) != 4) {
#     if (length(list(...)) == 5 || length(list(...)) == 7) {
#       # then the input variables are assumed without hr, min
#       start_serial <- as.Date(paste0(arg01, "-", arg02, "-", arg03))
#       end_serial <- as.Date(paste0(arg04, "-", arg05, "-", arg06))
#     } else {
#       if (length(list(...)) != 3) {
#         start_serial <- as.POSIXct(paste0(arg01, "-", arg02, "-", arg03, " ", arg04, ":", arg05, ":00"))
#         if (length(list(...)) != 6) {
#           end_serial <- as.POSIXct(paste0(arg06, "-", arg07, "-", arg08, " ", arg09, ":", arg10, ":00"))
#         }
#       }
#     }
#     
#     serial_time_step <- arg11 / 24
#     
#     if (length(list(...)) != 6) {
#       day_range <- as.numeric(difftime(end_serial, start_serial, units = "days"))
#       
#       num_timesteps <- floor(day_range / serial_time_step)
#       time_temp <- matrix(0, nrow = num_timesteps + 1, ncol = 7)
#       
#       if (num_timesteps == ceiling(day_range * 24 / arg11)) {
#         if (arg11 >= 1) {
#           time_temp[, 7] <- seq(start_serial, by = paste0(as.character(serial_time_step), " days"), length.out = num_timesteps + 1)
#         } else {
#           time_temp[, 7] <- subhrly(start_serial, end_serial, arg11)
#         }
#       } else {
#         if (arg11 >= 1) {
#           time_temp[, 7] <- seq(start_serial, by = paste0(as.character(serial_time_step), " days"), length.out = num_timesteps + 1)
#           #         disp('Note: Ending date was truncated since designated time step did not divide evenly into given time range.')
#         } else {
#           time_temp[, 7] <- subhrly(start_serial, start_serial + (num_timesteps) * serial_time_step, arg11)
#         }
#       }
#       
#       time_temp[, 1:5] <- as.POSIXlt(time_temp[, 7])$year + 1900
#       time_temp[, 2] <- as.POSIXlt(time_temp[, 7])$mon + 1
#       time_temp[, 3] <- as.POSIXlt(time_temp[, 7])$mday
#       time_temp[, 6] <- as.numeric(format(time_temp[, 7], "%j"))
#       time_temp[, 7] <- as.numeric(time_temp[, 7])
#     } else if (length(list(...)) == 6) {
#       time_temp <- matrix(0, nrow = num_timesteps, ncol = 7)
#       
#       if (arg11 >= 1) {
#         time_temp[, 7] <- seq(start_serial, by = paste0(as.character(serial_time_step), " days"), length.out = num_timesteps)
#       } else {
#         time_temp[, 7] <- subhrly(start_serial, start_serial + (serial_time_step * (num_timesteps - 1)), arg11)
#       }
#       time_temp[, 1:5] <- as.POSIXlt(time_temp[, 7])$year + 1900
#       time_temp[, 2] <- as.POSIXlt(time_temp[, 7])$mon + 1
#       time_temp[, 3] <- as.POSIXlt(time_temp[, 7])$mday
#       time_temp[, 6] <- as.numeric(format(time_temp[, 7], "%j"))
#     }
#   } else if (length(list(...)) == 4) {
#     if (arg01 != arg03) {
#       num_timesteps <- (12 - arg02 + 1) + ((max((arg03 - arg01 - 1), 0)) * 12) + arg04
#     } else {
#       num_timesteps <- arg04 - arg02 + 1
#     }
#     
#     time_temp <- matrix(0, nrow = num_timesteps, ncol = 7)
#     
#     time_temp[1, 1] <- arg01
#     time_temp[1, 2] <- arg02
#     time_temp[1, 3] <- 1
#     time_temp[1, 4] <- 0
#     time_temp[1, 5] <- 0
#     time_temp[1, 6] <- greg2jul(time_temp[1, 1], time_temp[1, 2], 1, 0, 0)
#     time_temp[1, 7] <- as.numeric(as.POSIXct(paste0(time_temp[1, 1], "-", time_temp[1, 2], "-01 00:00:00")))
#     
#     if (num_timesteps > 1) {
#       for (i in 2:num_timesteps) {
#         time_temp[i, 2] <- time_temp[i - 1, 2] + 1
#         if (time_temp[i, 2] > 12) {
#           time_temp[i, 2] <- 1
#           time_temp[i, 1] <- time_temp[i - 1, 1] + 1
#         } else {
#           time_temp[i, 1] <- time_temp[i - 1, 1]
#         }
#         time_temp[i, 3] <- 1
#         time_temp[i, 4] <- 0
#         time_temp[i, 5] <- 0
#         time_temp[i, 6] <- greg2jul(time_temp[i, 1], time_temp[i, 2], 1, 0, 0)
#         time_temp[i, 7] <- as.numeric(as.POSIXct(paste0(time_temp[i, 1], "-", time_temp[i, 2], "-01 00:00:00")))
#       }
#     }
#   } else {
#     time_temp <- matrix(0, nrow = nrow(sdates), ncol = 7)
#     time_temp[, 7] <- sdates
#     time_temp[, 1:5] <- as.POSIXlt(time_temp[, 7])$year + 1900
#     time_temp[, 2] <- as.POSIXlt(time_temp[, 7])$mon + 1
#     time_temp[, 3] <- as.POSIXlt(time_temp[, 7])$mday
#     time_temp[, 6] <- as.numeric(format(time_temp[, 7], "%j"))
#   }
#   
#   return(time_temp)
# }
#   
# time_builder(arg01 = 2010, arg02 = 11, arg03 = 2020, arg04 = 12)
# 
#  current_hr <- time_temp[1, 4]
# current_min <- time_temp[1, 5]
# 
# if (nargs() > 1 && nargs() != 4) {
#   for (i in 2:nrow(time_temp)) {
#     current_min <- current_min + (timestep * 60)
#     if (current_min >= 60) {
#       while (current_min >= 60) {
#         current_min <- current_min - 60
#         current_hr <- current_hr + 1
#       }
#     }
#     
#     if (current_hr >= 24) {
#       current_hr <- 0
#     }
#     
#     if ((current_min < time_temp[i, 5]) && (current_hr > time_temp[i, 4])) {
#       # Then the computed time step is probably 1-2 minutes behind, and the min/hr both need adjustment
#       time_temp[i, 4] <- current_hr
#       time_temp[i, 5] <- current_min
#       time_temp[i, 7] <- as.numeric(as.POSIXct(paste0(time_temp[i, 1], "-", time_temp[i, 2], "-", time_temp[i, 3], " ", time_temp[i, 4], ":", time_temp[i, 5], ":00")))
#       time_temp[i, 6] <- floor(greg2jul(time_temp[i, 1], time_temp[i, 2], time_temp[i, 3], time_temp[i, 4], time_temp[i, 5]))
#     } else if ((current_min < time_temp[i, 5]) && (current_hr == time_temp[i, 4])) {
#       # Then the computed time step is probably 1-2 minutes ahead, and the min column needs adjustment
#       time_temp[i, 4] <- current_hr
#       time_temp[i, 5] <- current_min
#       time_temp[i, 7] <- as.numeric(as.POSIXct(paste0(time_temp[i, 1], "-", time_temp[i, 2], "-", time_temp[i, 3], " ", time_temp[i, 4], ":", time_temp[i, 5], ":00")))
#       time_temp[i, 6] <- floor(greg2jul(time_temp[i, 1], time_temp[i, 2], time_temp[i, 3], time_temp[i, 4], time_temp[i, 5]))
#     } else if ((current_min > time_temp[i, 5]) && (current_hr == time_temp[i, 4])) {
#       # Then the computed time step is probably 1-2 minutes behind, and min column needs adjustment
#       time_temp[i, 4] <- current_hr
#       time_temp[i, 5] <- current_min
#       time_temp[i, 7] <- as.numeric(as.POSIXct(paste0(time_temp[i, 1], "-", time_temp[i, 2], "-", time_temp[i, 3], " ", time_temp[i, 4], ":", time_temp[i, 5], ":00")))
#       time_temp[i, 6] <- floor(greg2jul(time_temp[i, 1], time_temp[i, 2], time_temp[i, 3], time_temp[i, 4], time_temp[i, 5]))
#     }
#   }
# }
# 
# time_series <- time_temp
# }