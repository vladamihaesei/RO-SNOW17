library(dplyr)
library(lubridate)  
wateryear <- function(data){
  
  data$DAT <- as.Date(data$DAT)
  ani <- unique(year(data$DAT))
  data1 <- NULL
  
  for (j in 1:(length(ani) - 1)) {
    
    t3 <- data %>% dplyr::filter(DAT %in% seq(as.Date(paste0(ani[j],"-08-01")), as.Date(paste0(ani[j+1],"-07-31")), "days"))
    data1 <- rbind(data1,t3)
    
  }
  return(data1)
}

wateryeargrid <- function(data){
  
  ani <- unique(year(time(data)))
  rr <- rast()
  
  for(j in 1:(length(ani)-1)){
    
    data.f <- data[[which(time(data) %in% seq(as.Date(paste0(ani[j],"-08-01")), as.Date(paste0(ani[j+1],"-07-31")), "days"))]]
    rr <- c(rr,data.f)
    
  }
  
  return(rr)
  
}


wateryearlist <- function(data){
  
  ani <- sort(unique(substr(data,nchar(data) - 10,nchar(data) - 7)))
  dates <- as.Date(unique(substr(data,nchar(data) - 10,nchar(data) - 3)), format = "%Y%m%d")
  dates <- dates[year(dates) %in% ani]
  ls <- NULL
  
  for(j in 1:(length(ani)-1)){
   
    l.f <- data[dates %in% seq(as.Date(paste0(ani[j],"-08-01")), as.Date(paste0(ani[j+1],"-07-31")), "days") & !(month(dates) %in% c("7","8","9"))]
    ls <- c(ls,l.f)
    
  }
  return(ls)
  
}
#### verificare
# 
# ani <- unique(year(time(temp)))
# rr <- rast()
# 
# for(j in 1:(length(ani) - 1)){
#   
#   data.f <- temp[[which(terra::time(temp)%in%seq(as.Date(paste0(ani[j],"-08-01")), as.Date(paste0(ani[j+1],"-07-31")), "days"))]]
#   
#   rr <- c(rr,data.f)
# 
# }



