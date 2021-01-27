################### BASIC FUNCTIONS ###########################################

hargreaves_day <- function(data_temp, my_lat) {

#  if(!require('tibbletime'))install.packages('tibbletime')
#  if(!require('dplyr'))install.packages('dplyr')
#  if(!require('stats'))install.packages('stats')
#  if(!require('lubridate'))install.packages('lubridate')

  #set temperature as tibble ang get min mac and mean temperatures
  tbl_temperature <- tibble::tibble(time = as.POSIXct(data_temp[,1]),value = data_temp[,2])
  tbl_temperature[is.na(tbl_temperature)] <- 0
  tbl_temperature <- as_tbl_time(tbl_temperature, time)
  temperature <- collapse_by(tbl_temperature, period = 'day')
  temperature.min <- temperature %>% group_by(time) %>%
    summarise(min = min(.data[["value"]]))
  temperature.max <- temperature %>% group_by(time) %>%
    summarise(max = max(.data[["value"]]))
  temperature.mean <- temperature %>% group_by(time) %>%
    summarise(mean = mean(.data[["value"]]))

  temperature.min <- temperature.min[complete.cases(temperature.min),]
  temperature.max <- temperature.max[complete.cases(temperature.max),]
  temperature.mean <- temperature.mean[complete.cases(temperature.mean),]

  #set basic variables
  beg <- year(temperature.min$time[1])
  end <- year(tail(temperature.min$time,1))
  year_series <- beg:end
  leap.year_series <- leap_year(year_series)
  J.series <- NULL
  for (i in 1:length(year_series)){
    if (leap.year_series[i] == TRUE){
      J.series[[i]] <- 1:366
    } else{
      J.series[[i]] <- 1:365
    }
  }

  # Calculates radiation on the top of atmosphere
  radiation_atmosphere<- data.frame(J = unlist(J.series))
  radiation_atmosphere$dr <- 1 + 0.033*cos(2*pi*radiation_atmosphere$J/365)
  radiation_atmosphere$delta <- 0.4093*sin(2*pi*radiation_atmosphere$J/365  - 1.405)
  radiation_atmosphere$ws <- acos(-tan(my_lat)*tan(radiation_atmosphere$delta))
  radiation_atmosphere$ra <- 15.392 * radiation_atmosphere$dr *
    (radiation_atmosphere$ws * sin(my_lat) * sin(radiation_atmosphere$delta) +
       cos(my_lat) * cos(radiation_atmosphere$delta) * sin(radiation_atmosphere$ws))

  #compute ET0 as by Hargreaves-Samani
  et0 <- temperature.mean
  et0$et <- 0.0023*radiation_atmosphere$ra*(temperature.max$max -
                                              temperature.min$min)*(temperature.mean$mean + 17.8)
  et0 <- et0[,c(1,3)]
  return(et0)
}
