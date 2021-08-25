################### BASIC FUNCTIONS ###########################################

#' @title Hargreaves-Samany daily-ET0 function
#'
#' @param vtime data frame column or vector containing \code{date} data
#' @param vtemperature data frame column or vector containing temperature
#' @param my_lat Latitude of study area
#'
#' @return The function return a data frame with two columns, time stamps (daily) and ET0
#'
#' @export
#'
#' @examples
#'
hargreaves_day <- function(vtime, vtemperature, my_lat) {

  data_temp <- data.frame(time = vtime, value = vtemperature)

  temperature.min <- prepare.nc(data_temp, period = 'day', f = min)
  temperature.max <- prepare.nc(data_temp, period = 'day', f = max)
  temperature.mean <- prepare.nc(data_temp, period = 'day', f = mean)

  temperature.min <- temperature.min[complete.cases(temperature.min),]
  temperature.max <- temperature.max[complete.cases(temperature.max),]
  temperature.mean <- temperature.mean[complete.cases(temperature.mean),]

  #set basic variables
  beg <- lubridate::year(temperature.min$time[1])
  end <- lubridate::year(tail(temperature.min$time,1))
  year_series <- beg:end
  leap.year_series <- lubridate::leap_year(year_series)
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
  et0$et <- 0.0023*radiation_atmosphere$ra*(temperature.max$value - temperature.min$value)*(temperature.mean$value + 17.8)
  et0 <- et0[,c(1,3)]
  return(et0)
}
