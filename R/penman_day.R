################### BASIC FUNCTIONS ###########################################

#' @title penman_day - Daily ET0 using Penman-Monteith
#'
#' @param vtime data frame column or vector containing \code{date} data
#' @param vwind data frame column or vector containing daily wind velocity
#' @param vtemp data frame column or vector containing daily temperature
#' @param vvpd data frame column or vector containing daily vapor pressure
#' @param vheatflux data frame column or vector containing daily total heat flux (latent and sensible heat flux)
#' @param altitude study area altitude in meters above sea level
#'
#' @return The function returns a data frame with time stamped daily potential evapotranspiration
#'
#' @export
#'
#' @examples
#'  ET0 <- penman_day(vtime = de_tha_d$time, vwind = de_tha_d$wind_speed,
#'                    vvpd = de_tha_d$vapor_p_def, vtemp = de_tha_d$temperature,
#'                    vheatflux = (de_tha_d$sensible_heat + de_tha_d$latent_heat))
#'
penman_day <- function(vtime, vwind, vtemp, vvpd, vheatflux, altitude = 0) {

  #calculate the local pressure based on the altitude. If none is provided sea level is used.
  Patm <- 101.3*(1 - 2.22*altitude/100000)^5.26
  psychometric_c <- 0.000665*Patm

  vtime <- lubridate::ymd(vtime)
  vapor_p_sat <- 0.661*exp(17.27*vtemp / (237.3 + vtemp))
  slope_vapor_p <- 4098*vapor_p_sat/(237.3 + vtemp)^2
  #from W m-2 to M m-2 day-2

  #convert from W m-2 to MJ m-2 day-1
  vheatflux <- vheatflux*86400/10^6

  vet0 <- 0.408*slope_vapor_p*vheatflux + psychometric_c*(900/(vtemp + 273))*vwind*vvpd

  vet0 <- vet0/(slope_vapor_p + psychometric_c*(1 + 0.34*vwind))

  et0 <- data.frame(time = vtime, et0 = vet0)
  et0$et0[et0$et0 < 0] <- 0

  return(et0)
}
