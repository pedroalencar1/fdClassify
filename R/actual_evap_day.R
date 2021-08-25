################### BASIC FUNCTIONS ###########################################
#' Calculate daily actual evaporation
#'
#' @param vtime data frame column or vector containing \code{date} data
#' @param vlatent_heat data frame column or vector containing daily latent heat flux (W.m-2.day-1)
#' @param vtemperature data frame column or vector containing daily average tempeature (Celcius)
#'
#' @return Data frame containing dates (daily) and evaporation (mm/day)
#'
#' @details
#' The calculation is based on the description in "A short course in Cloud Physics" (Rogers and Yau, 1989)
#'
#' @export
#'
#' @examples
#' ETa <- actual_evap_day(vtime = de_tha_d$time,
#'                        vlatent_heat =de_tha_d$latent_heat,
#'                        vtemperature = de_tha_d$temperature)
#'
#'
#'
actual_evap_day <- function(vtime, vlatent_heat, vtemperature = 20){

  latent_d <- (2500.96 - 2.37*vtemperature)*1000
  density_w <- 999.84 - 0.005*vtemperature^2
  #get actual evapotranspiration
  vevap <- 86400000*vlatent_heat*(latent_d*density_w)^-1


  #check if there are ETa lower then zero and set those values to zero (eliminate
  # condensation)
  vevap[vevap < 0] <- 0

  eta <- data.frame(time = vtime, eta = vevap)
  return(eta)
}

