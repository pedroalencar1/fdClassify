################### BASIC FUNCTIONS ###########################################

penman_day <- function(v.time, v.wind, v.temp, v.vpd, v.heatflux, altitude = 0) {

  #calculate the local pressure based on the altitude. If none is provided sea level is used.
  Patm <- 101.3*(1 - 2.22*height/100000)^5.26
  psychometric_c <- 0.000665*Patm

  v.time <- ymd(v.time)
  vapor_p_sat <- 0.661*exp(17.27*v.temp / (237.3 + v.temp))
  slope_vapor_p <- 4098*vapor_p_sat/(237.3 + v.temp)^2
  #from W m-2 to M m-2 day-2


  v.et0 <- 0.408*slope_vapor_p*v.heatflux + psychometric_c*(900/(v.temp + 273))*v.wind*v.vpd

  v.et0 <- v.et0/(slope_vapor_p + psychometric_c*(1 + 0.34*v.wind))

  et0 <- data.frame(time = v.time, et0 = v.et0)
  et0$et0[et0$et0 < 0] <- 0

  return(et0)
}
