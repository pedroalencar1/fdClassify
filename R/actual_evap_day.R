################### BASIC FUNCTIONS ###########################################


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

