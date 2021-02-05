################### BASIC FUNCTIONS ###########################################


actual_evap_day <- function(v.time, v.latent_heat, v.temperature = 20){

  latent_d <- (2500.96 - 2.37*v.temperature)*1000
  density_w <- 999.84 - 0.005*v.temperature^2
  #get actual evapotranspiration
  v.evap <- 86400000*v.latent_heat*(latent_d*density_w)^-1


  #check if there are ETa lower then zero and set those values to zero (eliminate
  # condensation)
  v.evap[v.evap < 0] <- 0

  eta <- data.frame(time = v.time, eta = v.evap)
  return(eta)
}

