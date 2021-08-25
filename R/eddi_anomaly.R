
#' @title Calculate EDDI
#'
#' @description function to calculate EDDI (Evaporative Demand Drought Index) following Hobbins et al., 2016
#'
#' @param vet0 data frame column or vector containing daily ET0 (potential evapotranspiration). It can be obtained with the function \code(penman_day)
#'
#' @return vector with EDDI values
#'
#' @export
#'
#' @examples
#' eddi_values <- penman_day(vtime = de_tha_d$time, vwind = de_tha_d$wind_speed,
#'                           vvpd = de_tha_d$vapor_p_def, vtemp = de_tha_d$temperature,
#'                           vheatflux = (de_tha_d$sensible_heat + de_tha_d$latent_heat))[,2] %>%
#'                           eddi()
#'
eddi <- function(vet0){

  n <- length(vet0)
  empiric_p <- (1:n -0.33)/(n+0.33) #empiric probabilities (Tukey plotting)


  param <- data.frame(c0 = 2.515517,
                      c1 = 0.802853,
                      c2 = 0.010328,
                      d1 = 1.432788,
                      d2 = 0.189269,
                      d3 = 0.001308)

  W <- sqrt(-2*log(empiric_p))*(empiric_p <= 0.5) +
       sqrt(-2*log(1-empiric_p))*(empiric_p > 0.5)

  eddi <- (W - (param$c0 + param$c1*W + param$c2*W^2)*
    (1 + param$d1*W  + param$d2*W^2 + param$d3*W^3)^-1)*(2*(empiric_p >0.5) - 1)

  veddi <- eddi[rank(vet0)] # reorder to match input data

  return(veddi)
}


