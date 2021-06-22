#' Calibrate parameters of multiple log-logist functions using Probability
#' Weighted Moments (PWM) according to Singh (1998) "Entropu-based parameter
#' estimation in Hydrology" (Ch. 18)
#'
#' @description
#' Auxiliar function or calculate SPEI and EDDI.
#'
#' @param var A matrix or data frame with the variable (hydrologic deficit for
#' SPEI, potential evapotranspitation for EDDI). The data has to be organized with
#' weeks (or desired interval) in rows (e.g. 52 rows) and years in columns.
#' @param n_param Number of parameters of the Log-logist distribution (either 2
#' or 3).
#'
#' @return
#' @export
#'
#' @examples
param_loglogist <- function(var, n_param = 3){
  number_years <- ncol(var)
  param <- 1:number_years
  param <- 1- (param-0.35)/number_years
  param <- matrix(rep(param,nrow(var)), ncol = number_years,
                  nrow = nrow(var), byrow = T)

  w0 <- rowSums(var, na.rm = T)/number_years
  w1 <- rowSums(var*param,na.rm = T)/number_years
  w2 <- rowSums(var*param^2, na.rm = T)/number_years

  b <- (2*w1 - w0)/(6*w1 - w0 - 6*w2)
  a <- (w0 - 2*w1)*b/(gamma(1 + 1/b) * gamma(1 - 1/b))
  c <- w0 - (w0 - 2*w1)*b

  if (n_param == 2){
    parameters <- data.frame(a = a,b = b)
  } else if (n_param == 3){
    parameters <- data.frame(a = a,b = b, c = c)
  } else {
    parameters <- NA
  }

  return(parameters)
}
