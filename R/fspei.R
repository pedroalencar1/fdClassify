#' Standard Precipitation Evaporation Index calculation
#'
#' @param vtime a data.frame column or vector with daily time stamps (Date type)
#' @param vdeficit a data.frame column or vector with daily hydrological deficit
#' obtained by the difference of precipitation and potential evapotranspitation (P - ET0)
#' @param n a natural number that indicates the accumulation time (pentad, week, month, etc)
#'
#' @return The function return a list with two elements. One data frame with time stamped pentad values and a matrix with years organized in columns.
#'
#' @description Internal funciton to calculate the SPEI
#'
#' @export
#'
#' @examples
f.spei <- function(vtime, vdeficit, n){

  nyear <- max(lubridate::year(vtime)) - min(lubridate::year(vtime)) + 1

  #get accumulated deficit over n periods
  deficit_acc <- runner::runner(vdeficit, f = function(x) sum(x), k = n)
  deficit_acc[1:(n-1)] <- -1e7 #necessary to sort

  #get data as matrix with nyear columns.
  #The number of rows is equal to the number of periods in a year (73 pentads, 48-52 weeks, etc)
  deficit_matrix <- as.data.frame(matrix(deficit_acc, ncol = nyear, byrow = F))

  #sort the values to assess parameters
  deficit_matrix_sort <- t(apply(deficit_matrix, 1, sort))
  deficit_matrix_sort[deficit_matrix_sort == -1e7] <- NA

  #get paramenters of log-logistic distribution
  parameters <- param_loglogist(deficit_matrix_sort, n_param = 3)

  #get probabilities to assess SPEI
  deficit_matrix[deficit_matrix == -1e7] <- NA
  prob <- (1 + (parameters$a/(deficit_matrix - parameters$c))^parameters$b)^-1
  aux <- unlist(prob)
  aux[is.nan(aux)] <- 1e-4
  prob <-matrix(aux, dim(prob))

  spei_matrix <- as.data.frame(qnorm(prob))
  spei_time <- data.frame(time = vtime,spei = c(qnorm(prob)))

  output <- list('spei_timeseries' = spei_time, 'spei_matrix' = spei_matrix)

  return(output)
}
