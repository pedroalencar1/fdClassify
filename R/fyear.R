################### BASIC FUNCTIONS ###########################################

#' @title Separation by years - Internal function
#'
#' @param i a numeric index
#' @param day.var data frame with time stamps (input)
#' @param year.var data frame with time stamps (output - default = \code{NULL})
#'
#' @return return a vector with all data pertaining to a year
#'
#' @description Internal function to separate the data into years
#'
#' @export
#'
#' @examples
f.year <- function(i,day.var,year.var=NULL){

 # if(!require('dplyr'))install.packages('dplyr')
  day.var$year <- lubridate::year(day.var[,1])
  year.var$i <- dplyr::filter(day.var, day.var$year == i)
  return(year.var$i)
}
