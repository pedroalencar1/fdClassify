
#' Title
#'
#' @param data data frame with time stamps
#' @param period string with interval of accumulation (default = 'day')
#' @param f \code{R function} to be applied (mean, max, sum, etc)
#'
#' @return
#'
#' @export
#'
#' @examples
#'
prepare.nc <- function(data, period = 'day', f = function(x) x){
  tbl_data <- tibble::tibble(time = as.POSIXct(data[,1]),value = data[,2])
  tbl_data[is.na(tbl_data)] <- 0
  tbl_data <- tibbletime::as_tbl_time(tbl_data, time)
  tbl_data_period <- tibbletime::collapse_by(tbl_data, period = period)
  period.data <- tbl_data_period %>% dplyr::group_by(time) %>%
    dplyr::summarise(value = f(.data[["value"]]))

  return(period.data)
}
