
prepare.nc <- function(data, period = 'day', f = function(x) x){
  tbl_data <- tibble::tibble(time = as.POSIXct(data[,1]),value = data[,2])
  tbl_data[is.na(tbl_data)] <- 0
  tbl_data <- as_tbl_time(tbl_data, time)
  tbl_data_period <- collapse_by(tbl_data, period = period)
  period.data <- tbl_data_period %>% group_by(time) %>%
    summarise(value = f(.data[["value"]]))

  return(period.data)
}
