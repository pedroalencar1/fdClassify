################### BASIC FUNCTIONS ###########################################

precipiration_day <- function(data.prec) {

  tbl_precipitation <- tibble::tibble(time = as.POSIXct(data.prec[,1]),value = data.prec[,2])
  tbl_precipitation[is.na(tbl_precipitation)] <- 0
  tbl_precipitation <- as_tbl_time(tbl_precipitation, time)
  precipitation <- collapse_by(tbl_precipitation, period = 'day')
  precipitation <- precipitation %>% group_by(time) %>%
    summarise(p = sum(.data[["value"]]))

  return(precipitation)
}
