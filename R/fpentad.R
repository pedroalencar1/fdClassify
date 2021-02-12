f.pentad <- function(data.var, na_rm = F){

  #define a column of years and reload start and end of series
  data.var$year <- year(data.var[,1])
  beg <- min(data.var$year) #year of the series beginning
  end <- max(data.var$year)

  year.var.list <- lapply(beg:end,f.year, data.var)
  n.years <- length(year.var.list)

  pentad.matrix<- NULL
  pentad.series <- NULL
  for (i in 1:n.years){
    year.var <- year.var.list[[i]][1:2]
    year.var <- tibble::tibble(time = as.POSIXct(year.var[,1]),
                               value = year.var[,2]) %>% as_tbl_time(time)
    pentad.var <- collapse_by(year.var, period = '5 days')
    pentad.var <- pentad.var %>% group_by(time) %>%
      summarise(var = mean(.data[["value"]], na.rm = na_rm))

    #in leap years, the last pentad has 6 days
    if (nrow(pentad.var) == 74){
      pentad.var$var[73] <- (pentad.var$var[73]*5 + pentad.var$var[74])/6
    }
    pentad.matrix <- cbind(pentad.matrix,pentad.var$var)
    #set dataframe with all years removing undesireble 74th pentad
    pentad.series <- rbind(pentad.series, pentad.var[1:73,])
  }

  # by coercion a 74th pentad is added in all years. Its value is equal to the
  # first pentad. For the leap years the 73th pentad already contains the last
  # day (see comments above). Here we remove the non-desirable 74-th pentad
  pentad.matrix <- pentad.matrix[1:73,]

  output<- list('pentad_timestamp' = pentad.series, 'pentad_matrix' = pentad.matrix)
  return(output)
}