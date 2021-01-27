

Pendergrass2020 <- function(data.et0, limit.up = 10){

 #get pentads
  et0 <- as.data.frame(data.et0)
  et0$time <- as.Date(et0$time)

  pentad.et0.list <- f.pentad(et0)
  series.et0 <- pentad.et0.list$pentad_timestamp
  pentad.et0 <- pentad.et0.list$pentad_matrix

  # get percentiles
  percentile.et0 <- t(apply(pentad.et0,1, f.percentile))
  ts.percentile.et0 <- ts(c(percentile.et0), frequency = 73, start = min(year(series.et0$time)))

  data.table <- data.frame(time = time(ts.percentile.et0),
                           index = 1:length(percentile.et0),
                           percentile = c(percentile.et0),
                           et0 = c(pentad.et0))

  #Remove eventual NA in the beginning of the series
  firstNonNA <- min(which(!is.na(data.table$percentile)))
  data.table <- data.table[firstNonNA:nrow(data.table),]

  data.table$delta <- NA

  auxiliar1 <- unlist(lapply(4:nrow(data.table),
                             function(i) data.table$percentile[i] -
                               data.table$percentile[i-3])) %>%
    c(rep(NA,3),.)

  data.table$delta <- auxiliar1

  #Classification
  data.table$fd <- 0
  limit.upwards <- limit.up #max recuperation over sustain period

  for (i in 4:(nrow(data.table)-3)){
    data.table$fd[i] <- (data.table$delta[i] <= -50) *
      (data.table$percentile[i+1] - data.table$percentile[i] <= limit.upwards) *
      (data.table$percentile[i+2] - data.table$percentile[i] <= limit.upwards) *
      (data.table$percentile[i+3] - data.table$percentile[i] <= limit.upwards)
  }

  fd.info <- data.table[data.table$fd == 1,]

  output <- list('ET0_timeseries' = data.table, 'FD_info' = fd.info)

  return(output)

}
