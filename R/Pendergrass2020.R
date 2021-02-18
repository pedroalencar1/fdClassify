Pendergrass2020 <- function(vtime, vet0, limit.up = 10){

  et0 <- data.frame(time = vtime, et0 = vet0)

  #get weeks
  et0$time <- as.Date(et0$time)

  week.et0.list <- f.week(et0)
  series.et0 <- week.et0.list$week_timestamp
  week.et0 <- week.et0.list$week_matrix

  # get percentiles
  percentile.et0 <- t(apply(week.et0,1, f.percentile))

  data.table <- data.frame(time = as.Date(series.et0$time),
                           percentile = c(percentile.et0),
                           et0 = c(week.et0))

  data.table$dif_perc <- append(NA,as.vector(diff(data.table$percentile,
                                                  lag = 1)))

  #Remove eventual NA in the beginning of the series
  firstNonNA <- min(which(!is.na(data.table$percentile)))
  data.table <- data.table[firstNonNA:nrow(data.table),]

  # data.table$delta <- NA

  data.table$delta <- unlist(lapply(4:nrow(data.table),
                                    function(i) data.table$percentile[i] -
                                      data.table$percentile[i-3])) %>%   c(rep(NA,3),.)

  #Classification
  data.table$fd <- 0
  limit.upwards <- limit.up #max recuperation over sustain period

  for (i in 4:(nrow(data.table)-3)){
    data.table$fd[i] <- (data.table$delta[i] <= -50) *
      (data.table$percentile[i+1] - data.table$percentile[i] <= limit.upwards) *
      (data.table$percentile[i+2] - data.table$percentile[i] <= limit.upwards) *
      (data.table$percentile[i+3] - data.table$percentile[i] <= limit.upwards)
  }

  #get correct durations
  for (i in 4:(nrow(data.table)-1)){
    if (data.table$fd[i] ==1){
      data.table$fd[i-1] = data.table$fd[i-2] <- 1
      if (data.table$dif_perc[i+1] < limit.upwards){
        data.table$fd[i+1] <- 1
      }
    }
  }

  #get positon end of dorughts
  dur_aux1 <- rle(data.table$fd==0)[1]
  dur_aux2 <- cumsum(dur_aux1$lengths)
  n <- length(dur_aux2)/2
  durations <- dur_aux1$lengths[c(2*(1:n))]
  positions <- dur_aux2[c(2*(1:n))]

  fd.info <- data.table[positions,]
  fd.info$dur <- durations

  output <- list('ET0_timeseries' = data.table, 'FD_info' = fd.info)

}
