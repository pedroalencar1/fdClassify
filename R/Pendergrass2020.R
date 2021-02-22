Pendergrass2020 <- function(vtime, vet0, limit.down = 10){

  #load required packages
  library('tidyr')
  library('dplyr')
  library('readr')
  library('tibbletime')
  library('lubridate')
  library('stringr')
  library('runner')


  et0 <- data.frame(time = vtime, et0 = vet0)

  #get weeks
  et0$time <- as.Date(et0$time)

  week.et0.list <- f.week(et0)
  series.et0 <- week.et0.list$week_timestamp
  week.et0 <- week.et0.list$week_matrix

  # get percentiles
  # we used the here the percentiles of EDDI as described in Hoggins 2016.
  # Pendergrass is not quite clear on how to calculate the EDDI. What does a
  # 50% drop means? a drop in percentile or in absolute eddi?
  percentile.eddi <- t(apply(week.et0,1, eddi_percentile))

  data.table <- data.frame(time = as.Date(series.et0$time),
                           percentile = c(percentile.eddi),
                           et0 = c(week.et0))

  data.table$dif_perc <- append(c(NA,NA),as.vector(diff(data.table$percentile,
                                                        lag = 2)))

  #Remove eventual NA in the beginning of the series
  firstNonNA <- min(which(!is.na(data.table$percentile)))
  data.table <- data.table[firstNonNA:nrow(data.table),]


  #Classification
  data.table$is.fd <- 0

  for (i in 3:(nrow(data.table)-2)){
    data.table$is.fd[i] <- (data.table$dif_perc[i] >= 50) *
      (data.table$percentile[i+1] - data.table$percentile[i] > -limit.down) *
      (data.table$percentile[i+2] - data.table$percentile[i] > -limit.down)
  }

  #get correct durations
  for (i in 3:(nrow(data.table)-1)){
    if (data.table$is.fd[i] ==1 & data.table$is.fd[i-1] ==0){
      data.table$is.fd[i-1] = 1
      data.table$is.fd[i-2] = 1
      limit <- data.table$percentile[i] - limit.downwards
      while (data.table$percentile[i+1] >= limit){
        data.table$is.fd[i+1] <- 1
        i = i+1
      }
    }
  }

  #get positon end of dorughts
  dur_aux1 <- rle(data.table$is.fd==0)[1]
  dur_aux2 <- cumsum(dur_aux1$lengths)
  n <- length(dur_aux2)/2
  durations <- dur_aux1$lengths[c(2*(1:n))]
  positions <- dur_aux2[c(2*(1:n))]

  fd.info <- data.table[positions,]
  fd.info$dur <- durations

  output <- list('ET0_timeseries' = data.table, 'FD_info' = fd.info)

}
