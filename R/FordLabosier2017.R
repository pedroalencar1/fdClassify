## function to identify FD based on Ford and Laboiser 2017

FordLabosier2017 <- function(vtime, vswc, crit = c(40,20,30)){

  crit1 = crit[1] #upper limit
  crit2 = crit[2] #lower limit
  crit3 = crit[3] # recuperation limit

  swc <- data.frame(time = vtime, swc = vswc)

  #get pentads
  pentad.swc.list <- f.pentad(swc)
  series.swc <- pentad.swc.list$pentad_timestamp
  pentad.swc <- pentad.swc.list$pentad_matrix

  # get percentiles
  percentile.swc <- t(apply(pentad.swc,1, f.percentile))
  ts.percentile.swc <- ts(c(percentile.swc), frequency = 73, start =  min(year(series.swc$time)))

  #get column of percentiles
  percentile.series <- c(percentile.swc)

  #remove NA from the beggining of the series. Necessary for p.min calculation.
  firstNonNA <- min(which(!is.na(percentile.series)))
  percentile.series <- percentile.series[firstNonNA:length(percentile.series)]

  #get accumulated difference from 1 to 4 pentads.
  a1 <- unlist(lapply(1:length(percentile.series),
                      function(i) percentile.series[i] - percentile.series[i-1])) %>%
    c(rep(NA,1),.)
  a2 <- unlist(lapply(2:length(percentile.series),
                      function(i) percentile.series[i] - percentile.series[i-2])) %>%
    c(rep(NA,2),.)
  a3 <- unlist(lapply(3:length(percentile.series),
                      function(i) percentile.series[i] - percentile.series[i-3])) %>%
    c(rep(NA,3),.)
  a4 <- unlist(lapply(4:length(percentile.series),
                      function(i) percentile.series[i] - percentile.series[i-4])) %>%
    c(rep(NA,4),.)
  # combine data and get the most negative reduction of soil moisture over the previous 4 pentads
  data.table <- as.data.frame(cbind(percentile.series,a1,a2,a3,a4))
  data.table$a.min <- sapply(1:nrow(data.table), function(i) min(data.table[i,2:5],na.rm = T))
  data.table$p.min <- rbind(NA,as.data.frame(unlist(sapply(1:nrow(data.table),
                                                           function(i) which.max(data.table$a.min[i] - data.table[i,2:5])))))

  colnames(data.table[,7]) <- 'p.min'

  #Classification -- branchless style
  data.table$fd <- 0
  for (i in 2:nrow(data.table)){
    data.table$fd[i] <- (data.table$percentile.series[i] <= crit2) *
      (data.table$a.min[i] <= -20) *
      (data.table$percentile.series[i-data.table$p.min[i]] >= crit1)*
      (max(data.table$percentile.series[(i+1):(i+4)]) <= crit3)

  }

  data.table$p.min <- data.table$p.min * data.table$fd
  data.table[is.na(data.table)] <- 0 #avoid errors
  #function to get beginning, end and duration of FD
  data.table$event <- 0
  data.table$dur <- 0
  count <- 0
  for (i in 2:nrow(data.table)){
    if (data.table$fd[i-1] == 0 & data.table$fd[i] == 1){
      count <- count + 1
      data.table$event[i] <- count
      data.table$dur[i] <- 1
    }
    if (data.table$fd[i-1] == 1 & data.table$fd[i] == 1){
      data.table$event[i] <- count
      data.table$dur[i] <- data.table$dur[i-1] + 1
    }
  }
  #get dates from SWC series
  data.table$date <- series.swc$time[firstNonNA:nrow(series.swc)]

  #set output data: beg, onset, end, duration
  fd.summary <- data.frame(event = unique(data.table$event)[c(-1)])

  fd.summary$onset <- data.table  %>% group_by(event) %>%
    summarise(x = min(.data[["date"]])) %>% .[,2] %>% unlist() %>%
    as.POSIXct(origin = "1970-01-01") %>% .[c(-1)]

  fd.summary$end <- data.table  %>% group_by(event) %>%
    summarise(x = max(.data[["date"]])) %>% .[,2] %>% unlist() %>%
    as.POSIXct(origin = "1970-01-01") %>% .[c(-1)]

  fd.summary$dur <- data.table  %>% group_by(event) %>%
    summarise(x = max(.data[["dur"]]) + min(max(.data[["p.min"]]))) %>% .[,2] %>%
    unlist() %>% .[c(-1)]

  fd.summary$beg <- fd.summary$end - fd.summary$dur*5*86400

  fd.summary <- fd.summary[,c(1,5,2,3,4)]

  # get time series of flash drought occurences
  ts.fd <- rbind(matrix(NA,ncol = 1, nrow =firstNonNA - 1), matrix(data.table$fd,ncol = 1))

  # get series of 20 and 40 percentiles for visualization
  n_years <- max(year(series.swc$time)) - min(year(series.swc$time)) + 1
  p20 <- NA
  p40<- NA
  for (i in 1:73){
    p20[i] <- quantile(pentad.swc[i,], probs = 0.2, na.rm = T)
    p40[i] <- quantile(pentad.swc[i,], probs = 0.4, na.rm = T)
  }
  p20_series <- rep(p20,n_years)
  p40_series <- rep(p40,n_years)

  series.swc$p20 <- p20_series
  series.swc$p40<- p40_series

  swc_series <- cbind(series.swc,ts.percentile.swc, ts.fd)
  colnames(swc_series) <- c('Date','SWC','p20','p40', 'p.SWC', 'is.fd')

  output <- list('SWC_timeseries' = swc_series, 'FD_info' = fd.summary)

  return(output)
}
