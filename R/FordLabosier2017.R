#' @title Method of Ford and Labosier (2017)
#'
#' @description
#' This function follows the description contained in the original paper. We have as additional criterion that FD should have at least 3 pentads with SM lower than 30th percentile (as proposed by Dr. Ford in personal communication).
#'
#' @param vtime data frame column or vector containing \code{date} data
#' @param vswc data frame column or vector containing soil water content values
#' @param crit a vector of three value (default \code{c(20,40,30)}) indicating the model thresholds for lower, upper and persistance limits for SWC percentiles.
#'
#' @return A list with two data frames, one a time series with all data for FD identification, and the second with a summary of FD events.
#' @export
#'
#' @examples
#' FD_events <- FordLabosier2017(de_tha_d$time, de_tha_d$soil_water, crit = c(40,20,30))
#'
#'
FordLabosier2017 <- function(vtime, vswc, crit = c(40,20,30)){

  crit1 = crit[1] #upper limit
  crit2 = crit[2] #lower limit
  crit3 = crit[3] # recuperation limit


  swc <- data.frame(time = vtime, swc = vswc)

  #get pentads
  pentad.swc.list <- f.pentad(vtime = swc$time, vvalue = swc$swc,
                              na_rm = F, f = mean)
  series.swc <- pentad.swc.list$pentad_timestamp
  pentad.swc <- pentad.swc.list$pentad_matrix


  # get percentiles
  percentile.swc <- t(apply(pentad.swc,1, f.percentile))

  #get column of percentiles
  percentile.series <- c(percentile.swc)

  #remove NA from the beggining of the series. Necessary for p.min calculation.
  firstNonNA <- min(which(!is.na(percentile.series)))
  lastNonNA <- max(which(!is.na(percentile.series)))
  percentile.series <- percentile.series[firstNonNA:lastNonNA]

  #get accumulated difference from 1 to 4 pentads.
  a1 <- diff(percentile.series, lag = 1)  %>%  c(rep(100,1),.)
  a2 <- diff(percentile.series, lag = 2)  %>%  c(rep(100,2),.)
  a3 <- diff(percentile.series, lag = 3)  %>%  c(rep(100,3),.)
  a4 <- diff(percentile.series, lag = 4)  %>%  c(rep(100,4),.)

  # combine data and get the most negative reduction of soil moisture over the previous 4 pentads
  data.table <- as.data.frame(cbind(percentile.series,a1,a2,a3,a4))
  data.table$a.min <- sapply(1:nrow(data.table), function(i) min(data.table[i,2:5],na.rm = T))
  data.table$p.min <- rbind(NA,as.data.frame(unlist(sapply(2:nrow(data.table),
                                                           function(i) which.max(data.table$a.min[i] - data.table[i,2:5])))))
  colnames(data.table[,7]) <- 'p.min'

  #Classification -- branchless style
  data.table$fd <- 0
  for (i in 2:(nrow(data.table)-4)){
    # i = 1021

    data.table$fd[i] <- (data.table$percentile.series[i] <= crit2) *
      # (data.table$a.min[i] <= crit2 - crit1) *
      (data.table$percentile.series[i-data.table$p.min[i]] >= crit1)*
      (max(data.table$percentile.series[(i+1):(i+3)]) <= crit3)

    if ((data.table$fd[i-1] == 1) & (data.table$fd[i] == 1)){
      data.table$fd[i-1] <- 0
    }

  }

  data.table$p.min <- data.table$p.min * data.table$fd
  data.table[is.na(data.table)] <- 0 #avoid errors

  #function to get beginning, end and duration of FD
  data.table$event <- 0
  data.table$dur <- 0
  count <- 0
  for (i in 2:nrow(data.table)){
    if (data.table$fd[i] == 1){
      count <- count + 1
      data.table$event[i] <- count
      data.table$dur[i] <- data.table$p.min[i] + 4
    }

  }
  #get dates from SWC series
  data.table$date <- series.swc$time[firstNonNA:lastNonNA]

  fd.summary <- data.table[data.table$fd == 1,] %>%
    dplyr::select(date,event,dur, percentile.series,a.min)


  # get time series of flash drought occurences
  #get correct duration
  data.table$is.fd <- 0
  i = 1
  while(i < nrow(data.table)){
    if (data.table$fd[i] == 1){
      back <- data.table$p.min[i] - 1
      data.table$is.fd[(i - back):(i+4)] <- 1
      i <- i+4
    }
    i <- i + 1
  }

  ts.fd <- rbind(matrix(NA,ncol = 1, nrow =firstNonNA - 1), matrix(data.table$is.fd,ncol = 1),
                 matrix(NA, ncol = 1, nrow = (nrow(series.swc)-lastNonNA)))

  # get series of 20 and 40 percentiles for visualization
  n_years <- max(lubridate::year(series.swc$time)) - min(lubridate::year(series.swc$time)) + 1
  p20 <- NA
  p40<- NA
  p50<- NA
  for (i in 1:73){
    p20[i] <- quantile(pentad.swc[i,], probs = 0.2, na.rm = T)
    p40[i] <- quantile(pentad.swc[i,], probs = 0.4, na.rm = T)
    p50[i] <- quantile(pentad.swc[i,], probs = 0.5, na.rm = T)
  }
  p20_series <- rep(p20,n_years)
  p40_series <- rep(p40,n_years)
  p50_series <- rep(p50,n_years)

  series.swc$p20 <- p20_series
  series.swc$p40<- p40_series
  series.swc$p50<- p50_series

  percentile.series <- c(rep(NA,firstNonNA-1),percentile.series,rep(NA,nrow(series.swc)-lastNonNA)) #recompose length percentile series #


  swc_series <- cbind(series.swc,percentile.series, ts.fd)

  colnames(swc_series) <- c('time','SWC','p20','p40', 'p50', 'p.SWC', 'is.fd')

  output <- list('SWC_timeseries' = swc_series, 'FD_info' = fd.summary)

  return(output)



}
