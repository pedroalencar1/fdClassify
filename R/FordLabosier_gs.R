
#' @title Modified method of Ford and Labosier (2017)
#'
#' @description
#' Besides the additional duration criteria (at least 3 pentads under 30th p.)
#' we also added two new criteria:
#'    1. Events with onset out of the growing season (MAMJJASO) are not considered
#'       as flash droughts
#'    2. Evetns that are 3 pentads or less apart are considers the same event.
#'
#' @param vtime data frame column or vector containing \code{date} data
#' @param vswc data frame column or vector containing soil water content values
#' @param crit a vector of three value (default \code{c(20,40,30)}) indicating the model thresholds for lower, upper and persistance limits for SWC percentiles.
#'
#' @return A list with two data frames, one a time series with all data for FD identification, and the second with a summary of FD events.
#' @export
#'
#' @examples
#' FD_events <- FordLabosier_modif(de_tha_d$time, de_tha_d$soil_water, crit = c(40,20,30))
#'
#'

FordLabosier_gs <- function(vtime, vswc, crit = c(40,20,30)){

  crit1 = crit[1]
  crit2 = crit[2]
  crit3 = crit[3]

  #structure data
  swc <- data.frame(time = vtime, swc = vswc)

  #get pentads
  pentad.swc.list <- f.pentad(vtime = swc$time, vvalue = swc$swc,
                              na_rm = F, f = mean)
  series.swc <- pentad.swc.list$pentad_timestamp
  pentad.swc <- pentad.swc.list$pentad_matrix

  #get percentiles and separate NA values
  percentile.swc <- t(apply(pentad.swc, 1, f.percentile))
  percentile.series <- c(percentile.swc)
  firstNonNA <- min(which(!is.na(percentile.series)))
  lastNonNA <- max(which(!is.na(percentile.series)))
  percentile.series <- percentile.series[firstNonNA:lastNonNA]

  #find critical intensification periods
  a1 <- diff(percentile.series, lag = 1) %>% c(rep(100, 1),
                                               .)
  a2 <- diff(percentile.series, lag = 2) %>% c(rep(100, 2),
                                               .)
  a3 <- diff(percentile.series, lag = 3) %>% c(rep(100, 3),
                                               .)
  a4 <- diff(percentile.series, lag = 4) %>% c(rep(100, 4),
                                               .)
  data.table <- as.data.frame(cbind(percentile.series, a1,
                                    a2, a3, a4))
  data.table$a.min <- sapply(1:nrow(data.table), function(i) min(data.table[i,
                                                                            2:5], na.rm = T))
  data.table$p.min <- rbind(NA, as.data.frame(unlist(sapply(2:nrow(data.table),
                                                            function(i) which.max(data.table$a.min[i] - data.table[i, 2:5])))))
  #apply classification
  colnames(data.table[, 7]) <- "p.min"
  data.table$fd <- 0
  for (i in 2:(nrow(data.table) - 4)) {
    data.table$fd[i] <- (data.table$percentile.series[i] <=
                           crit2) * (data.table$percentile.series[i - data.table$p.min[i]] >=
                                       crit1) * (max(data.table$percentile.series[(i + 1):(i + 3)]) <= crit3)

    # if ((data.table$fd[i] == 1) & (data.table$fd[i] == 1)) { #remove redundances
    #   data.table$fd[i - 1] <- 0
    # }
  }

  #if onset is out of growing season it is not a flash drought
  data.table$month <- lubridate::month(as.Date(series.swc$time[firstNonNA:lastNonNA]))
  data.table$fd <- data.table$fd*(data.table$month > 3)*(data.table$month < 11)

  #remove redundances
  i = 1
  data.table$fd1<-data.table$fd #auxiliar column
  while (i < (nrow(data.table) - 3)){
  # for (i in 1:(nrow(data.table) - 3)) {
    # i = 79
    if ((data.table$fd1[i] == 1) & (sum(data.table$fd1[(i+1):(i+3)]) > 0)){
      data.table$fd[(i+1):(i+3)] <- 0
    }
    if (data.table$fd1[i] == 1){
      data.table$fd[(i - data.table$p.min[i]):(i+3)] <- 1
      i = i+4
    }
    i = i+1
  }
  data.table$fd1 <- NULL #remove auxiliar column
  data.table$month <- NULL
  # data.table$time <-as.Date(series.swc$time[firstNonNA:lastNonNA])
  # View(data.table)


  #join events that are 3 or less weeks apart
  index_aux3 <- rle(data.table$fd)$lengths
  index_aux4 <- runner::runner(index_aux3, f = sum)

  onset <- index_aux4[seq(0,length(index_aux4),2)] - 3
  duration <- index_aux3[seq(0,length(index_aux3),2)]

  index_aux3 <- index_aux3[seq(1,length(index_aux3),2)]
  index_aux4 <- index_aux4[seq(1,length(index_aux4),2)]

  if (min(index_aux3) < 4){
    length_short_breaks <- index_aux3[which(index_aux3 < 4)]
    position_short_breaks <- index_aux4[which(index_aux3 < 4)]

    for (i in 1:length(length_short_breaks)){
      i = 1
      beg_break <- position_short_breaks[i] - length_short_breaks[i] + 1
      for (j in beg_break:position_short_breaks[i]){
        data.table$fd[j] <- 1
      }
    }
  }


  #get summary data frame
  if (length(onset) == 0){
    fd.summary <- NA
  } else{
    fd.summary <- data.table[onset,]
    fd.summary$duration <- duration
    fd.summary$Onset_Date <- as.Date(series.swc$time[firstNonNA:lastNonNA])[onset]
    fd.summary$End_Date <- as.Date(series.swc$time[firstNonNA:lastNonNA])[onset+3]
    fd.summary <- fd.summary[,c(10,11,9,1,6)]
    fd.summary$event <- 1:nrow(fd.summary)
  }

  #complete series FD
  ts.fd <- c(rep(NA, times=firstNonNA - 1),data.table$fd,rep(NA, times=(nrow(series.swc) - lastNonNA)))

  #get relevant values of percentiles
  n_years <- max(lubridate::year(series.swc$time)) - min(lubridate::year(series.swc$time)) +  1
  p20 <- NA
  p40 <- NA
  p50 <- NA
  for (i in 1:73) {
    p20[i] <- quantile(pentad.swc[i, ], probs = 0.2, na.rm = T)
    p40[i] <- quantile(pentad.swc[i, ], probs = 0.4, na.rm = T)
    p50[i] <- quantile(pentad.swc[i, ], probs = 0.5, na.rm = T)
  }
  p20_series <- rep(p20, n_years)
  p40_series <- rep(p40, n_years)
  p50_series <- rep(p50, n_years)
  series.swc$p20 <- p20_series
  series.swc$p40 <- p40_series
  series.swc$p50 <- p50_series
  percentile.series <- c(rep(NA, firstNonNA - 1), percentile.series,
                         rep(NA, nrow(series.swc) - lastNonNA))

  #build complete data frame
  swc_series <- cbind(series.swc, percentile.series, ts.fd)
  colnames(swc_series) <- c("time", "SWC", "p20",
                            "p40", "p50", "p.SWC", "is.fd")

  swc_series$time <- as.Date(swc_series$time)

  #output list of series and summary
  output <- list(SWC_timeseries = swc_series, FD_info = fd.summary)

  return(output)
}
