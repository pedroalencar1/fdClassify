Osman2021 <- function(vtime, vswc, threshold = 20){


  swc <- data.frame(time = vtime, swc = vswc)
  # set data into pentads
  swc_pentad <- f.pentad(vtime = swc$time, vvalue = swc$swc)

  #calculate pentad percentiles (matrix and array)
  swc_percentile <- t(apply(swc_pentad[[2]],1, f.percentile))
  swc_percentile_series <- c(swc_percentile)

  #remove NA in the beginning of the series
  firstNonNA <- min(which(!is.na(swc_percentile_series)))
  percentile_series <- swc_percentile_series[firstNonNA:length(swc_percentile_series)]

  # calculate moving average with 4 penta window
  moving_average_swc <- runner::runner(swc_pentad[[1]]$var[firstNonNA:nrow(swc_pentad[[1]])]
                                       ,k = 4, f = mean)
  # moving_average_p <- runner(percentile_series,k = 4, f = mean)

  swc_df <- cbind(swc_pentad[[1]][firstNonNA:nrow(swc_pentad[[1]]),],
                  moving_average_swc, percentile_series)

  colnames(swc_df) <- c('time', 'swc','mvAvg_swc', 'percentile')

  # pentad swc lower than moving average
  swc_df$crit1 <- (swc_df$swc < swc_df$mvAvg_swc)*1


  ########################## new classification

  # get positon events
  index_aux <- rle(swc_df$crit1)$lengths
  index_aux2 <- runner::runner(index_aux, f = sum)

  index_aux <- index_aux[seq(2,length(index_aux),2)]
  index_aux2 <- index_aux2[seq(2,length(index_aux2),2)]

  index_df <- data.frame(index = index_aux2, length = index_aux)
  index_df <- index_df[index_df$length > 3,]

  index_df$onset <- 0

  for (i in 1:nrow(index_df)){
    # i = 1
    id.beg <- index_df$index[i] - index_df$length[i] + 4
    id.end <- index_df$index[i]

    min_perc_index <- which.min(swc_df$percentile[id.beg:id.end]-0)
    index_df$onset[i] <- id.beg + min_perc_index - 1
  }

  index_df$is.fd <- (swc_df$percentile[index_df$onset] < threshold)*1
  index_df <- index_df[index_df$is.fd == 1,]

  # join evetns to series
  swc_df$index <- 1:nrow(swc_df)
  swc_df <- dplyr::left_join(swc_df,index_df, by= 'index')
  swc_df[is.na(swc_df)] <- 0
  swc_df$is.fd <- 0

  #fix duration (intensification)
  for (i in 1:nrow(swc_df)){
    if (swc_df$length[i]>0){
      dur <- swc_df$length[i]
      swc_df$is.fd[(i-dur + 1):i] <- 1
    }
  }




  swc_df$dur <- 0
  # fix duration (after onset)
  for (i in 2:nrow(swc_df)){
    if((swc_df$is.fd[i-1] == 1) & (swc_df$is.fd[i] == 0) & (swc_df$percentile[i] <= threshold)){
      swc_df$is.fd[i] <- 1
    }
    if (swc_df$is.fd[i]>0){
      swc_df$dur[i] <- swc_df$dur[i-1] + swc_df$is.fd[i]
    }
  }


  # limit max duration
  for (i in 1:nrow(swc_df)){
    if (swc_df$dur[i] > 12) {
      swc_df$dur[i] <- 0
      swc_df$is.fd[i] <- 0
    }
  }



  ######################################################

  swc_df$event <- 0
  count <- 1
  for (i in 2:nrow(swc_df)){
    if ((swc_df$dur[i-1] != 0) & (swc_df$dur[i] == 0)){
      swc_df$event[i-1] <- count
      count <- count+1
    }
  }

  fd_summary <- swc_df[swc_df$event != 0,]
  fd_summary <- fd_summary[,c(1:3,10,11)]

  swc_df <- swc_df[,c(1:4,9:11)]

  # get series of 20 and 40 percentiles for visualization
  n_years <- max(lubridate::year(swc_pentad[[1]]$time)) - min(lubridate::year(swc_pentad[[1]]$time)) + 1
  p_threshold <- NA
  p40<- NA
  for (i in 1:73){
    p_threshold[i] <- quantile(swc_pentad[[2]][i,], probs = threshold/100, na.rm = T)
    p40[i] <- quantile(swc_pentad[[2]][i,], probs = 0.4, na.rm = T)
  }
  p_threshold_series <- rep(p_threshold,n_years)
  p40_series <- rep(p40,n_years)

  #add p20 and p40 for visualization
  swc_df$pthreshold <- p_threshold_series[firstNonNA:length(p_threshold_series)]
  swc_df$p40 <- p40_series[firstNonNA:length(p40_series)]

  NAs <- data.frame(matrix(NA,ncol = ncol(swc_df), nrow =firstNonNA - 1))
  colnames(NAs) <- colnames(swc_df)
  if(firstNonNA>1){
    NAs$time <- swc_pentad[[1]]$time[1:(firstNonNA - 1)]
    swc_df <- rbind(NAs, swc_df)
  }

  swc_df <- swc_df[,c(1:4,8,9,5:7)]


  output <- list('FD_time_series' = swc_df, 'FD_info' = fd_summary)

  return(output)
}
