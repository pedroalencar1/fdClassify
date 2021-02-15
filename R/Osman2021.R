Osman2021 <- function(swc){

  # set data into pentads
  swc_pentad <- f.pentad(swc)

  #calculate pentad percentiles (matrix and array)
  swc_percentile <- t(apply(swc_pentad[[2]],1, f.percentile))
  swc_percentile_series <- c(swc_percentile)

  #remove NA in the beginning of the series
  firstNonNA <- min(which(!is.na(swc_percentile_series)))
  percentile_series <- swc_percentile_series[firstNonNA:length(swc_percentile_series)]

  # calculate moving average with 4 penta window
  moving_average_swc <- runner(swc_pentad[[1]]$var[firstNonNA:nrow(swc_pentad[[1]])]
                               ,k = 4, f = mean)
  # moving_average_p <- runner(percentile_series,k = 4, f = mean)

  swc_df <- cbind(swc_pentad[[1]][firstNonNA:nrow(swc_pentad[[1]]),],
                  moving_average_swc, percentile_series)

  colnames(swc_df) <- c('date', 'swc','mvAvg_swc', 'percentile')

  swc_df$crit1 <- (swc_df$swc < swc_df$mvAvg_swc)*1

  swc_df$crit2 <- 0
  for (i in 2:nrow(swc_df)){
    swc_df$crit2[i] <- (swc_df$crit1[i] + swc_df$crit2[i-1])*swc_df$crit1[i]
  }

  swc_df$dur <- (swc_df$crit2 >3)*(swc_df$percentile < 20)*swc_df$crit2
  swc_df$is.fd <- (swc_df$dur > 0)*1

  swc_df$event <- 0
  count <- 1
  for (i in 2:nrow(swc_df)){
    if ((swc_df$dur[i-1] != 0) & (swc_df$dur[i] == 0)){
      swc_df$event[i-1] <- count
      count <- count+1
    }
  }

  fd_summary <- swc_df[swc_df$event != 0,]
  fd_summary <- fd_summary[,c(1,9,2,3,4,7)]

  swc_df <- swc_df[,c(1,9,2,3,4,7)]

  # get series of 20 and 40 percentiles for visualization
  n_years <- max(year(swc_pentad[[1]]$time)) - min(year(swc_pentad[[1]]$time)) + 1
  p20 <- NA
  p40<- NA
  for (i in 1:73){
    p20[i] <- quantile(swc_pentad[[2]][i,], probs = 0.2, na.rm = T)
    p40[i] <- quantile(swc_pentad[[2]][i,], probs = 0.4, na.rm = T)
  }
  p20_series <- rep(p20,n_years)
  p40_series <- rep(p40,n_years)

  #add p20 and p40 for visualization
  swc_df$p20 <- p20_series[firstNonNA:length(p20_series)]
  swc_df$p40 <- p40_series[firstNonNA:length(p40_series)]

  NAs <- data.frame(matrix(NA,ncol = ncol(swc_df), nrow =firstNonNA - 1))
  colnames(NAs) <- colnames(swc_df)
  if(firstNonNA>1){
    NAs$date <- swc_pentad[[1]]$time[1:(firstNonNA - 1)]
    swc_df <- rbind(NAs, swc_df)
  }


  output <- list('FD_time_series' = swc_df, 'FD_info' = fd_summary)

  return(output)
}
