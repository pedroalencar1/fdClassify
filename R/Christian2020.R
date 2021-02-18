Christian2020 <- function(esr_list){

  series.esr <- esr_list[[1]]
  pentad.esr <- esr_list[[2]]

  # 1) get SESR and variations into pentads
  # ger SESR values and percentiles
  pentad.sesr_value <- t(apply(pentad.esr,1, f.anomaly))
  pentad.sesr_percentile <- t(apply(pentad.sesr_value,1, f.percentile))

  #get de Delta.SESR values and percentiles
  #  - first we get the differences using the diff function.
  # we definded Delta.SESR_p = SESR_p - SESR_p-1, so we also need to fix the
  # position of the results (for loop)

  pentad.delta_sesr_series <- c(pentad.sesr_value)
  pentad.delta_sesr_series <- diff(pentad.delta_sesr_series,lag = 1)

  pentad.delta_sesr <- rep(NA, length(pentad.delta_sesr_series))

  for (i in 2:nrow(series.esr)){
    pentad.delta_sesr[i] <- pentad.delta_sesr_series[i-1]
  }

  # - Now we cam get the Delta.SESR values and percentiles.
  pentad.delta_sesr <- matrix(pentad.delta_sesr, nrow = nrow(pentad.sesr_value),
                              ncol =  ncol(pentad.sesr_value), byrow = F)

  pentad.delta_sesr_value <- t(apply(pentad.delta_sesr,1, f.anomaly))
  pentad.delta_sesr_percentile <- t(apply(pentad.delta_sesr_value,1, f.percentile))

  ########## set unique dataframe
  series_sesr <- data.frame(time = series.esr$time, esr_value = series.esr$var,
                            sesr_value = c(pentad.sesr_value),
                            sesr_perc = c(pentad.sesr_percentile),
                            d.sesr_value = c(pentad.delta_sesr_value),
                            d.sesr_perc = c(pentad.delta_sesr_percentile))
  # View(series_sesr)
  #############################################################################
  # 2) apply rules 1 to 3

  #get years and pentads
  series_sesr$year <- year(series_sesr$time)
  series_sesr$pentad <- rep(1:73,(max(series_sesr$year)-
                                    min(series_sesr$year) + 1))

  # set two columns of auxiliar indexes
  # aux_delta indicates lines (pentads) that satisfy the variation threshold
  # aux_delta2 accumulates over the time series and allow us to assess if an
  #    event attends the minimum duration
  series_sesr$aux_delta <- abs((series_sesr$d.sesr_perc < 40) - 1)
  series_sesr$aux_delta2 <- series_sesr$aux_delta
  for (i in 3:nrow(series_sesr)){
    if(series_sesr$aux_delta[i] == 1 & series_sesr$aux_delta[i-1] == 1){
      series_sesr$aux_delta2[i] <- series_sesr$aux_delta2[i-1] + 1
    }
  }


  # this gets the duration of the consecutive accumulation of delta_sesr over
  #  periods from 1 to 18 pentads.
  max_ds <- matrix(data = 0, nrow = nrow(series_sesr), ncol = 30)
  for (i in 1:30){
    max_ds[,i] <- runner(series_sesr$aux_delta2, k = i, f = max)
  }

  # set zero values to one, avoid computation errors and does not change the output
  #  since only values above 6 are considered
  max_ds[max_ds==0] <-1

  # This step allows pentads with delta_sesr during FD, but no consecutive occurances
  series_sesr$dur <- 0
  for (i in 2:nrow(series_sesr)){
    # if (max(max_ds[i,], na.rm = T)==1){
    # series_sesr$dur[i] <- series_sesr$dur[i-1]+1
    # } else {
    series_sesr$dur[i] <- (min(which(max_ds[i,] != 1))-1)*(series_sesr$sesr_perc[i] < 20)
    # }
  }

  # View(max_ds)
  # selects only events which attenc the first three rules
  series_sesr_selec <- series_sesr[(!is.na(series_sesr$dur) & series_sesr$dur>5),]

  # View(series_sesr_selec)
  #############################################################################
  # 3) apply rules 1 to 3

  n_events <- nrow(series_sesr_selec)

  # set an auxiliar pentad matrix - simple way to avoid errors in FD occurring
  #   in the beginning of years
  pentad.delta_sesr_aux <- rbind(pentad.delta_sesr, pentad.delta_sesr)
  pentad.sesr_percentile <- t(apply(pentad.sesr_value,1, f.percentile))

  series_sesr_selec$av_change_perc <- NA
  for (i in 1:n_events){
    #identify potential FD location in the time series
    year_col <- series_sesr_selec$year[i] - min(series_sesr$year) + 1
    duration <- series_sesr_selec$dur[i]
    p_final <- series_sesr_selec$pentad[i] + 73
    p_inicial <- p_final - duration + 1

    # Assess percentile
    av_frame_delta_sesr <- colMeans(pentad.delta_sesr_aux[p_inicial:p_final,], na.rm = T)
    series_sesr_selec$av_change_perc[i] <- f.percentile(av_frame_delta_sesr)[year_col]
  }

  series_sesr_selec <- series_sesr_selec[series_sesr_selec$av_change_perc <= 30,]

  # Remove redundant events
  for (i in 2:nrow(series_sesr_selec)){
    if (series_sesr_selec$time[i] - series_sesr_selec$time[i-1] == 5){
      series_sesr_selec[i-1,] <- NA
    }
  }

  series_sesr_selec <- series_sesr_selec[complete.cases(series_sesr_selec),]

  series_sesr_output <- series_sesr[,-c(9,10)]
  series_sesr_output$dur[is.nan(series_sesr_output$dur)] <- NA

  # get series of 20 and 40 percentiles for visualization
  n_years <- max(year(series.esr$time)) - min(year(series.esr$time)) + 1
  p20 <- NA
  for (i in 1:73){
    p20[i] <- quantile(pentad.delta_sesr[i,], probs = 0.2, na.rm = T)
  }
  p20_series <- rep(p20,n_years)


  series_sesr_output$p20 <- p20_series

  #id flash droughts in the large df
  series_sesr_output$is.fd <- 0
  for (i in 1:nrow(series_sesr_output)){
    if (is.element(series_sesr_output$time[i], series_sesr_selec$time)){
      series_sesr_output$is.fd[i] <- 1
    }
  }

  output <- list('SESR_timeseries' = series_sesr_output,
                 'FD_info' = series_sesr_selec)
  return(output)
}
