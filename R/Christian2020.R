
#' FD identification method from Christian et al. (2020)
#'
#' @param esr_list The list output from the function \code{Christian2020_clean_data}
#'
#' @return
#' The function returns a list with two data frames. One with weekly and detailed values from the function and a second with a summary of all evetns identified.
#'
#' @export
#'
#' @examples
#' #' fd_Christian <- Christian2020_clean_data(vtime = df_d$time,
#' vET0 = ET0$et0, vETa = ETa$eta,
#' threshold = 2) %>% Christian2020()
#'
Christian2020 <- function(esr_list){

  series.esr <- esr_list[[1]]
  pentad.esr <- esr_list[[2]]


  # 1) get SESR and variations into pentads
  # ger SESR values and percentiles
  pentad.sesr_value <- t(apply(pentad.esr,1, f.anomaly))
  pentad.sesr_percentile <- t(apply(pentad.sesr_value,1, f.percentile))

  #get de Delta.SESR values and percentiles
  #  - first we get the differences using the diff function.
  pentad.delta_sesr_series <- c(pentad.sesr_value) %>% diff(lag = 1) %>% append(NA,.)


  # - Now we can get the Delta.SESR values and percentiles.
  ### - get the data in matrix format (cols with years)
  pentad.delta_sesr <- matrix(pentad.delta_sesr_series, nrow = nrow(pentad.sesr_value),
                              ncol =  ncol(pentad.sesr_value), byrow = F)

  max(pentad.delta_sesr, na.rm = T)
  pentad.delta_sesr_value <- t(apply(pentad.delta_sesr,1, f.anomaly))
  pentad.delta_sesr_percentile <- t(apply(pentad.delta_sesr_value,1, f.percentile))

  ########## set unique dataframe
  series_sesr <- data.frame(time = series.esr$time, esr_value = series.esr$var,
                            sesr_value = c(pentad.sesr_value),
                            sesr_perc = c(pentad.sesr_percentile),
                            d.sesr_value = c(pentad.delta_sesr_value),
                            d.sesr_perc = c(pentad.delta_sesr_percentile))
  #############################################################################


  # 2) apply rules 1 to 3

  #get years and pentads
  series_sesr$year <- lubridate::year(series_sesr$time)
  series_sesr$pentad <- rep(1:73,(max(series_sesr$year)-
                                    min(series_sesr$year) + 1))


  #remove NA values from the begining
  firstNonNA <- min(which(!is.na(series_sesr$sesr_perc)))
  bkp <- series_sesr[1:firstNonNA-1,]
  series_sesr <- series_sesr[firstNonNA:nrow(series_sesr),]
  ######

  # criterion 3 -> delta.SESR below 40 allowing 1 pentad recuperation
  aux_delta <- (series_sesr$d.sesr_perc <= 40)*1
  aux_delta[is.na(aux_delta)] <- 0

  #get positions where criterion 3 is true
  df_id <- data.frame(id_abs = rle(aux_delta)[[1]],
                      class = rle(aux_delta)[[2]])

  # remove contiguous events and update indexes
  df_id$id_acc <- runner::runner(df_id$id_abs, sum)
  df_id$id_new <- df_id$id_abs
  for (i in seq(4,nrow(df_id),2)){
    if(df_id$id_abs[i-1] == 1){
      df_id$id_new[i] <- df_id$id_abs[i] + df_id$id_new[i-2] + 1
      df_id$id_new[i-2] <- 0
      df_id$id_new[i-1] <- 0
    }
  }
  df_id$id_acc2 <- runner::runner(df_id$id_new, sum)

  # criterion 1 -> at least six pentads duration
  df_id_fd <- df_id[(df_id$id_new >= 6 & df_id$class == 1),]
  fd_events <- series_sesr[df_id_fd$id_acc2,]
  fd_events$dur <- df_id_fd$id_new

  # criterion 2 -> final SESR percentile lower than 20th.
  series_sesr_selec <- fd_events[fd_events$sesr_perc <= 20,]


  #############################################################################
  # 3) apply rule 4

  n_events <- nrow(series_sesr_selec)

  # set an auxiliar pentad matrix - simple way to avoid errors in FD occurring
  #   in the beginning of years
  pentad.delta_sesr_aux <- rbind(pentad.delta_sesr, pentad.delta_sesr)
  pentad.sesr_percentile <- t(apply(pentad.sesr_value,1, f.percentile))

  series_sesr_selec$av_change_perc <- NA
  for (i in 1:n_events){
    # i = 1
    #identify potential FD location in the time series
    year_col <- series_sesr_selec$year[i] - min(series_sesr$year) + 1 # set index from 1 to nyear
    duration <- series_sesr_selec$dur[i]
    p_final <- series_sesr_selec$pentad[i] + 73
    p_inicial <- p_final - duration + 1

    # Assess percentile
    av_frame_delta_sesr <- colMeans(pentad.delta_sesr_aux[p_inicial:p_final,], na.rm = T)
    series_sesr_selec$av_change_perc[i] <- f.percentile(av_frame_delta_sesr)[year_col]
    # print(i)
  }

  series_sesr_selec <- series_sesr_selec[series_sesr_selec$av_change_perc <= 30,]

  # Remove redundant events (if any)
  for (i in 2:nrow(series_sesr_selec)){
    if (series_sesr_selec$time[i-1] >= series_sesr_selec$time[i] - 86400*5*series_sesr_selec$dur[i]){
      series_sesr_selec[i-1,] <- NA
    }
  }


  series_sesr_selec <- series_sesr_selec[complete.cases(series_sesr_selec),]
  series_sesr_selec_duration <- series_sesr_selec[,c(1,9)]

  series_sesr_output <- series_sesr[,-c(9,10)]
  series_sesr_output <- dplyr::left_join(series_sesr_output,series_sesr_selec_duration, by = 'time')

  series_sesr_output$dur[is.nan(series_sesr_output$dur)] <- NA

  #recompose the time series with NA
  if(nrow(bkp > 0)){
    bkp$dur <- NA
    series_sesr_output <- rbind(bkp,series_sesr_output)
  }

  ##############

  # get series of 20 and 40 percentiles for visualization
  n_years <- max(lubridate::year(series.esr$time)) - min(lubridate::year(series.esr$time)) + 1
  p20 <- NA
  for (i in 1:73){
    p20[i] <- quantile(pentad.delta_sesr[i,], probs = 0.2, na.rm = T)
  }
  p20_series <- rep(p20,n_years)


  series_sesr_output$p20 <- p20_series

  #id flash droughts in the large df and correct durations
  series_sesr_output$is.fd <- 0
  for (i in 1:nrow(series_sesr_output)){
    if (is.element(series_sesr_output$time[i], series_sesr_selec$time)){
      dur_aux <- series_sesr_output$dur[i] - 1
      series_sesr_output$is.fd[(i-dur_aux):i] <- 1
    }
  }



  output <- list('SESR_timeseries' = series_sesr_output,
                 'FD_info' = series_sesr_selec)
  return(output)
}
