
#' FD identificaiton method proposed by the Author (Pedro Alencar)
#'
#' @param vtime data frame column or vector containing \code{date} data
#' @param vprecipitation data frame column or vector containing daily precipitation (mm.day-1)
#' @param vet0 data frame column or vector containing daily potential evapotranspiration (Penman-Monteith - mm.day-1)
#' @param crit a vector with four elements indicating the thresholds (more in details).
#' @param months a vector with two elements, the first and last months in the analysis (definition of growing season)
#'
#' @details
#' The method tracks the variation (slope) of the precipitation and potential evapotranspiration.
#' It uses intra-year limits and the anomaly to the time series to identify exceptionally dry events.
#'
#' The \code{crit} vector contains 4 elements:
#'     c1 = accumulation time for slope (in weeks) - Default = 4
#'     c2 = threshold for combined anomaly (sum) - Default = 2
#'     c3 = latency period of negative SPEI (in weeks) - Default = 8
#'     c4 = allows positive SPEI over latency period - Default = 2
#'
#' @return
#' The function returns a list with two data frames. One with weekly and detailed values from the function and a second with a summary of all evetns identified.
#'
#' @export
#'
#' @examples
#' fd_Alencar <- alencar2021(vtime = df_d$time,
#'                           vprecipitation = df_d$precipitation,
#'                           vet0 = ET0$et0)
#'
#'


alencar2021 <- function(vtime, vprecipitation, vet0, crit = c(4, 2, 8, 2), months = c(3,10)){

  ###### calculate SPEI #############
  deficit <- data.frame(time = vtime, deficit = vprecipitation - vet0)

  # cast deficit into weeks and arrange it (weeks according to Noguera)
  week.data <- f.week(deficit, kind = 'standard')

  #get accumulated deficit over four weeks
  spei_list <- f.spei(week.data[[1]]$time, week.data[[1]]$var, n = crit[1])
  spei <- spei_list[[1]]$spei
  ###################################

  df <- data.frame(Date = vtime, precipitation = vprecipitation, et0 = vet0)
  # df$month <- lubridate::month(df$time)
  ##########################

  #get interval MAMJJASO
  df$precipitation[lubridate::month(df$Date) > months[2] | lubridate::month(df$Date) <  months[1]] <- NA
  df$et0[lubridate::month(df$Date) >  months[2] | lubridate::month(df$Date) <  months[1]] <- NA

  df_precipitation <- dplyr::select(df,Date, precipitation)
  df_et0 <- dplyr::select(df,Date, et0)

  #colapse interval into weeks
  week_et0 <- f.week(df_et0, f = sum, kind = 'standard')[[2]]
  week_precipitation <- f.week(df_precipitation, f = sum)[[2]]
  week_series <-f.week(df_et0, f = sum)[[1]][,1]


  # note that in the first three weeks of march are we cant accumulate 4 weeks to assess
  #   slope. The method works starting on the 22nd of March, in the first day of Spring
  slope_4w_et0 <- apply(week_et0,2, FUN = function(x) runner::runner(x,sum, k = crit[1]))
  slope_4w_et0[1:3,] <- NA

  slope_4w_prec  <- apply(week_precipitation,2, FUN = function(x) runner::runner(x,sum, k = crit[1]))
  slope_4w_prec[1:3,] <- NA

  slope_anomaly_et0 <- t(apply(slope_4w_et0,1, f.anomaly))
  slope_anomaly_prec <- t(apply(slope_4w_prec,1, f.anomaly))


  df_complete <- data.frame(Date = as.Date(week_series$time), precip = c(week_precipitation),
                            et0 = c(week_et0),
                            slope_precip = c(slope_4w_prec),
                            slope_et0 = c(slope_4w_et0),
                            anomaly_slope_prec = c(slope_anomaly_prec),
                            anomaly_slope_et0 = c(slope_anomaly_et0),
                            spei = spei)


  #get events
  df_complete$spei_sign <- runner::runner(sign(df_complete$spei),sum, k = crit[3])

  df_complete$is.fd <- (df_complete$anomaly_slope_et0 - df_complete$anomaly_slope_prec >= crit[2])*
    (df_complete$spei_sign>(crit[4] - crit[3])) * (df_complete$spei <= -1)

  df_complete[is.na(df_complete)] <- 0

  #get duration into the intensfication fase
  for (i in 2:nrow(df_complete)){
    if(df_complete$is.fd[i-1] == 0 & df_complete$is.fd[[i]] == 1){
      df_complete$is.fd[(i-3):(i-1)] <- 1
    }
  }


  #join events that are 3 or less weeks apart
  index_aux3 <- rle(df_complete$is.fd)$lengths
  index_aux4 <- runner::runner(index_aux3, f = sum)

  index_aux3 <- index_aux3[seq(1,length(index_aux3),2)]
  index_aux4 <- index_aux4[seq(1,length(index_aux4),2)]

  if (min(index_aux3) < 4){
    length_short_breaks <- index_aux3[which(index_aux3 < 4)]
    position_short_breaks <- index_aux4[which(index_aux3 < 4)]

    for (i in 1:length(length_short_breaks)){
      i = 1
      beg_break <- position_short_breaks[i] - length_short_breaks[i] + 1
      for (j in beg_break:position_short_breaks[i]){
        df_complete$is.fd[j] <- 1
      }
    }
  }


  #summary data frame
  fd_summary <- NULL
  for (i in 1:(nrow(df_complete)-1)){
    if (df_complete$is.fd[i] == 1 & df_complete$is.fd[i+1] == 0){
      fd_summary <- rbind(fd_summary, df_complete[i,])
    }
  }

  output <- list(series = df_complete, summary = fd_summary)
  return(output)

}







