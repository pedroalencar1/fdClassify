#' @title FD identification based on Noguera et al. (2020)
#'
#' @param vtime data frame column or vector containing \code{date} data
#' @param vprecipitation data frame column or vector containing daily precipitation
#' @param vet0 data frame column or vector containing daily ET0 (potential evapotranspiration). It can be obtained with the function \code(penman_day)
#' @param threshold numeric, indicate the lower limit to identify a FD (see description)
#'
#' @description The function uses the SPEI to identify a FD. The intentisication period is defined by a 4 week (one month) period with a SPEI reduction of 2. The SPEI value must go equal or below the threshold.
#' If \code{threshold} is not provided -1.28 (10 % quantile in a normal distribution) is assigned.
#'
#' @return The function returns a list with two data frames. One with weekly and detailed values from the function and a second with a summary of all events identified.
#'
#' @export
#' ET0 <- penman_day(vtime = df_d$time, vwind = df_d$wind_speed,
#'                   vvpd = df_d$vapor_p_def, vtemp = df_d$temperature,
#'                   vheatflux = (df_d$sensible_heat + df_d$latent_heat))
#'
#' fd_noguera <- Noguera2020(vtime = df_d$time,
#'                           vprecipitation = df_d$precipitation,
#'                           vet0 = ET0$et0)
#'
#' @examples
Noguera2020 <- function(vtime, vprecipitation, vet0, threshold = NA){

  deficit <- data.frame(time = vtime, deficit = vprecipitation - vet0)

  # cast deficit into weeks and arrange it (weeks according to Noguera)
  week.data <- f.week(deficit, kind = 'noguera')


  #get accumulated deficit over four weeks
  spei_list <- f.spei(week.data[[1]]$time, week.data[[1]]$var, n = 4)
  spei <- spei_list[[1]]

  spei$dif <- c(rep(NA,4), diff(spei$spei, lag = 4))

  # get threshold of SPEI
  # the threshold is deruved directly from the gaussian function
  if (is.na(threshold)){
    threshold <- qnorm(0.1)# the threshold is derived directly from the gaussian function
  }
  # qnorm(0.1) = -1.28
  ###### Another option, more strict, would be to get the threshold from the sampled data
  ###### and still stricter, get it for only the negative spei:
  # threshold <- quantile(spei$spei,0.1, na.rm = T)
  # neg.spei <- spei$spei[spei$spei<0] %>% .[complete.cases(.)] %>% sort(decreasing = T)
  # threshold <- quantile(neg.spei$spei,0.1, na.rm = T)

  #get onset
  spei$is.fd <- (spei$dif <= -2)*(spei$spei <= threshold)

  # fd.info <- spei[complete.cases(spei),]
  # fd.info$is.fd[1:3] <- 0 # fix issue for events identified to early in the series

  fd.info <- spei
  fd.info[is.na(fd.info)] <- 0 #get complete series and fill Na with 0

  # get complete event
  for (i in 2:nrow(fd.info)){
    # get duration after the onset
    if ((fd.info$is.fd[i-1] == 1) & (fd.info$spei[i] <= threshold)){
      fd.info$is.fd[i] = 1
    }
    # get duration before onset (intensification)
    if ((fd.info$is.fd[i-1] == 0) & (fd.info$is.fd[i] == 1)){
      fd.info$is.fd[(i-3):(i-1)] <- 1
    }
  }

  dur_aux1 <- rle(fd.info$is.fd==1)[1]
  dur_aux2 <- cumsum(dur_aux1$lengths)
  n <- length(dur_aux2)/2
  durations <- dur_aux1$lengths[c(2*(1:n))]
  positions_beg <- dur_aux2[c(2*(1:n)-1)]+1
  positions_end <- dur_aux2[c(2*(1:n))]

  fd.summary <- data.frame(event = 1:length(positions_end),
                           time_beg = fd.info$time[positions_beg],
                           time_end = fd.info$time[positions_end],
                           duration = durations)
  for (i in fd.summary$event){
    fd.summary$severity[i] <- sum(fd.info$spei[positions_beg[i]:positions_end[i]])
  }

  output <- list('spei_timeseries' = fd.info,
                 'FD_info' = fd.summary)

  return(output)
}
