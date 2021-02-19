Noguera2020 <- function(vtime, vprecipitation, vtemperature, my_lat, threshold = NA){


  #load required packages
  library('tidyr')
  library('dplyr')
  library('readr')
  library('tibbletime')
  library('lubridate')
  library('stringr')
  library('runner')

  #set dataframe
  data.spei <- data.frame(time = vtime, precipitation = vprecipitation,
                          temperature = vtemperature)

  data.prec <- data.spei[,c(1,2)]
  data.temp <- data.spei[,c(1,3)]

  #get daily data and deficit
  precipitation <- prepare.nc(data.prec, period = 'day', f = sum)
  et0 <- hargreaves_day(vtime = vtime, vtemperature = vtemperature, my_lat)
  deficit <- data.frame(time = et0$time, deficit = precipitation$value - et0$et)

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

  spei$is.fd <- (spei$dif <= -2)*(spei$spei <= threshold)

  fd.info <- spei[complete.cases(spei),]

  # get complete event
  for (i in 2:nrow(fd.info)){
    if ((fd.info$is.fd[i-1] == 1) & (fd.info$spei[i] <= threshold)){
      fd.info$is.fd[i] = 1
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
