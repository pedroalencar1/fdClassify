# pacman::p_load('ncdf4','ncdf4.helpers','PCICt','ggplot2','tidyr','dplyr','readr'
#                ,'raster','tibbletime','lubridate','RColorBrewer','stringr','knitr'
#                ,'tinytex','data.table','runner', 'reshape2', 'tidyquant', 'SPEI'
#                , 'ts', 'expss', 'ggforce')
#
#
# WD <- "C:/Users/pedro/OneDrive/@DOUTORADO/@@TU-Berlin/@Artigos/CAP 4/R"
# setwd(WD)
# de.tha <- read.csv('FLX_DE-Tha_FLUXNET2015_SUBSET_HH_1996-2014_1-4.csv')
# df <- de.tha
# data.et <- df[,c('TIMESTAMP_START','TA_F')]
# data.et$TIMESTAMP_START <- ymd_hm(data.et$TIMESTAMP_START)
# data.et <- na_if(data.et,-9999)
# colnames(data.et) <- c('time','temp')
# my_lat <- 50.963611
# et0 <- hargreaves_day(data.et, my_lat = my_lat)
#
# #####
#
# de_tha.list <- Pendergrass2020(et0)
# de_tha.et0_series <- de_tha.list[[1]]
# de_tha.et0fd_info <- de_tha.list[[2]]
# nrow(de_tha.et0fd_info)


Pendergrass2020 <- function(data.et0, limit.up = 10){

 # if(!require('lubridate'))install.packages('lubridate')
 # if(!require('tibbletime'))install.packages('tibbletime')
 # if(!require('dplyr'))install.packages('dplyr')
 # if(!require('stats'))install.packages('stats')

  #get pentads
  et0 <- as.data.frame(data.et0)
  et0$time <- as.Date(et0$time)

  pentad.et0.list <- f.pentad(et0)
  series.et0 <- pentad.et0.list$pentad_timestamp
  pentad.et0 <- pentad.et0.list$pentad_matrix

  # get percentiles
  percentile.et0 <- t(apply(pentad.et0,1, f.percentile))
  ts.percentile.et0 <- ts(c(percentile.et0), frequency = 73, start = beg)

  data.table <- data.frame(time = time(ts.percentile.et0),
                           index = 1:length(percentile.et0),
                           percentile = c(percentile.et0),
                           et0 = c(pentad.et0))

  #Remove eventual NA in the beginning of the series
  firstNonNA <- min(which(!is.na(data.table$percentile)))
  data.table <- data.table[firstNonNA:nrow(data.table),]

  data.table$delta <- NA

  auxiliar1 <- unlist(lapply(4:nrow(data.table),
                             function(i) data.table$percentile[i] -
                               data.table$percentile[i-3])) %>%
    c(rep(NA,3),.)

  data.table$delta <- auxiliar1

  #Classification
  data.table$fd <- 0
  limit.upwards <- limit.up #max recuperation over sustain period

  for (i in 4:(nrow(data.table)-3)){
    data.table$fd[i] <- (data.table$delta[i] <= -50) *
      (data.table$percentile[i+1] - data.table$percentile[i] <= limit.upwards) *
      (data.table$percentile[i+2] - data.table$percentile[i] <= limit.upwards) *
      (data.table$percentile[i+3] - data.table$percentile[i] <= limit.upwards)
  }

  fd.info <- data.table[data.table$fd == 1,]

  output <- list('ET0_timeseries' = data.table, 'FD_info' = fd.info)

  return(output)

}
