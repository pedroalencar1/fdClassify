#' FD identification by Pendergrass et al. (2021)
#'
#' @description
#' Identifies Flash Drougt evetns using EDDI variations, as described in
#' Pendergrass et al. "Flash droughts present a new challenge for
#' subseasonal-to-seasonal prediction", 2021.
#'
#'
#' @param vtime a data.frame column or vector with daily time stamps (Date type)
#' @param vet0 a data.frame column or vector with ordered daily ET0 values. They
#' can be obtained directly from reanalysis/models or from the functions
#' \code{penman_day} or \code{hargreaves_day}
#' @param limit.down a numeric value, indicating the limit of recuperation after
#' onset. This criterion is additional, to flexibilise the original method.
#' We set it's value to 10 as default. TO run the original method (more restrict)
#' define it to 0.
#'
#' @details
#' This function is based on EDDI calculated according to Hobbings et al. (2016)
#' DOI: 10.1175/jhm-d-15-0121.1
#' It uses a simple empirical Tukey plotting position to assess EDDI percentiles.
#'
#' @return
#' Function \code{Pendergrass2020} retuns a list with two data frames.
#' 1) a complete time stamped series containing relevant variables and FD events;
#' 2) a summary of each event with its duration and interval of occurance.
#'
#' @export
#'
#' @examples
#' fd_Pendergrass <- Pendergrass2020(vtime = de_tha_d$time,
#'                                   vet0 = ET0$et0, limit.down = 10)
#'
Pendergrass2020 <- function(vtime, vet0, limit.down = 10){

  et0 <- data.frame(time = vtime, et0 = vet0)

  #get weeks
  et0$time <- as.Date(et0$time)

  week.et0.list <- f.week(et0, f = sum)
  series.et0 <- week.et0.list$week_timestamp
  week.et0 <- week.et0.list$week_matrix

  # get percentiles
  # we used the here the percentiles of EDDI as described in Hobbins 2016.

  percentile.eddi <- eddi_percentile(vtime = series.et0$time,
                                     vet0 = series.et0$var, dist = 'tukey')

  data.table <- data.frame(time = as.Date(series.et0$time),
                           percentile = c(percentile.eddi),
                           et0 = c(week.et0))

  data.table$dif_perc <- append(c(NA,NA),as.vector(diff(data.table$percentile,
                                                        lag = 2)))

  #Remove eventual NA in the beginning of the series
  firstNonNA <- min(which(!is.na(data.table$percentile)))
  data.table <- data.table[firstNonNA:nrow(data.table),]


  #Classification
  data.table$is.fd <- 0

  for (i in 3:(nrow(data.table)-2)){
    data.table$is.fd[i] <- (data.table$dif_perc[i] >= 50) *
      (data.table$percentile[i+1] - data.table$percentile[i] >= -limit.down) *
      (data.table$percentile[i+2] - data.table$percentile[i] > -limit.down)
  }


  #get correct durations and event number
  data.table$event <- 0
  count <- 0

  for (i in 3:(nrow(data.table)-1)){
    if (data.table$is.fd[i] ==1 & data.table$is.fd[i-1] ==0){
      count <- count+1
      data.table$event[i] <- count
      data.table$is.fd[i-1] = 1
      # data.table$is.fd[i-2] = 1
      limit <- data.table$percentile[i] - limit.down
      while (data.table$percentile[i+1] >= limit & i+1 <= nrow(data.table)){
        data.table$is.fd[i+1] <- 1
        i = i+1
      }
    }
  }


  #get positon end of droughts
  dur_aux1 <- rle(data.table$is.fd==0)[1]
  dur_aux2 <- cumsum(dur_aux1$lengths)
  n <- length(dur_aux2)/2
  durations <- dur_aux1$lengths[c(2*(1:n))]
  positions <- dur_aux2[c(2*(1:n))]



  fd.info <- data.table[positions,]
  fd.info$dur <- durations

  output <- list('ET0_timeseries' = data.table, 'FD_info' = fd.info)

  return(output)

}
