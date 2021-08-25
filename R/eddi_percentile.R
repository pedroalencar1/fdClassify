
#' Function to calculate EDDI in percentiles on a week accumulation time
#'
#' @param vtime a data.frame column or vector with daily time stamps (Date type)
#' @param vet0 a data.frame column or vector with ordered daily ET0 values. They
#' can be obtained directly from reanalysis/models or from the functions
#' \code{penman_day} or \code{hargreaves_day}
#' @param dist string containing the distribution used to calibrate EDDI. It can
#' either be 'tukey' following the original method from Hobbings et al (2016)
#' or log-logist' according to Noguera et al. (2021)
#'
#' @return
#'
#' @export
#'
#' @examples
#' ET0 <- penman_day(vtime = df_d$time, vwind = df_d$wind_speed,
#'                   vvpd = df_d$vapor_p_def, vtemp = df_d$temperature,
#'                   vheatflux = (df_d$sensible_heat + df_d$latent_heat))
#'
#' percentiles_eddi <- eddi_percentile(vtime = de_tha_d$time, ET0$et0, dist = 'tukey')
#'
eddi_percentile <- function(vtime, vet0, dist = 'tukey'){

  nyear <- max(lubridate::year(vtime)) - min(lubridate::year(vtime)) + 1
  et0 <- data.frame(time = vtime, et0 = vet0)

  #get weeks
  et0$time <- as.Date(et0$time)

  week.et0.list <- f.week(et0, f = sum)
  series.et0 <- week.et0.list$week_timestamp
  week.et0 <- week.et0.list$week_matrix

  if (dist == 'log-logist'){

    #sort data in increasing order
    et0_sort <- t(apply(week.et0, 1, sort))
    et0_sort[et0_sort < 0] <- 0

    #get paramenters of log-logistic distribution
    parameters <- param_loglogist(et0_sort, n_param = 3)
    prob <- (1 + (parameters$a/(week.et0 - parameters$c))^parameters$b)^-1

    percentile <- round(prob*100) #result

  } else if(dist == 'tukey'){
    percentile <- matrix(NA, nrow = dim(week.et0)[1],ncol = dim(week.et0)[2])
    for (i in 1:nrow(week.et0)){
      # i = 1
      vet0 <- week.et0[i,]
      n <- length(vet0)
      empiric_p <- (1:n -0.33)/(n+0.33) #empiric probabilities (Tukey plotting)

      percentile[i,] <- round(empiric_p[rank(vet0)]*100)
    }

    v_et0 <- week.et0[1,]
    n <- length(v_et0)
    empiric_p <- (1:n -0.33)/(n+0.33) #empiric probabilities (Tukey plotting)
    veddi <- round(empiric_p[rank(v_et0)]*100)
  } else {
    stop("Wrong type of distributio. input either 'log-logist' or 'tukey'.")
  }

  return(percentile)
}




#
# time = de_tha_d$time
# et0 = penman_day(vtime = de_tha_d$time, vwind = de_tha_d$wind_speed, vtemp = de_tha_d$temperature,
#                   vvpd = de_tha_d$vapor_p_def, vheatflux = (de_tha_d$latent_heat + de_tha_d$sensible_heat))[,2]
#
#
# test <- eddi(vtime = time, vet0 = et0)
# test2 <- eddi(vtime = time, vet0 = et0, type = 'log-logist')
