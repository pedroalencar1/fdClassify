
#' Process all methods with default values
#'
#' @param df_d a data frame with all relevant variables. It can be obtained using
#' the functions \code{get_df_era5} or \code{get_df_fluxnet}
#' @param include_variables Boolean,informs with all variables should be included
#' in the output
#' @param data string value, either 'station' or 'reanalysis'.
#'
#' @details
#' The function run all methods with their default thresholds
#'
#' @return
#' The function returns a list with two data frames. One with daily and detailed values for all methods and a list with one data frame for each method, each containing a summary of all identified events.
#'
#' @export
#'
#' @examples
#'   all_methods <- process_all(de_tha_d,include_variables = T, data = 'station')


process_all <- function(df_d,include_variables = T, data = 'station'){
  ############################# process all menthods###########################

  if (data == 'station'){
    #0. Evapotranspirations
    ET0 <- penman_day(vtime = df_d$time, vwind = df_d$wind_speed,
                      vvpd = df_d$vapor_p_def, vtemp = df_d$temperature,
                      vheatflux = (df_d$sensible_heat + df_d$latent_heat))

    ETa <- actual_evap_day(vtime = df_d$time, vlatent_heat = df_d$latent_heat,
                           vtemperature = df_d$temperature)



    # 1. Mo and Lettenmeier 2016

    fd_Mo <- Mo2016(vtime = df_d$time, vprecipitation = df_d$precipitation,
                    vtemperature = df_d$temperature, vsoil_water = df_d$soil_water,
                    vlatent_heat = df_d$latent_heat)
    print('Mo and Lettenmeier: Done')

    # 2. Ford and Labosier 2017

    fd_FordLabosier <- FordLabosier2017(vtime = df_d$time,
                                        vswc = df_d$soil_water)
    print('Ford and Labosier: Done')


    # 3. Pendergrass et al. 2020
    fd_Pendergrass <- Pendergrass2020(vtime = df_d$time,
                                      vet0 = ET0$et0, limit.down = 10)
    print('Pendergrass et al.: Done')


    # 4. Noguera et al. 2020
    fd_noguera <- Noguera2020(vtime = df_d$time,
                              vprecipitation = df_d$precipitation,
                              vet0 = ET0$et0)
    print('Noguera et al: Done')



    # 5. Christian et al. 2020
    fd_Christian <- Christian2020_clean_data(vtime = df_d$time,
                                             vET0 = ET0$et0, vETa = ETa$eta,
                                             threshold = 2) %>% Christian2020()
    print('Christian et al: Done')


    # 6. Osman et al. 2021

    fd_Osman  <- Osman2021(vtime = df_d$time, vswc = df_d$soil_water,
                           threshold = 20)
    print('Osman et al.: Done')

    # 7. Alencar et al. 2021

    fd_Alencar <- alencar2021(vtime = df_d$time,
                              vprecipitation = df_d$precipitation,
                              vet0 = ET0$et0)

    print('Alencar et al.: Done')

    # 8. Modified Ford and Labosier - Growing season only

    fd_FordLabosier_gs <- FordLabosier_gs(vtime = df_d$time,
                                          vswc = df_d$soil_water)
    print('Modified Ford and Labosier: Done')

    # 9. Multi-criteria FD classification

    fd_Multi_crit <- multicriteria_fd(vtime = df_d$time,
                                      vtemp = df_d$temperature,
                                      vprec = df_d$precipitation,
                                      vet0 = ET0$et0,
                                      veta = ETa$eta,
                                      score = 0.6,
                                      d_score = 0.2)

    print('Multi-criteria: Done')


  } else if (data == 'reanalysis'){
    #get new columns for output
    df_d$soil_water <- df_d$swvl1
    df_d$temperature <- df_d$t2m
    df_d$precipitation <- df_d$tp

    # adjust signal and remove dew
    # df_d$pev <- -df_d$pev
    # df_d$pev[df_d$pev<0] <- 0
    #
    # df_d$e <- -df_d$e
    # df_d$e[df_d$e<0] <- 0

    #more columns
    ET0 <- data.frame(time = df_d$time, et0 = df_d$pev)
    ETa <- data.frame(time = df_d$time, eta = df_d$e)

    fd_Mo <- Mo2016(vtime = df_d$time, vprecipitation = df_d$tp,
                    vtemperature = df_d$t2m, vsoil_water = df_d$swvl1,
                    vevap = df_d$e, flux_data = F)
    print('Mo and Lettenmeier: Done')

    # 2. Ford and Labosier 2017

    fd_FordLabosier <- FordLabosier2017(vtime = df_d$time,
                                        vswc = df_d$swvl1)
    print('Ford and Labosier: Done')


    # 3. Pendergrass et al. 2020
    fd_Pendergrass <- Pendergrass2020(vtime = df_d$time,
                                      vet0 = df_d$pev, limit.down = 10)
    print('Pendergrass et al.: Done')


    # 4. Noguera et al. 2020
    fd_noguera <- Noguera2020(vtime = df_d$time,
                              vprecipitation = df_d$tp,
                              vet0 = df_d$pev)
    print('Noguera et al: Done')



    # 5. Christian et al. 2020
    fd_Christian <- Christian2020_clean_data(vtime = df_d$time,
                                             vET0 = df_d$pev, vETa = df_d$e,
                                             threshold = 2) %>% Christian2020()
    print('Christian et al: Done')


    # 6. Osman et al. 2021

    fd_Osman  <- Osman2021(vtime = df_d$time, vswc = df_d$swvl1,
                           threshold = 20)
    print('Osman et al.: Done')


    # 7. Alencar et al. 2021
    fd_Alencar <- alencar2021(vtime = df_d$time,
                              vprecipitation = df_d$tp,
                              vet0 = df_d$pev)
    print('Alencar et al: Done')

    # 8. Modified Ford and Labosier - Growing season only

    fd_FordLabosier_gs <- FordLabosier_gs(vtime = df_d$time,
                                          vswc = df_d$swvl1)
    print('Modified Ford and Labosier: Done')

    # 9. Multi-criteria FD classification

    fd_Multi_crit <- multicriteria_fd(vtime = df_d$time,
                                      vtemp = df_d$t2m,
                                      vprec = df_d$tp,
                                      vet0 = df_d$pev,
                                      veta = df_d$e,
                                      score = 95)

    print('Multi-criteria: Done')

  } else {'no valid dataset'}

  ############################# joint dataframe ###########################

  #get event occurences for each method
  dates <- data.frame(time = df_d$time)

  is.fd_mo <- data.frame(time = as.Date(fd_Mo[[1]]$time),
                         mo = fd_Mo[[1]]$is.fd)
  is.fd_ford <- data.frame(time = as.Date(fd_FordLabosier[[1]]$time),
                           ford = fd_FordLabosier[[1]]$is.fd)
  is.fd_pendergrass <- data.frame(time = as.Date(fd_Pendergrass[[1]]$time),
                                  pendergrass = fd_Pendergrass[[1]]$is.fd)
  is.fd_noguera <- data.frame(time = as.Date(fd_noguera[[1]]$time),
                              noguera = fd_noguera[[1]]$is.fd)
  is.fd_christian <- data.frame(time = as.Date(fd_Christian[[1]]$time),
                                christian = fd_Christian[[1]]$is.fd)
  is.fd_osman <- data.frame(time = as.Date(fd_Osman[[1]]$time),
                            osman = fd_Osman[[1]]$is.fd)
  is.fd_alencar <- data.frame(time = as.Date(fd_Alencar[[1]]$Date),
                            osman = fd_Alencar[[1]]$is.fd)
  is.fd_ford_gs <- data.frame(time = as.Date(fd_FordLabosier_gs[[1]]$time),
                              osman = fd_FordLabosier_gs[[1]]$is.fd)
  is.fd_Multi <- data.frame(time = as.Date(fd_Multi_crit[[1]]$time),
                              osman = fd_Multi_crit[[1]]$is.fd)

  #get anomalies

  data_et0 <- data.frame(time = df_d$time, var =  ET0$et0)
  data_eta<- data.frame(time = df_d$time, var = ETa$eta)
  data_temp <- data.frame(time = df_d$time, var = df_d$temperature)
  data_prec <- data.frame(time = df_d$time, var =df_d$precipitation)


  pent_et0 <- f.pentad(vtime = data_et0$time,
                       vvalue = data_et0$var)
  pent_eta <- f.pentad(vtime = data_eta$time,
                       vvalue = data_eta$var)
  pent_temp <- f.pentad(vtime = data_temp$time,
                        vvalue = data_temp$var)
  pent_prec <- f.pentad(vtime = data_prec$time,
                        vvalue = data_prec$var)
  anom_et0 <- data.frame(time =  as.Date(pent_et0[[1]][,1]$time, format = "%Y-%m-%d"),
                         anom_et0 = c(t(apply(pent_et0[[2]],1, f.anomaly))))
  anom_eta <- data.frame(time = as.Date(pent_eta[[1]][,1]$time, format = "%Y-%m-%d"),
                         anom_eta = c(t(apply(pent_eta[[2]],1, f.anomaly))))
  anom_temp <- data.frame(time = as.Date(pent_temp[[1]][,1]$time, format = "%Y-%m-%d"),
                          anom_temp = c(t(apply(pent_temp[[2]],1, f.anomaly))))
  anom_prec <- data.frame(time = as.Date(pent_prec[[1]][,1]$time, format = "%Y-%m-%d"),
                          anom_prec = c(t(apply(pent_prec[[2]],1, f.anomaly))))


  ###########################################


  #jointed Data Frame

  complete_series <- dplyr::left_join(dates,is.fd_mo, by= 'time') %>%
    dplyr::left_join(is.fd_ford, by= 'time') %>%
    dplyr::left_join(is.fd_pendergrass, by= 'time') %>%
    dplyr::left_join(is.fd_noguera, by= 'time') %>%
    dplyr::left_join(is.fd_christian, by= 'time') %>%
    dplyr::left_join(is.fd_osman, by= 'time') %>%
    dplyr::left_join(is.fd_alencar, by= 'time') %>%
    dplyr::left_join(is.fd_ford_gs, by= 'time') %>%
    dplyr::left_join(is.fd_Multi, by= 'time') %>%
    dplyr::left_join(anom_et0, by= 'time') %>%
    dplyr::left_join(anom_eta, by= 'time') %>%
    dplyr::left_join(anom_temp, by= 'time') %>%
    dplyr::left_join(anom_prec, by= 'time')


  #fill NA values completing series
  n_data <- ncol(complete_series) - 1

  non_na <- list(NA)
  # locate non NAs in the wanted column
  for (i in 1:n_data){
    non_na[[i]] <-  which(!is.na(complete_series[,(i+1)]))
  }


  # define the function replace_NAs_custom with next non NA
  replace_NAs_custom <- function(i, col){
    if(is.na(col[i])){
      col[i] <- col[min(not_na[not_na > i] )]
    }
    return(col[i] )
  }

  for (i in 1:n_data){
    not_na <- non_na[[i]]
    complete_series[,(i+1)] <- unlist(lapply(1:nrow(complete_series),
                                             replace_NAs_custom,complete_series[,(i+1)]))
  }

  colnames(complete_series) <- c('time', 'Mo and Lettenmeier',
                                 'Ford and Labosier', 'Pendergrass et al.',
                                 'Noguera et al.','Christian et al.',
                                 'Osman et al.', 'Alencar et al.',
                                 'Ford  Modified', 'Multi-criteria',
                                 'et0_anomaly', 'eta_anomaly',
                                 'temperature_anomaly', 'precipitation_anomaly')



  if(include_variables){

    complete_series$swc <- df_d$soil_water
    complete_series$et0 <- ET0$et0
    complete_series$eta <- ETa$eta
    complete_series$temperature <- df_d$temperature
    complete_series$precipitation <- df_d$precipitation

    swc_p20 <- data.frame(time = as.Date(fd_FordLabosier[[1]]$time),swc_p20 = fd_FordLabosier[[1]]$p20)
    swc_p40 <- data.frame(time = as.Date(fd_FordLabosier[[1]]$time),swc_p40 = fd_FordLabosier[[1]]$p40)
    swc_p50 <- data.frame(time = as.Date(fd_FordLabosier[[1]]$time),swc_p50 = fd_FordLabosier[[1]]$p50)
    SPEI <- data.frame(time = fd_noguera[[1]]$time, spei = fd_noguera[[1]]$spei)
    SESR <- data.frame(time = as.Date(fd_Christian[[1]]$time), sesr = fd_Christian[[1]]$sesr_value)
    EDDI <- data.frame(time = fd_Pendergrass[[1]]$time, eddi = fd_Pendergrass[[1]]$percentile)
    an_slope_prec <- data.frame(time = as.Date(fd_Alencar[[1]]$Date), an_slope_prec = fd_Alencar[[1]]$anomaly_slope_prec)
    an_slope_et0 <- data.frame(time = as.Date(fd_Alencar[[1]]$Date), an_slope_et0 = fd_Alencar[[1]]$anomaly_slope_et0)
    score <- data.frame(time = as.Date(fd_Multi_crit[[1]]$time),
                        score_perc = fd_Multi_crit[[1]]$score_percentile_global,
                        score = fd_Multi_crit[[1]]$score)

    complete_series <- dplyr::left_join(complete_series,swc_p20, by= 'time') %>%
      dplyr::left_join(swc_p40, by= 'time') %>%
      dplyr::left_join(swc_p50, by= 'time') %>%
      dplyr::left_join(SPEI, by= 'time') %>%
      dplyr::left_join(SESR, by= 'time') %>%
      dplyr::left_join(EDDI, by= 'time')%>%
      dplyr::left_join(an_slope_prec, by= 'time') %>%
      dplyr::left_join(an_slope_et0, by= 'time') %>%
      dplyr::left_join(score, by= 'time')

    #repeat the process of NA filling
    n_data <- ncol(complete_series) - 1

    non_na <- list(NA)
    # locate non NAs in the wanted column
    for (i in 1:n_data){
      non_na[[i]] <-  which(!is.na(complete_series[,(i+1)]))
    }


    # define the function replace_NAs_custom with next non NA
    replace_NAs_custom <- function(i, col){
      if(is.na(col[i])){
        col[i] <- col[min(not_na[not_na > i] )]
      }
      return(col[i] )
    }

    for (i in 1:n_data){
      not_na <- non_na[[i]]
      complete_series[,(i+1)] <- unlist(lapply(1:nrow(complete_series),
                                               replace_NAs_custom,complete_series[,(i+1)]))
    }


    names <- colnames(complete_series)

    colnames(complete_series) <- c('Date', 'Mo and Lettenmeier',
                                   'Ford and Labosier', 'Pendergrass et al.',
                                   'Noguera et al.','Christian et al.',
                                   'Osman et al.', 'Alencar et al.','Ford  Modified',
                                   'Multi-criteria',names[11:length(names)])
  } else{
    complete_series <- complete_series[,c(1:10)]
  }

  summary_list <- list(Mo = fd_Mo[[2]][[3]],
                       Ford = fd_FordLabosier[[2]],
                       Pendergrass = fd_Pendergrass[[2]],
                       Noguera = fd_noguera[[2]],
                       Christian = fd_Christian[[2]],
                       Osman = fd_Osman[[2]],
                       Alencar = fd_Alencar[[2]],
                       Ford_modified = fd_FordLabosier_gs[[2]],
                       Multi_crit = fd_Multi_crit[[4]])

  output <- list(Series = complete_series, Summary = summary_list)

  return(output)

}
