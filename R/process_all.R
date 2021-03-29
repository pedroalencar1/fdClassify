
process_all <- function(df_d,include_variables = T, data = 'station'){
  ############################# process all menthods###########################

  if (data = 'station'){
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
                                           threshold = 1) %>% Christian2020()
  print('Christian et al: Done')


  # 6. Osman et al. 2021

  fd_Osman  <- Osman2021(vtime = df_d$time, vswc = df_d$soil_water,
                         threshold = 20)
  print('Osman et al.: Done')

  } else if (data = 'reanalysis'){


    fd_Mo <- Mo2016(vtime = df_d$time, vprecipitation = df_d$tp,
                    vtemperature = df_d$t2m, vsoil_water = df_d$swvl1,
                    vevap = df_d$e)
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
                                             threshold = 1) %>% Christian2020()
    print('Christian et al: Done')


    # 6. Osman et al. 2021

    fd_Osman  <- Osman2021(vtime = df_d$time, vswc = df_d$swvl1,
                           threshold = 20)
    print('Osman et al.: Done')

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

  complete_series <- dplyr::left_join(dates,is.fd_mo, by= 'time') %>%
    dplyr::left_join(is.fd_ford, by= 'time') %>%
    dplyr::left_join(is.fd_pendergrass, by= 'tim e') %>%
    dplyr::left_join(is.fd_noguera, by= 'time') %>%
    dplyr::left_join(is.fd_christian, by= 'time') %>%
    dplyr::left_join(is.fd_osman, by= 'time')



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
                                 'Osman et al.')



  if(include_variables){

  complete_series$swc <- df_d$soil_water
  complete_series$et0 <- ET0$et0
  complete_series$eta <- ETa$eta
  complete_series$temperature <- df_d$temperature
  complete_series$precipitation <- df_d$precipitation

  swc_p20 <- data.frame(time = as.Date(fd_FordLabosier[[1]]$time),swc_p20 = fd_FordLabosier[[1]]$p20)
  SPEI <- data.frame(time = fd_noguera[[1]]$time, spei = fd_noguera[[1]]$spei)
  SESR <- data.frame(time = as.Date(fd_Christian[[1]]$time), sesr = fd_Christian[[1]]$sesr_value)
  EDDI <- data.frame(time = fd_Pendergrass[[1]]$time, eddi = fd_Pendergrass[[1]]$percentile)

  complete_series <- dplyr::left_join(complete_series,swc_p20, by= 'time') %>%
    dplyr::left_join(SPEI, by= 'time') %>%
    dplyr::left_join(SESR, by= 'time') %>%
    dplyr::left_join(EDDI, by= 'time')


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
                                 'Osman et al.',names[8:length(names)])
  }

  summary_list <- list(Mo = fd_Mo[[2]][[3]],
                       Ford = fd_FordLabosier[[2]],
                       Pendergrass = fd_Pendergrass[[2]],
                       Noguera = fd_noguera[[2]],
                       Christian = fd_Christian[[2]],
                       Osman = fd_Osman[[2]])

  output <- list(Series = complete_series, Summary = summary_list)

  return(output)

}
