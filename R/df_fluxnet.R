#' @title get_df_fluxnet - Generate DF from FLUXNET raw data
#'
#' @param filename name of fluxnet file
#' @param timestep what is the analysed timestep, if daily or hourly
#' @param soil_level Which is the sensor index (depends on the station, please pay attention to the .csv file)
#'
#' @description
#' Builds data frame to be used as input in all functions from the raw FLUXNET data (.csv)
#'
#' @return
#'
#' @export df
#'
#' @examples
#'
get_df_fluxnet <- function(filename, timestep, soil_level = 1){

  #read csv file
  station <- utils::read.csv(filename)

  # set standard names for the timestep
  set_day <- c('day', 'daily', 'd', 'dd')
  set_hour <- c('hour', 'hourly', 'sub-daily', 'subdaily', 'h', 'half-hour', 'half', 'halfhour',
                'half hour', '30 min', '30-min', '30', 'hh')

  #get column with soil water content to be used. Provided by the user.
  # usually vary from 1 to 4
  # heights are not uniform and should be confirmd by the user in the FLUSNET2015 documentation
  pos_soil <- which(colnames(station) == paste('SWC_F_MDS_',soil_level,sep =''))

  if (tolower(timestep) %in% set_day){
    df <- data.frame(time = lubridate::ymd(station$TIMESTAMP),
                     precipitation = station$P_F,
                     temperature = station$TA_F,
                     wind_speed = station$WS_F,
                     pressure_atm = station$PA_F,
                     vapor_p_def = station$VPD_F/10, #from hPa to kPa
                     sensible_heat = station$H_F_MDS,
                     latent_heat = station$LE_F_MDS,
                     soil_water = station[,pos_soil])

  } else if (tolower(timestep) %in% set_hour){
    df <- data.frame(time = lubridate::ymd_hm(station$TIMESTAMP_START),
                     precipitation = station$P_F,
                     temperature = station$TA_F,
                     wind_speed = station$WS_F,
                     pressure_atm = station$PA_F,
                     vapor_p_def = station$VPD_F/10, #from hPa to kPa
                     sensible_heat = station$H_F_MDS,
                     latent_heat = station$LE_F_MDS,
                     soil_water = station[,pos_soil])
  }

  df[df == -9999] <- NA #missing values as NA

  return(df)
}


