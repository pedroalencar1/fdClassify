#' @title get_df_era5 - Generate DF from ERA5 raw (netcedf) data
#'
#' @param list_files vector containing the name of file(s)
#' @param lat latitude of study area (decimal degrees)
#' @param lon longitude of study area (decimal degrees)
#' @param soil_layers vector with on or more values between 1 and 4. This indicates which are the soil layers desired in the analysis. More information in the function description.
#'
#' @description The ERA5 provides 4 soil water content layers:
#'         Layer 1: 0 - 7cm,
#'         Layer 2: 7 - 28cm,
#'         Layer 3: 28 - 100cm,
#'         Layer 4: 100 - 289cm.
#'
#' @return The function returns a data frame with all variables of interest:
#'         Precipitation (tp)
#'         Temperature (t2m)
#'         Potential evapotranspiration (pev)
#'         actual evapotranspiration (e)
#'         volumetric soil water content (vswl1, vswl2, vswl3, vswl4)
#'
#' @export
#'
#' @examples
get_df_era5 <- function(list_files, lat, lon, soil_layers = c(1)){

  #Name of useful variables
  list_var <- c('tp', 'pev', 'e', 't2m')

  #get var name of provided soil layers
  for (i in 1:length(soil_layers)){
    list_var <- c(list_var, paste('swvl',soil_layers[i], sep = ''))
  }

  #get dates and hours
  times_nc <- get.nc.data(lat,lon,list_files,list_var[1], file = F)[,1]
  df <- data.frame(time = times_nc)

  #get series for all variables provided
  for ( i in 1:length(list_var)){
    df[,i+1] <- get.nc.data(lat,lon,list_files,list_var[i], file = F)[,2]
  }

  # rename columns and set correct units
  colnames(df) <- c('time', list_var)
  df$tp <- df$tp*1000 # convert to mm
  df$t2m <- df$t2m - 273.15 # convert to degrees C
  df$pev <- -df$pev*1000 # convert to mm and adjust direction
  df$e <- -df$e*1000 # convert to mm and adjust direction

  #remove negative values
  df$pev[df$pev < 0] <- 0
  df$e[df$e < 0] <- 0

  #convert soil moisture to percentage
  for (i in 1:length(soil_layers)){
    df[,5+i] <- df[,5+i]*100 # convert to %
  }

  # df$swvl1 <- df$swvl1*100 # convert to %
  # df$swvl2 <- df$swvl2*100 # convert to %
  # df$swvl3 <- df$swvl3*100 # convert to %
  # df$swvl4 <- df$swvl4*100 # convert to %

  # get daily data
  df_day_era <- prepare.nc(df[,c(1,2)], period = 'day', f = sum)[,1]

  # if (database == 'land'){ #ERA5-land accumulates precipitation and evaporation over the day!
  #   # tp, pev, e
  #   for ( i in 1:3){
  #     df_day_era[,i+1] <- prepare.nc(df[,c(1,i+1)], period = 'day', f = max)[,2]
  #   }
  # } else {
    # tp, pev, e
    for ( i in 1:3){
      df_day_era[,i+1] <- prepare.nc(df[,c(1,i+1)], period = 'day', f = sum)[,2]
    }
  # }

  # t2m, swvl's
  for ( i in 4:length(list_var)){
    df_day_era[,i+1] <- prepare.nc(df[,c(1,i+1)], period = 'day', f = mean)[,2]
  }

  colnames(df_day_era) <- c('time', list_var)

  # get data out of tible format and dates without hour
  df_day <- as.data.frame(df_day_era)
  df_day$time <- as.Date(df_day$time)

  return(df_day)

}
