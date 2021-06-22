# Function to clean the ESR data
Christian2020_clean_data <- function(vtime, vET0, vETa, threshold = 1){

  # #load required packages
  # library('tidyr')
  # library('dplyr')
  # library('readr')
  # library('tibbletime')
  # library('lubridate')
  # library('stringr')
  # library('runner')

  aux_year <- lubridate::year

  data_et <- data.frame(time = vtime, et0 = vET0, eta = vETa)

  data_et$esr <- data_et$eta/data_et$et0
  data_et$esr[is.infinite(data_et$esr)] <- NA
  data_et$esr[is.nan(data_et$esr)] <- NA
  data_et$esr[(data_et$esr) < 0] <- NA
  data_et$esr[(data_et$esr) > threshold] <- NA ### this is flexible. Christian used 2.

  count_NA <- sum(is.na(data_et$esr))

  data_et$issue <- is.na(data_et$esr)*1
  data_et$month <- lubridate::month(data_et$time)

  esr_pentad <- f.pentad(vtime = data_et$time, vvalue = data_et$esr, na_rm = T)

  #Check data quality for the growing season
  growing_season <- esr_pentad[[2]][19:60,]
  pentad_above_1 <- sum(growing_season > 1, na.rm = T) # if default it is 0.
  pentad_na <- sum(is.nan(growing_season)) # ESR = NaN
  max_esr <- max(growing_season, na.rm = T)

  print(paste('The series contain', count_NA, 'days with invalid values of ESR out of',
              nrow(data_et)))
  print(paste('Applying the cleaning algorithm of Christian, during the growing',
              'season (April to October):', pentad_above_1, 'pentads have ESR',
              'above one,',pentad_na,
              'pentads have no data and need to be interpolated.'))

  series.esr <- esr_pentad[[1]]
  series.esr$year <- aux_year(series.esr$time)
  series.esr$pentad <- rep(1:73,(max(series.esr$year)- min(series.esr$year) + 1))


  ### Fill NA values with linear interpolation

  ## 1) identify NA values and occurrence of consecutive pentads with NA
  series.esr$isna <- 0

  series.esr$var[is.na(series.esr$var)] <- NA


  for (i in 2:(nrow(series.esr))){
    if (is.na(series.esr$var[i])){
      series.esr$isna[i] = 1
    }
    if ( (is.na(series.esr$var[i])) & (is.na(series.esr$var[i-1])) ){
      series.esr$isna[i] <- series.esr$isna[i-1]+1
      # series.esr$isna[i-1] <- series.esr$isna[i]
    }
  }

  count1 <- nrow(series.esr)
  while (count1 > 1){
    if (is.na(series.esr$var[count1])){
      aux <- series.esr$isna[count1]

      if (aux > 1){
        for (i in 1:(aux-1)){
          series.esr$isna[count1-i] <- series.esr$isna[count1]
        }
        count1 <- count1 - aux
      } else{count1 <- count1 - 1}
    } else {count1 <- count1 - 1}
  }



  ## 2) implement linear interpolation, considering consecutive pentads with NA
  aux_count <- max(series.esr$isna) #if larger than 1 there are consecutive pentads
  # with NaN

  #limits of interpolation
  NA_beg <- min(which(!is.na(series.esr$var)))
  NA_end <- max(which(!is.na(series.esr$var)))

  for (i in NA_beg:NA_end){
    if (is.na(series.esr$var[i])){
      # calculates the interpolation increments
      aux_na <- series.esr$isna[i]
      dif <- (series.esr$var[i+aux_na] - series.esr$var[i-1])/(aux_na+1)
      for (j in 0:(aux_na-1)){
        series.esr$var[i+j] <- series.esr$var[i-1]+ dif*(j+1)

      }
    }
  }


  # get pentads in matrix form
  pentad.esr <- matrix(series.esr$var, nrow = 73, byrow = F)

  output <- list('pentad_timestamp' = series.esr, 'pentad_matrix' = pentad.esr)
  return(output)
}
