# Function to clean the ED data
Li2020_clean_data <- function(vtime, vET0, vETa, threshold = 0){

  data_et <- data.frame(time = vtime, et0 = vET0, eta = vETa)

  data_et$ed <- data_et$eta - data_et$et0
  data_et$ed[is.infinite(data_et$ed)] <- NA
  data_et$ed[is.nan(data_et$ed)] <- NA
  data_et$ed[(data_et$ed) > threshold] <- NA ### this is flexible. Christian used 2.

  count_NA <- sum(is.na(data_et$ed))

  data_et$issue <- is.na(data_et$ed)*1
  data_et$month <- month(data_et$time)

  ed_pentad <- f.pentad(data.frame(time = data_et$time, ed = data_et$ed)
                        , na_rm = T)

  #Check data quality for the growing season
  growing_season <- ed_pentad[[2]][19:60,]
  pentad_above_1 <- sum(growing_season > 1, na.rm = T) #ED > 0 -> 0, since we set the threshold to 1.
  pentad_na <- sum(is.nan(growing_season)) # ED = NaN
  max_ed <- max(growing_season, na.rm = T)

  print(paste('The series contain', count_NA, 'days with invalid values of ED out of',
              nrow(data_et)))
  print(paste('Applying the cleaning algorithm of Christian, during the growing',
              'season (April to October):', pentad_above_1, 'pentads have ED',
              'above 0,',pentad_na,
              'pentads have no data and need to be interpolated.'))

  series.ed <- ed_pentad[[1]]
  series.ed$year <- year(series.ed$time)
  series.ed$pentad <- rep(1:73,(max(series.ed$year)- min(series.ed$year) + 1))


  ### Fill NA values with linear interpolation

  ## 1) identify NA values and occurrence of consecutive pentads with NA
  series.ed$isna <- 0

  for (i in 2:(nrow(series.ed))){
    if (is.na(series.ed$var[i])){
      series.ed$isna[i] = 1
    }
    if ( (is.na(series.ed$var[i])) & (is.na(series.ed$var[i-1])) ){
      series.ed$isna[i] <- series.ed$isna[i-1]+1
      # series.ed$isna[i-1] <- series.ed$isna[i]
    }
  }

  count1 <- nrow(series.ed)
  while (count1 > 1){
    if (is.na(series.ed$var[count1])){
      aux <- series.ed$isna[count1]

      if (aux > 1){
        for (i in 1:(aux-1)){
          series.ed$isna[count1-i] <- series.ed$isna[count1]
        }
        count1 <- count1 - aux
      } else{count1 <- count1 - 1}
    } else {count1 <- count1 - 1}
  }



  ## 2) implement linear interpolation, considering consecutive pentads with NA
  aux_count <- max(series.ed$isna) #if larger than 1 there are consecutive pentads
  # with NaN

  for (i in 2:(nrow(series.ed)-1)){
    if (is.na(series.ed$var[i])){
      # calculates the interpolation increments
      aux_na <- series.ed$isna[i]
      dif <- (series.ed$var[i+aux_na] - series.ed$var[i-1])/(aux_na+1)
      for (j in 0:(aux_na-1)){
        series.ed$var[i+j] <- series.ed$var[i-1]+ dif*(j+1)

      }
    }
  }


  # get pentads in matrix form
  pentad.ed <- matrix(series.ed$var, nrow = 73, byrow = F)

  output <- list('pentad_timestamp' = series.ed, 'pentad_matrix' = pentad.ed)
  return(output)
}
