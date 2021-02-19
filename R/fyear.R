################### BASIC FUNCTIONS ###########################################

f.year <- function(i,day.var,year.var=NULL){

 # if(!require('dplyr'))install.packages('dplyr')
  day.var$year <- lubridate::year(day.var[,1])
  year.var$i <- filter(day.var, day.var$year == i)
  return(year.var$i)
}
