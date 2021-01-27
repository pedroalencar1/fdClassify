################### BASIC FUNCTIONS ###########################################

f.year <- function(i,day.var,year.var=NULL){

 # if(!require('dplyr'))install.packages('dplyr')

  year.var$i <- filter(day.var, day.var$year == i)
  return(year.var$i)
}
