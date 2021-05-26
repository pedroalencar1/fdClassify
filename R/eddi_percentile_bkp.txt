#function to calculate EDDI following Hobbins et al., 2016

# It receives a single input, a vector/dataframe-row with ET0 data organized as follows:
# a) years separates by column
# b) weeks (or other time window of interest) separated by rows

eddi_percentile <- function(vet0){

  n <- length(vet0)
  empiric_p <- (1:n -0.33)/(n+0.33) #empiric probabilities (Tukey plotting)

  veddi <- round(empiric_p[rank(vet0)]*100)

  return(veddi)
}


