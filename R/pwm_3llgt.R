################### BASIC FUNCTIONS ###########################################


pwm_3llgt <- function(deficit){
  number_years <- ncol(deficit)
  param <- 1:number_years
  param <- 1- (param-0.35)/number_years
  param <- matrix(rep(param,nrow(deficit)), ncol = number_years,
                  nrow = nrow(deficit), byrow = T)

  w0 <- rowSums(deficit, na.rm = T)/number_years
  w1 <- rowSums(deficit*param,na.rm = T)/number_years
  w2 <- rowSums(deficit*param^2, na.rm = T)/number_years

  b <- (2*w1 - w0)/(6*w1 - w0 - 6*w2)
  a <- (w0 - 2*w1)*b/(gamma(1 + 1/b) * gamma(1 - 1/b))
  c <- w0 - (w0 - 2*w1)*b
  parameters <- data.frame(a = a,b = b,c = c)
  return(parameters)
}
