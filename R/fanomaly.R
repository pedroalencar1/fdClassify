################### BASIC FUNCTIONS ###########################################

#' @title Function to calculate anomalies
#'
#' @param vector A general vector or data frame column
#'
#' @description Calculates the anomaly (number of standard deviations). This is an intern function.
#'
#' @return vector of anomalies.
#'
#' @export
#'
#' @examples
f.anomaly <- function(vector){
  v.anomaly <- NULL
  v.anomaly <- sapply(1:length(vector),FUN = function(i)
    v.anomaly[i] = (vector[i] - mean(vector, na.rm = TRUE))/sd(vector, na.rm = TRUE))
  idx <- !(sapply(v.anomaly, length))
  v.anomaly[idx] <- NA
  v.anomaly <- unlist(v.anomaly)

  # return(v.anomaly)
}

