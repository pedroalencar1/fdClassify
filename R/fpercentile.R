################### BASIC FUNCTIONS ###########################################

#' @title Percentile function
#'
#' @param vector data frame column or vector containing the analysed data
#'
#' @description Calculates the percentile. This is an intern function.
#'
#' @return The function return a vector with percentiles
#'
#' @export
#'
#' @examples
#'
f.percentile <- function(vector){

  v.percentile <- NULL
  quantiles <- quantile(vector,prob = seq(0,1,0.01), na.rm = T) #101 elements
  quantiles_unique <- unique(as.data.frame(quantiles), fromLast = TRUE)
  rows <- rownames(quantiles_unique)
  values <- as.integer(strsplit(rows, "%")) #from 0 to 100

  v.percentile <- sapply(1:length(vector),FUN = function(i)
    v.percentile[i] = values[which.min(abs(quantiles_unique$quantiles - vector[i]))])
  idx <- !(sapply(v.percentile, length))
  v.percentile[idx] <- NA
  v.percentile <- unlist(v.percentile)
}
