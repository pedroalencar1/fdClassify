################### BASIC FUNCTIONS ###########################################

f.percentile <- function(vector){

 # if(!require('stats'))install.packages('stats')

  v.percentile <- NULL
  a1 <- quantile(vector,prob = seq(0,1,0.01), na.rm = T) #101 elements
  a2 <- unique(as.data.frame(a1), fromLast = TRUE)
  a3 <- rownames(a2)
  a4 <- as.integer(strsplit(a3, "%")) #from 0 to 100

  v.percentile <- sapply(1:length(vector),FUN = function(i)
    v.percentile[i] = a4[which.min(abs(a2$a1 - vector[i]))])
  idx <- !(sapply(v.percentile, length))
  v.percentile[idx] <- NA
  v.percentile <- unlist(v.percentile)
}
