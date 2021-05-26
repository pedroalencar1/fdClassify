#' Auxiliar (intern) function for calculating and exporting confusion matrix
#'
#' @param df complete dataset with all events (by month)
#' @param ref the reference model - Mod. Ford and Labosier
#' 
#' @details This function is supposed to be used only in the context of the 
#' FD-Viz app
#'
#' @return melted data frame to draw plot.
#' @export
#'
#' @examples
conf_matrix <- function(df, ref){
  
  # df <- stats
  # ref <- stats$`Mod. Ford and Labosier`
  
  metric1 <- NULL
  for (i in 3:11){
    # i = 3
    conf_mat <- confusionMatrix(data = factor(df[,i]), reference = factor(ref))
    overall <- data.frame(value = conf_mat$overall)
    byclass <- data.frame(value = conf_mat$byClass)
    conf_mat <- rbind(overall, byclass)
    # conf_mat <- overall
    if (i == 3){
      metric1 <- conf_mat
    } else {
      metric1 <- cbind(metric1,conf_mat)
    }
  }
  
  colnames(metric1) <- c("Mo and Lettenmeier","Ford and Labosier", "Pendergrass et al.",
                         "Noguera et al." , "Christian et al.", "Osman et al.", 
                         'Alencar et al.', "Mod. Ford and Labosier", "Multi-criteria")
  
  metric1$metric <- row.names(metric1)
  
  metric1 <- metric1[c(1,2,8,9,12,14),] %>%
    gather(key = 'model', value = 'value', 1:9) %>%
    .[.$model != "Mod. Ford and Labosier",]
  
  return(metric1)
  
}
