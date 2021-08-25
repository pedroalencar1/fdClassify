#' Auxiliar (intern) function for calculating and exporting confusion matrix
#'
#' @param df complete dataset with all events (by month)
#' @param ref the reference model - Mod. Ford and Labosier
#'
#' @details This function is supposed to be used only in the context of the
#' FD-Viz app
#'
#' @return melted data frame to draw plot. The function return some confusion matrix common statistics:
#'   - Accuracy
#'   - Cohen's Kappa
#'   - Specificity
#'   - Sensitivity
#'   - Precision
#'   - F1
#'
#' @export
#'
#' @examples
#' all_fd <- process_all(de_tha_d,include_variables = T, data = 'station')$Series
#'
#' confusion_matrix <- conf_matrix(all_fd, all_fd$`Osman et al.`)
#'
conf_matrix <- function(df, ref){

  metric1 <- NULL
  for (i in 2:10){
    # i = 2
    conf_mat <- confusionMatrix(data = factor(df[,i]), reference = factor(ref))
    overall <- data.frame(value = conf_mat$overall)
    byclass <- data.frame(value = conf_mat$byClass)
    conf_mat <- rbind(overall, byclass)
    # conf_mat <- overall
    if (i == 2){
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
