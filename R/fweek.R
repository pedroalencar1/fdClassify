#' @title Accumulation into weeks
#'
#' @param data.var data frame with time stamps
#' @param na_rm boolean (should NA values be removed? Defaulf = F)
#' @param f \code{R function} to be applied (default = mean)
#' @param kind String indicating what is the size of the week. The default value is "standard" (conventional 7 day week). Other option is "noguera", that devides each month in 4 weeks (1 to 8; 9 to 15; 16 to 22; and 23 to 29/30/31)
#'
#' @return The function return a list with two elements. One data frame with time stamped pentad values and a matrix with the 52 (or 48) weeks organized in lines and years in columns.
#'
#' @description Internal function to accumulate data into weeks using different accumulation functions (mean, max, min, sum, etc.)
#'
#' @export
#'
#' @examples
f.week <- function(data.var, na_rm = F, f = mean, kind = 'standard'){

  #define a column of years and reload start and end of series
  data.var$year <- lubridate::year(data.var[,1])
  beg <- min(data.var$year) #year of the series beginning
  end <- max(data.var$year)

  year.var.list <- lapply(beg:end,f.year, data.var)
  n.years <- length(year.var.list)

  week.matrix<- NULL
  week.series <- NULL

  if (kind == 'standard'){

    for (i in 1:n.years){
      year.var <- year.var.list[[i]][1:2]
      year.var <- tibble::tibble(time = as.POSIXct(year.var[,1]),
                                 value = year.var[,2]) %>% tibbletime::as_tbl_time(time)
      week.var <- tibbletime::collapse_by(year.var, period = '7 days')
      week.var <- week.var %>% dplyr::group_by(time) %>%
        dplyr::summarise(var = f(.data[["value"]], na.rm = na_rm))

      week.var$var[52] <- (week.var$var[52]*7 + week.var$var[53])/8

      week.matrix <- cbind(week.matrix,week.var$var[1:52])
      # set dataframe with all years removing undesireble 53 th week
      week.series <- rbind(week.series, week.var[1:52,])
    }
  } else if (kind == 'noguera'){

    deficit <- data.var

    # cast deficit into weeks and arrange it (weeks according to Noguera)
    deficit$week <- 1 + (lubridate::day(deficit$time) > 8)*1 +
      (lubridate::day(deficit$time) > 15)*1 + (lubridate::day(deficit$time) > 22)*1

    deficit$week.code <- paste(lubridate::year(deficit$time),lubridate::month(deficit$time),
                               deficit$week, sep = '-')

    week.deficit <- deficit[,c(5,2)]
    colnames(week.deficit) <- c('week.code', 'deficit')

    week.deficit <- week.deficit %>% dplyr::group_by(week.code) %>%  dplyr::summarise(deficit = sum(.data[["deficit"]]))

    week.deficit$week.code <- as.Date(week.deficit$week.code)

    week.deficit <- dplyr::arrange(week.deficit,week.code)

    date <- data.frame(year = sort(rep(beg:end,48)), month = rep(sort(rep(1:12,4)),n.years),
                       day = rep(c(1,9,16,22),12*n.years))
    date$date <- as.Date(paste(date$year, date$month, date$day, sep='-'))

    week.series <- data.frame(time = date$date, var = week.deficit$deficit )
    week.matrix <- as.data.frame(matrix(week.deficit$deficit, ncol = n.years, byrow = F))
  }

  output <- list('week_timestamp' = week.series, 'week_matrix' = week.matrix)
  return(output)
}







