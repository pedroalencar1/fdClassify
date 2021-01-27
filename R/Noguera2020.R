Noguera2020 <- function(data.spei, my_lat){

  data.temp <- data.spei[,c(1,3)]
  data.prec <- data.spei[,c(1,2)]

  precipitation <- precipiration_day(data.prec)
  et0 <- hargreaves_day(data.temp, my_lat)

  deficit <- data.frame(time = et0$time, deficit = precipitation$p - et0$et)

  # cast deficit into weeks and arrange it
  deficit$week <- 1 + (day(deficit$time) > 8)*1 +
    (day(deficit$time) > 15)*1 + (day(deficit$time) > 22)*1
  deficit$week.code <- paste(year(deficit$time),month(deficit$time),
                             deficit$week, sep = '-')
  week.deficit <- deficit[,c(2,4)]
  week.deficit <- week.deficit %>% group_by(week.code) %>%
    summarise(deficit = sum(.data[["deficit"]]))
  week.deficit$week.code <- as.Date(week.deficit$week.code)
  week.deficit <- arrange(week.deficit,week.code)

  #get accumulated deficit over four weeks
  n = 4 #accumulation time in weeks (defaut = 4)
  deficit <- runner(week.deficit$deficit, f = function(x) sum(x), k = n)
  deficit[1:(n-1)] <- -1e7

  deficit_ac_week <- as.data.frame(matrix(deficit, ncol = 19, byrow = F))
  # deficit_ac_week <- deficit_ac

  deficit_ac_sort_week <-t(apply(deficit_ac_week, 1, sort))
  deficit_ac_sort_week[deficit_ac_sort_week == -1e7] <- NA
  # plot(deficit_ac_sort[10,])
  # deficit_ac_sort_week <- deficit_ac_sort

  parameters <- pwm_3llgt(deficit_ac_sort_week)

  deficit_ac_week[deficit_ac_week == -1e7] <- NA

  prob <- (1 + (parameters$a/(deficit_ac_week - parameters$c))^parameters$b)^-1
  aux <- unlist(prob)
  aux[is.nan(aux)] <- 1e-4
  prob <-matrix(aux, dim(prob))
  # head(prob)

  spei <- as.data.frame(qnorm(prob)) #normalization

  beg <- min(year(data.temp[,1])) #year of the series beginning
  end <- max(year(data.temp[,1]))

  date <- data.frame(year = sort(rep(beg:end,48)), month = rep(sort(rep(1:12,4)),19),
                     day = rep(c(1,9,16,22),228))
  date$date <- as.Date(paste(date$year, date$month, date$day, sep='-'))

  spei <- data.frame(time = as.POSIXct.Date(date$date), spei = unlist(spei))

  spei$dif <- c(rep(NA,4), diff(spei$spei, lag = 4))

  # get threshold ofSPEI
  # the threshold is deruved directly from the gaussian function
  threshold <- qnorm(0.1) # the threshold is deruved directly from the gaussian function
  # qnorm(0.1) = -1.28

  ###### Another option, more strict, would be to get the threshold from the sampled data
  ###### and still stricter, get it for only the negative spei:
  # threshold <- quantile(spei$spei,0.1, na.rm = T)
  # neg.spei <- spei$spei[spei$spei<0] %>% .[complete.cases(.)] %>% sort(decreasing = T)
  # threshold <- quantile(neg.spei$spei,0.1, na.rm = T)

  spei$is.fd <- (spei$dif <= -2)*(spei$spei <= threshold)

  fd.info <- spei[complete.cases(spei),]

  count<-0
  fd.info$event <- 0
  fd.info$dur <- 0
  fd.info$sev <- 0
  fd.info$beg.init <- fd.info$time
  for (i in 2:nrow(fd.info)){
    if (fd.info$is.fd[i-1] == 0 & fd.info$is.fd[i] == 1){
      count = count + 1
      fd.info$event[i] <- count
      fd.info$dur[i] <- 1
      fd.info$beg.init[i] <- fd.info$time[i-4]
      fd.info$sev[i] <- fd.info$spei[i]
    }
    if (fd.info$is.fd[i-1] == 1 & fd.info$is.fd[i] == 1){
      fd.info$event[i] <- count
      fd.info$dur[i] <- fd.info$dur[i-1] + 1
      fd.info$sev[i] <- fd.info$sev[i-1] + fd.info$spei[i]
    }
  }


  fd.summary <- data.frame(event = unique(fd.info$event)[c(-1)])

  fd.summary$intensification <- fd.info  %>% group_by(event) %>%
    summarise(x = min(.data[["beg.init"]])) %>% .[,2] %>% unlist() %>%
    as.POSIXct(origin = "1970-01-01") %>% .[c(-1)]

  fd.summary$onset <- fd.info  %>% group_by(event) %>%
    summarise(x = min(.data[["time"]])) %>% .[,2] %>% unlist() %>%
    as.POSIXct(origin = "1970-01-01") %>% .[c(-1)]

  fd.summary$end <- fd.info  %>% group_by(event) %>%
    summarise(x = max(.data[["time"]])) %>% .[,2] %>% unlist() %>%
    as.POSIXct(origin = "1970-01-01") %>% .[c(-1)]

  fd.summary$dur <- fd.info  %>% group_by(event) %>%
    summarise(x = max(.data[["dur"]])) %>% .[,2] %>% unlist() %>% .[c(-1)]

  fd.summary$sev <- fd.info  %>% group_by(event) %>%
    summarise(x = sum(.data[["sev"]])) %>% .[,2] %>% unlist() %>% .[c(-1)]

  output <- list('spei_timeseries' = fd.info, 'FD_info' = fd.summary)

  return(output)
}
