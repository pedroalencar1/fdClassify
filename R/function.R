################### BASIC FUNCTIONS ###########################################

f.year <- function(i,year.var,day.var){
  year.var$i <- filter(day.var, day.var$year == i)
}

f.percentile <- function(vector){
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
################################################################################

#### FD classification by Ford and Labosier (2017) -- based on
#soil moisture/soil water content

fd_fl2017 <- function(swc){
  swc$year <- year(swc$time)

  beg <- min(swc$year)
  end <- max(swc$year)
  year.swc <- NULL
  year.swc <- lapply(beg:end,f.year, year.swc, swc)
  n.years <- length(year.swc)

  pent.swc <- NULL

  for (i in 1:n.years){
    year.var <- year.swc[[i]][1:2]
    year.var <- tibble::tibble(time = as.POSIXct(year.var[,1]),
                               value = year.var[,2]) %>%
      as_tbl_time(time)
    pent.var <- collapse_by(year.var, period = '5 days')
    pent.var <- pent.var %>% group_by(time) %>%
      summarise(swc = mean(.data[["value"]]))
    pent.swc <- cbind(pent.swc,pent.var$swc)
  }

  pent.swc <- pent.swc[1:73,] # non-desireble 74-th removed.
  ts.swc <- ts(c(pent.swc), frequency = 73, start = beg)

  # get percentiles
  perc.swc <- t(apply(pent.swc,1, f.percentile))
  ts.perc.swc <- ts(c(perc.swc), frequency = 73, start = beg)
  ts.comp <- c(perc.swc)

  a1 <- unlist(lapply(1:length(ts.comp),function(i) ts.comp[i] - ts.comp[i-1])) %>%
    c(rep(NA,1),.)
  a2 <- unlist(lapply(2:length(ts.comp),function(i) ts.comp[i] - ts.comp[i-2])) %>%
    c(rep(NA,2),.)
  a3 <- unlist(lapply(3:length(ts.comp),function(i) ts.comp[i] - ts.comp[i-3])) %>%
    c(rep(NA,3),.)
  a4 <- unlist(lapply(4:length(ts.comp),function(i) ts.comp[i] - ts.comp[i-4])) %>%
    c(rep(NA,4),.)

  ts.comp <- cbind(ts.comp,a1,a2,a3,a4)
  a.min <- sapply(1:nrow(ts.comp), function(i) min(ts.comp[i,2:5],na.rm = T))

  fd <- (ts.comp[,1] <= 20) * (a.min <= -20)
  length(fd)
  ts.fd <- ts(fd, frequency = 73, start = beg)

  output <- cbind(ts.swc,ts.perc.swc, ts.fd)
  colnames(output) <- c('SWC', 'p.SWC', 'is.fd')
  return(output)
}

################################################################################











####################### ERA5 FUNCTIONS #########################################
get.nc.data <- function(x,y,z,name){
  vname <- name

  print('Files read:')

  for (i in 1:z) {
    ncin <- nc_open(my_filename[i])# open a netCDF file
    # print(ncin)
    print(my_filename[i])
    time <- ncvar_get(ncin,"time") #get time in hours since 01-01-1900

    lon <- ncvar_get(ncin,"longitude")
    lat <- ncvar_get(ncin,"latitude")

    lon_index <- which.min(abs(lon - x))
    lat_index <- which.min(abs(lat - y))
    # extract time for single pixel
    pixel_time <- nc.get.time.series(ncin, v = vname,
                                     time.dim.name = "time",
                                     correct.for.gregorian.julian = FALSE,
                                     return.bounds = TRUE)
    new_time <- as.POSIXct(pixel_time,
                           format="%Y-%m-%d %H:%M:S")


    # extract timeseries value for single pixel TEMPERATURE
    pixel_data  <- nc.get.var.subset.by.axes(ncin, vname,
                                             axis.indices = list(X = lon_index,
                                                                 Y = lat_index))
    fillvalue <- ncatt_get(ncin,vname,"_FillValue")
    pixel_data[pixel_data==fillvalue$value] <- NA
    # pixel_data <- pixel_data*1000 #convert from m to mm
    # pixel_data <- pixel_data - 273.15 #convert from m to mm
    one_data <- data.frame("date"=new_time, "value"=as.vector(pixel_data))
    if (i==1) all_data <- one_data else all_data <- rbind(all_data, one_data)
    # close nc file
    nc_close(ncin)
  }
  write.table(all_data,paste('data_',name,'.txt',sep = ''),sep = ';')
}
################################################################################



