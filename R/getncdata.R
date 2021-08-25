#' @title get.nc.data - Function to extract data from ERA5 raw data
#'
#' @param my_lon Longituge of study area (decimal degrees)
#' @param my_lat Latitude of study area (decimal degrees)
#' @param my_filename File name
#' @param vname Variable to be extracted
#' @param file boolean, indicate if a csv file should be generated (defaulf = True)
#'
#' @return The function returns a data frame containing one variable and time stamps
#'
#' @export
#'
#' @examples
get.nc.data <- function(my_lon,my_lat,my_filename,vname, file = T){

  z <- length(my_filename)

  for (i in 1:z) {
    ncin <- ncdf4::nc_open(my_filename[i])# open a netCDF file
    # print(ncin)
    print(my_filename[i])
    time <- ncdf4::ncvar_get(ncin,"time") #get time in hours since 01-01-1900

    lon <- ncdf4::ncvar_get(ncin,"longitude")
    lat <- ncdf4::ncvar_get(ncin,"latitude")

    lon_index <- which.min(abs(lon - my_lon))
    lat_index <- which.min(abs(lat - my_lat))
    # extract time for single pixel
    pixel_time <- ncdf4.helpers::nc.get.time.series(ncin, v = vname,
                                     time.dim.name = "time",
                                     correct.for.gregorian.julian = FALSE,
                                     return.bounds = TRUE)
    new_time <- as.POSIXct(pixel_time,
                           format="%Y-%m-%d %H:%M:S")


    # extract timeseries value for single pixel TEMPERATURE
    pixel_data  <- ncdf4.helpers::nc.get.var.subset.by.axes(ncin, vname,
                                             axis.indices = list(X = lon_index,
                                                                 Y = lat_index))
    fillvalue <- ncdf4::ncatt_get(ncin,vname,"_FillValue")
    pixel_data[pixel_data==fillvalue$value] <- NA
    # pixel_data <- pixel_data*1000 #convert from m to mm
    # pixel_data <- pixel_data - 273.15 #convert from m to mm
    one_data <- data.frame("date"=new_time, "value"=as.vector(pixel_data))
    if (i==1) all_data <- one_data else all_data <- rbind(all_data, one_data)
    # close nc file
    ncdf4::nc_close(ncin)
  }

  if (file){
  write.csv(all_data,paste('data_',vname,'.csv',sep = ''),sep = ';')
  }
  return(all_data)
}
