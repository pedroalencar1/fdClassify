
plot2d <- function(complete_series){

complete_series$events <- rowSums(complete_series[,2:7])
complete_series$year <- lubridate::year(complete_series$Date)
complete_series$jd <- as.numeric(format(complete_series$Date, "%j"))
fd.class <- complete_series[,c(18,19,17)] %>% tidyr::spread(key = 'jd', value = events) %>%
  .[,2:366] %>% as.matrix(ds.class1, nrow = 14, rownames = T) %>% t(.)

year_min <- min(complete_series$year)
year_max <- max(complete_series$year)

plot3D::image2D(x = 1:365, y = year_min:year_max,z = fd.class,
        xlab = "Julian days", ylab = 'Year', contour = F, breaks = 0:6,
        colkey = list(side = 4, length = 0.8),
        main = paste('FD events'))

grDevices::png(filename="events.png", width=750, height = 500, bg="white")
plot3D::image2D(x = 1:365, y = year_min:year_max,z = fd.class,
        xlab = "Julian days", ylab = 'Year', contour = F, breaks = 0:6,
        colkey = list(side = 4, length = 0.8),
        main = paste('FD events'))
dev.off()

}
