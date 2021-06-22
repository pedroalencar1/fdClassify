\name{get_df_era5}
\alias{get_df_era5}
\title{function to build dataframe from era5 ncfiles.}
\usage{
  get_df_era5(list_files, lat, lon, soil_layers = c(1))
}

\description{
  *Auxiliar function - build dataframe from era5 ncfiles.
}

\arguments{
  \item{list_files}{list of nc files (in the wd or with path) tto be used in the FD identification}
  \item{lat, lon}{numeric, the latitude and longitude of the area of interest}
  \item{soil_layers}{list of integers (from 1 to 4) indicating which layer(s) will be extracted. For more info on the available layers of soil data, please refer to the ERA5 documentation}

}

\examples{
  get_df_era5(list_files, lat, lon, soil_layers = c(1))
}
