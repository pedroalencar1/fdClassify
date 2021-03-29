\name{get_df_era}
\alias{get_df_era}
\title{function to build dataframe from era5 ncfiles.}
\usage{
  prepare(list_files, lat, lon, soil_layers = c(1))
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
  get_df_era(list_files, lat, lon, soil_layers = c(1))
}
