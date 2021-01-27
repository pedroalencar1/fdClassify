\name{get.nc.data}
\alias{get.nc.data}
\title{function to extract data from a pixel in a NC file for an expecific variable}
\usage{
get.nc.data(my_lon,my_lat,my_filename,vname)
}

\description{
*Auxiliar function to prepare input data from nc files.
}

\arguments{
\item{my_lon, my_lat}{pixel or location lontigute and latitude}
\item{my_filename}{a character with the file (with or wihout path) of the NC file}
\item{vname}{string containing the name of the variable as listed on the NC file. For ERA5 data, some examples are: "t2m", "tp", "pev", "swvl1"}

\examples{
get.nc.data()
}
