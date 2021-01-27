\name{hargreaves_day}
\alias{hargreaves_day}
\title{Potential evapotranspiration using Hargreaves-Samani method}
\usage{
hargreaves_day(data_temp, my_lat)
}

\description{
*Function to calculate daily potential evapotranspiration using Hargreaves-Samani method.
}

\arguments{
\item{data_temp}{a dataframe with two columns, time stamp (format \code{"\%Y-\%m-\%d"}) and temperature}
\item{my_lat}{pixel or location lontigute and latitude}
}

\examples{
hargreaves_day()
}
