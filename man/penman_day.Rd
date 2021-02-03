\name{penman_day}
\alias{penman_day}
\title{Potential evapotranspiration using Penman-Monteith method}
\usage{
penman_day(v.time, v.wind, v.temp, v.vpd, v.heatflux, altitude = 0)
}

\description{
*Function to calculate daily potential evapotranspiration using Penman-Monteith  method.
}

\arguments{
\item{v.time}{a vector or dataframe column with time stamps}
\item{v.wind}{a vector or dataframe column with wind speed (m s-1)}
\item{v.temp}{a vector or dataframe column with air temperatures (Celcius)}
\item{v.vpd}{a vector or dataframe column with vapor pressure deficit (kPa)}
\item{v.heatflux}{a vector or dataframe column with heat flux (MJ m-2 day -1). The heat flux can be [Ner Radiation - Ground Flux] or [Latenr Heat Flux + Sensible Heat Flux]}
\item{altitude}{Altitude in m.a.s.l, default value = 0}
}

\examples{
penman_day(v.time, v.wind, v.temp, v.vpd, v.heatflux, altitude = 0)
}
