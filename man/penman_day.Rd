\name{penman_day}
\alias{penman_day}
\title{Potential evapotranspiration using Penman-Monteith method}
\usage{
penman_day(vtime, vwind, vtemp, vvpd, vheatflux, altitude = 0)
}

\description{
*Function to calculate daily potential evapotranspiration using Penman-Monteith  method.
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps}
\item{vwind}{a vector or dataframe column with wind speed (m s-1)}
\item{vtemp}{a vector or dataframe column with air temperatures (Celcius)}
\item{vvpd}{a vector or dataframe column with vapor pressure deficit (kPa)}
\item{vheatflux}{a vector or dataframe column with heat flux (W m-2). The heat flux can be [Ner Radiation - Ground Flux] or [Latenr Heat Flux + Sensible Heat Flux]}
\item{altitude}{Altitude in m.a.s.l, default value = 0}
}

\examples{
ET0 <- penman_day(vtime = de_tha_d$time, vwind = de_tha_d$wind_speed,
                  vvpd = de_tha_d$vapor_p_def*10, vtemp = de_tha_d$temperature,
                  vheatflux = (de_tha_d$sensible_heat + de_tha_d$latent_heat))
}
