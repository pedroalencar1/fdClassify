\name{actual_evap_day}
\alias{actual_evap_day}
\title{Actual evapotranspiration using balance energy method }
\usage{
actual_evap_day(v.time, v.latent_heat, v.temperature = 20)
}

\description{
*Function to calculate daily actual evapotranspiration using balance energy method.
}

\arguments{
\item{v.time}{a vector or dataframe column with time stamps}
\item{v.latent_heat}{a vector or dataframe column with latent heat flux (W m-2). }
\item{v.temp}{a vector or dataframe column with air temperatures (Celcius). If none is provided, 20ºC is used by default}
}

\examples{
actual_evap_day(v.time, v.latent_heat, v.temperature = 20)
}
