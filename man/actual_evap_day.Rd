\name{actual_evap_day}
\alias{actual_evap_day}
\title{Actual evapotranspiration using balance energy method }
\usage{
actual_evap_day(vtime, vlatent_heat, vtemperature = 20)
}

\description{
*Function to calculate daily actual evapotranspiration using balance energy method.
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{vlatent_heat}{a vector or dataframe column with latent heat flux (W m-2 day-1). }
\item{vtemp}{a vector or dataframe column with air temperatures (Celcius). If none is provided, 20ºC is used by default}
}

\details{
Attention to the units and the time scale required. Only daily measures are allowed.
}

\examples{
actual_evap_day(v.time, v.latent_heat, v.temperature = 20)
}
