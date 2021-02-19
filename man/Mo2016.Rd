\name{Mo2016}
\alias{Mo2016}
\title{Function - Flash Drought identification via Mo and Lettenmeier, 2016}
\usage{
Mo2016(v.time, v.precipitation, v.temperature, v.soil_water,
        v.latent_heat = NULL, v.evap = NULL, flux_data = T)
}
\description{
Function of classify as FD based on Mo and Lettenmeier, 2016.
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{v.precipitation}{a vector or dataframe column with precipiration (mm)}
\item{v.temperature}{a vector or dataframe column with air temperatures (Celcius)}
\item{v.soil_water}{a vector or dataframe column with soil moisture or soil water content}
\item{v.latent_heat}{a vector or dataframe column with latent heat flux (MJ m-2 day -1), by default it iss \code{NULL}}
\item{v.evap}{a vector or dataframe column actual evapotranspiration data (mm day -1), by default it iss \code{NULL}}
\item{flux_data}{boolean, inform is the data to compute evapotranspiration is of heat_flux (default) or if direct values of ETa are used (measured or modelled)}
}

\details{
The output is a list with two elements. One with time-stamped climatic data, its anomalies, flash drought occurrence and event tag. The second a summary with each FD event, its beginning and duration. This summary is organized in a list with three elements, one for heat wave flash droughts (HWFD), one for precipitation deficit flash drought (PDFD) and one with the joint events (Joint).

Note that at least one of \code{v.latent_heat} or \code{v.evap} has to be given as input. If both are given, the \code{flux_data} will define qhich is used. If  \code{flux_data} is set to \code{True}, latent heat data will be used, else, direct values of ETa qill be used.
}

\examples{
fd_Mo <- Mo2016(vtime = de_tha_d$time, vprecipitation = de_tha_d$precipitation,
                vtemperature = de_tha_d$temperature, vsoil_water = de_tha_d$soil_water,
                vlatent_heat = de_tha_d$latent_heat)
}
