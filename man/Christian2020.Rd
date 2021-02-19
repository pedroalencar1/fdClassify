\name{Christian2020}
\alias{Christian2020}
\title{Function - Flash Drought identification via Christianet al., 2020}
\usage{
Christian2020(esr_list)
}
\description{
Function of classify as FD based on Mo and Lettenmeier, 2016.
}

\arguments{
\item{v.esr_list}{A list containing two elements, first a time stamped pentad ESR, second, a matrix of ESR with years in columns and pentads in rows.}
}

\details{
Preferably use \code{Christian2020_clean_data} to get the input data for this function.

For more information, please refer to \url{https://doi.org/10.1088/1748-9326/ab9faf}
}

\examples{
ET0 <- penman_day(vtime = de_tha_d$time, vwind = de_tha_d$wind_speed,
                  vvpd = de_tha_d$vapor_p_def*10, vtemp = de_tha_d$temperature,
                  vheatflux = (de_tha_d$sensible_heat+de_tha_d$latent_heat))

ETa <- actual_evap_day(vtime = de_tha_d$time, vlatent_heat = de_tha_d$latent_heat,
                       vtemperature = de_tha_d$temperature)

fd_Christian <- Christian2020_clean_data(vtime = de_tha_d$time,
                                         vET0 = ET0$et0, vETa = ETa$eta,
                                         threshold = 1) \%>\% Christian2020()
}
