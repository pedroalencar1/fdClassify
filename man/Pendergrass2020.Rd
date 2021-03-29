\name{Pendergrass2020}
\alias{Pendergrass2020}
\title{Function - Flash Drought classification via Pendergrass et al., 2020}
\usage{
Pendergrass2020(vtime, vet0, limit.down = 10)
}
\description{
Function of classify as FD based on Pendergrass et al., 2020.
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{vet0}{a vector or dataframe column with potential evapotranspiration, prefereably obtained using Penman-Monteith's equation or measured data (mm day-1)}
\item{limit.down}{a non-negative integer. The threshold for recuperation (towards wet conditions) durin the FD to still be flagged as such}
}

\details{
The output is a list with two data.frames. One with a time-stamped ET0, percentile and flash drought flagging. The second a summary with each FD event, it's begging, onset and end dates and its duration in pentads.

}

\examples{
ET0 <- penman_day(vtime = de_tha_d$time, vwind = de_tha_d$wind_speed,
                  vvpd = de_tha_d$vapor_p_def*10, vtemp = de_tha_d$temperature,
                  vheatflux = (de_tha_d$sensible_heat + de_tha_d$latent_heat))

fd_Pendergrass <- Pendergrass2020(vtime = de_tha_d$time,
                                  vet0 = ET0$et0, limit.down = 10)
}
