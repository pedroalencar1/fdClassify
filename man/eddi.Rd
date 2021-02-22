\name{eddi}
\alias{eddi}
\title{Function obtain EDDI values (z-score)}
\usage{
eddi(vector)
}
\description{
\emph{Intern function} to obtain EDDI values (z-score), as defined in Hoggins et al. (2016).
}

\arguments{
\item{vector}{A vector or dataframe's column or row with ET0 values.}
}

\examples{
ET0 <- penman_day(vtime = de_tha_d$time, vwind = de_tha_d$wind_speed,
                  vvpd = de_tha_d$vapor_p_def*10, vtemp = de_tha_d$temperature,
                  vheatflux = (de_tha_d$sensible_heat + de_tha_d$latent_heat))

et0_week <- fweek(et0)[[2]]

eddi <- eddi(et0_week[1,])
}
