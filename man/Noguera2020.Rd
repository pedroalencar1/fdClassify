\name{Noguera2020}
\alias{Noguera2020}
\title{Function - Flash Drought classification via Noguera et al., 2020}
\usage{
Noguera2020(vtime, vprecipitation, vtemperature, my_lat, threshold = NA)
}

\description{
Function of classify as FD based on Noguera et al., 2020.
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{vprecipitation}{a vector or dataframe column with precipiration (mm)}
\item{vtemperature}{a vector or dataframe column with air temperatures (Celcius)}
\item{my_lat}{latitude of the studied area}
\item{threshold}{a negative number, the limit for identification of FD}
}

\details{
The output is a list with two data.frames. One with a time-stamped spei, duration, severity, and flash drought flagging. The second a summary with each FD event, it's begging, onset and end dates and its duration in weeks.

}

\examples{
fd_noguera <- Noguera2020(vtime = de_tha_h$time,
                          vprecipitation = de_tha_h$precipitation,
                          vtemperature = de_tha_h$temperature, my_lat = 50.96)}
