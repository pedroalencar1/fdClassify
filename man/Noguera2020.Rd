\name{Noguera2020}
\alias{Noguera2020}
\title{Function - Flash Drought classification via Noguera et al., 2020}
\usage{
Noguera2020(data.spei, my_lat)
}

\description{
Function of classify as FD based on Noguera et al., 2020.
}

\arguments{
\item{data.spei}{a dataframe with three columns, time, precipitation and temperature, respectively.}
\item{my_lat}{pixel or location lontigute and latitude}
}

\details{
The output is a list with two data.frames. One with a time-stamped spei, duration, severity, and flash drought flagging. The second a summary with each FD event, it's begging, onset and end dates and its duration in weeks.

}

\examples{
Noguera2020()
}
