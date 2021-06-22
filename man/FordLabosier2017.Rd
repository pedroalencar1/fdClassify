\name{FordLabosier2017}
\alias{FordLabosier2017}
\title{Function - Flash Drought classification via Foster and Labosier 2017}
\usage{
FordLabosier2017(vtime, vswc, crit(x,y,z))
}
\description{
Function of classify as FD based on Foster and Labosier2017.
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{vswc}{a vector or dataframe column with daily soil moisture or soil water content}
\item{crit}{an array with three numeric (non-negative integer) values. The first (x) contains the upper limit for FD identification, the second (y) the lower limit and the third (z) the recuperation threshold after FD initiation.}
}

\details{
Please attention to the order of the criteria (\code{crit}).

The output is a list with two data.frames. One with a time-stamped soil water content, percentile and flash drought flagging. The second a summary with each FD event, it's begging, onset and end dates and its duration in pentads.

For more information, please refer to \url{https://doi.org/10.1016/j.agrformet.2017.08.031}
}

\examples{
fd_FordLabosier <- FordLabosier2017(vtime = de_tha_d$time,
                                    vswc = de_tha_d$soil_water,
                                    crit = c(40,20,30))
}
