\name{Osman2021}
\alias{Osman2021}
\title{Function - Flash Drought classification via Osman et al., 2021}
\usage{
Osman2021(vtime, vswc, threshold = 20)
}
\description{
Function of classify as FD based on Osman et al., 2021.
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{vswc}{a vector or dataframe column with daily soil moisture or soil water content}
\item{threshold}{non-negative integet, the limit for FD identification (percentile of SWC)}
}

\details{
The output is a list with two data.frames. One with a time-stamped soil water content, percentile and flash drought flagging. The second a summary with each FD event, it's begging, onset and end dates and its duration in pentads.
}

\examples{
Osman2021()
}
