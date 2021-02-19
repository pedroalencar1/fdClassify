\name{Pendergrass2020}
\alias{Pendergrass2020}
\title{Function - Flash Drought classification via Pendergrass et al., 2020}
\usage{
Pendergrass2020(vtime, vet0, limit.up = 5)
}
\description{
Function of classify as FD based on Pendergrass et al., 2020.
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{vet0}{a vector or dataframe column with potential evapotranspiration, prefereably obtained using Penman-Monteith's equation or measured data (mm day-1)}
\item{limit.up}{a non-negative integer. The threshold for recuperation durin the FD to still be flagged as such}
}

\details{
The output is a list with two data.frames. One with a time-stamped ET0, percentile and flash drought flagging. The second a summary with each FD event, it's begging, onset and end dates and its duration in pentads.

}

\examples{
Pendergrass2020(df)
}
