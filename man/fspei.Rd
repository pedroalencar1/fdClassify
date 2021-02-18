\name{f.spei}
\alias{f.spei}
\title{Calcualte the Standard Precipitation Evapotranspiration Index - SPEI}
\usage{
f.spei(vtime, vdeficit, n)
}

\description{
*Function to calculate the Standard Precipitation Evapotranspiration Index - SPEI.
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{vdeficit}{a vector or dataframe column with deficits (precipitation - potential evapotranspiration) already accumulated in analysis intervals (pentad, week, month, etc)}
\item{n}{positive integer representing the number of intervals of analysis to estimate SPEI}
}

\examples{
f.spei(vtime, vdeficit, n)
}
