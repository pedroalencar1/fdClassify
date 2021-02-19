\name{prepare.nc}
\alias{prepare.nc}
\title{function to extract data from a pixel in a NC file for an expecific variable}
\usage{
  prepare(data, period = 'day', f = function(x) x)
}

\description{
  *Auxiliar function - get subdaily precipitation data into days or other intervals.
}

\arguments{
  \item{data}{dataframe with two columns, timestamp and variable of interest}
  \item{period}{string, identify the interval of interst (defaul is day)}
  \item{f}{a function, as \code{mean}, \code{sum}, \code{max} and \code{min}. User's functions may be used.}

}

\examples{
  prepare()
}
