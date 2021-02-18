\name{Christian2020_clean_data}
\alias{Christian2020_clean_data}
\title{Function - Organize and clean data to implement Flash Drought identification via Crhistian et al., 2020}
\usage{
Christian2020_clean_data(vtime, vET0, vETa, threshold = 1)
}
\description{
Organize and clean data to implement Flash Drought identification via Crhistian et al., 2020.
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{vET0}{a vector or dataframe column with daily potential evapotranspiration (mm. day-1). It can be obtained using the function \code{penman_day}}
\item{vETa}{a vector or dataframe column with daily actual evapotranspiration (mm. day-1). It can be obtained using the function \code{actual_evap_day}}
\item{threshold}{a numeric value, equal to or larger than 1. It indicates wich values of ESR above one will be discarded.}

}

\details{
The output is a list with two elements. One with time-stamped ESR divided into pentads, the o a matrix form of the same data, with lines for pentads and rows for years.

This data can be used directly into \code{Christian2020} to identify Flash Droguhts.
}

\examples{
Christian2020_clean_data(vtime, vET0, vETa, threshold = 1)
}
