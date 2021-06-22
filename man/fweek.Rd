\name{f.week}
\alias{f.week}
\title{function to organize the data into weeks with the average value of the variable}
\usage{
f.week(data.var, na_rm = F, f = mean, kind = 'standard')
}

\description{
*Auxiliar function - Separate data (daily or sub-daily) into weeks
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{vvalue}{a vector or dataframe column with daily or subdaily data of the variable of interest}
\item{na_rm}{boolean, if NA values should be removed. Errors might occur if set to \code{True}}
\item{f}{a function, as \code{mean}, \code{sum}, \code{max} and \code{min}. User's functions may be used.}
\item{kind}{string. It has two possible values: (1) \code{'standard'} will separate the data into intervals of 7 days, always begining in 1st of January (52 weeks - last week with 8 or 9 days); (2) \code{'noguera'} will diveide the year in 48 weeks, always begining in 1st of January}
}

\details{
The \code{data.var} object has to contains two columns, the first with the time stamps (formated as a time structure) and the second with the values for the variable.

If \code{kind == 'noguera'} only daily data is accepted. Noguera et al. (2020 - \url{https://doi.org/10.1111/nyas.14365}) divide the weeks as follows: W1 from 1 to 8; W2 from 9 to 15; W3 from 16 to 22 and W4 from 23 to the end of the month.
}

\examples{
f.week()
}
