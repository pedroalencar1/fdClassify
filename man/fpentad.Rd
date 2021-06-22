\name{f.pentad}
\alias{f.pentad}
\title{function to organize the data into pentads with the average value of the variable}
\usage{
f.pentad(vtime, vvalue, na_rm = F, f = mean)
}

\description{
*Auxiliar function - Separate data (daily or sub-daily) into pentads
}

\arguments{
\item{vtime}{a vector or dataframe column with time stamps (\code{date} or \code{POSIX} type)}
\item{vvalue}{a vector or dataframe column with daily or subdaily data of the variable of interest}
\item{na_rm}{boolean, if NA values should be removed. Errors might occur if set to \code{True}}
\item{f}{a function, as \code{mean}, \code{sum}, \code{max} and \code{min}. User's functions may be used.}
}

\details{
The \code{data.var} object has to contains two columns, the first with the time stamps (formated as a time structure) and the second with the values for the variable

Leap years have the last pentad with 6 instead of 5 days.
}

\examples{
f.pentad()
}
