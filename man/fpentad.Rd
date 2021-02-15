\name{f.pentad}
\alias{f.pentad}
\title{function to organize the data into pentads with the average value of the variable}
\usage{
f.pentad(data.var)
}

\description{
*Auxiliar function - Separate data (daily or s
ub-daily) into pentads
}

\arguments{
\item{data.var}{a dataframe time and data for the variable of interest}
}

\details{
The \code{data.var} object has to contains two columns, the first with the time stamps (formated as a time structure) and the second with the values for the variable
}

\examples{
f.pentad()
}
