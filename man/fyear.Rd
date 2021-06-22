\name{f.year}
\alias{f.year}
\title{Function organize data in list of years}
\usage{
f.year(i,day.var,year.var = NULL)
}
\description{
*Intern function* to set unifor scale of dataframe.
}

\arguments{
\item{i}{a italic(integer) that represents the year}
\item{day.var}{a _data.frame_ with two columns,
              [1] -- a time stamp;
              [2] -- a column with values to be separeted into lists;

\item{year.var}{\code{NULL} by defult, a pre-initialized valiable with \code{NULL} argument (\code{year.var <- NULL})}

\examples{
f.year()
}
