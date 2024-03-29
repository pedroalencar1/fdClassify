% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FordLabosier2017.R
\name{FordLabosier2017}
\alias{FordLabosier2017}
\title{Method of Ford and Labosier (2017)}
\usage{
FordLabosier2017(vtime, vswc, crit = c(40, 20, 30))
}
\arguments{
\item{vtime}{data frame column or vector containing \code{date} data}

\item{vswc}{data frame column or vector containing soil water content values}

\item{crit}{a vector of three value (default \code{c(20,40,30)}) indicating the model thresholds for lower, upper and persistance limits for SWC percentiles.}
}
\value{
A list with two data frames, one a time series with all data for FD identification, and the second with a summary of FD events.
}
\description{
This function follows the description contained in the original paper. We have as additional criterion that FD should have at least 3 pentads with SM lower than 30th percentile (as proposed by Dr. Ford in personal communication).
}
\examples{
FD_events <- FordLabosier2017(de_tha_d$time, de_tha_d$soil_water, crit = c(40,20,30))


}
