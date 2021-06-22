\name{plot_series}
\alias{plot_series}
\title{Function to plot an interval of the time series for all methods}
\usage{
  plot_series(years, complete_series)
}

\description{
  \code{plot_series} generates and export in \code{.png} a plot in A4 format containing the FD identification and relevat variables for the six methods.
}

\arguments{
  \item{years}{a numeric vector with two values of years (integers -  see examples)}
  \item{complete_series}{a dataframe. The output from \code{process_all}.}
}

\details{
  A file is automaticaly exported to the working directory with a concise name.
}

\examples{
  complete series <- plot_series(de_tha_d, include_variables = T)
  plot_series(c(2010,2010), complete_series$Series)
  plot_series(c(2001,2005), complete_series$Series)
}
