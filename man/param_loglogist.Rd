\name{param_loglogist}
\alias{param_loglogist}
\title{Function to calibrate parameters of a 2- and 3-parameter log-logistic distribution}
\usage{
  param_loglogist(deficit, n_param = 3)
}

\description{
  Auxiliar function - Function to calibrate parameters of a 2- and 3-parameter log-logistic distribution using the Probability Weighted Mommentd method (see:. Singh, 1998, Entropy-based Parameter Estimation in Hydrology.)
}

\arguments{
  \item{deficit}{\code{R} object (matrix or dataframe), with hydrological deficit (precipitaition - potencial evapotranspiration) values.}
  \item{n_param}{number of parameters, 2 or 3.}
}

\details{
  The \code{deficit} data should be structure with years separated in columns and the time scale (pentad, week, etc.) as rows.
}

\examples{
  param_loglogist()
}
