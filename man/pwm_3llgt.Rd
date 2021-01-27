\name{pwm_3llgt}
\alias{pwm_3llgt}
\title{Function to calibrate parameters of a 3-parameter log-logistic distribution}
\usage{
pwm_3llgt(deficit)
}

\description{
Auxiliar function - Function to calibrate parameters of a 3-parameter log-logistic distribution using the Probability Weighted Mommentd method (see:. Singh, 1998, Entropy-based Parameter Estimation in Hydrology.)
}

\arguments{
\item{deficit}{\code{R} object (matrix or dataframe), with hydrological deficit (precipitaition - potencial evapotranspiration) values.}
}

\details{
The \code{deficit} data should be structure with years separated in columns and the time scale (pentad, week, etc.) as rows.
}

\examples{
pwm_3llgt()
}
