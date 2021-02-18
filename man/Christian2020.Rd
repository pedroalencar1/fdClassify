\name{Christian2020}
\alias{Christian2020}
\title{Function - Flash Drought identification via Christianet al., 2020}
\usage{
Christian2020(esr_list)
}
\description{
Function of classify as FD based on Mo and Lettenmeier, 2016.
}

\arguments{
\item{v.esr_list}{A list containing two elements, first a time stamped pentad ESR, second, a matrix of ESR with years in columns and pentads in rows.}
}

\details{
Preferably use \code{Christian2020_clean_data} to get the input data for this function.

For more information, please refer to \url{https://doi.org/10.1088/1748-9326/ab9faf}
}

\examples{
Christian2020(esr_list)
}
