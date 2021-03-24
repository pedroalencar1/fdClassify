\name{process_all}
\alias{process_all}
\title{Function to compile all methods simultaneously, used for comparison}
\usage{
  process_all(df_d, include_variables = T)
}

\description{
  \code{process_all} generates a single data frame with the comple time series for all classifications, variables and indexes used. Also produces a list with the summary of events for each method.
}

\arguments{
  \item{df_d}{\a dataframe with all relevant variables for FD identification: time stamp, precipitation, temperature, wind speed, soil water, atmosferic pressure, vapour pressure deficit, sensible and latent heat (in this order and with daily data). For FLUXNET it can be generated using \code{df_fluxnet}. For ERA5, using \code{df_era5}.}
  \item{include_variables}{booleam, indicates if the dataframe should contain only the classifications (\code{False}) or also the variables (default - \cod{True})}
}

\details{
  The \code{deficit} data should be structure with years separated in columns and the time scale (pentad, week, etc.) as rows.
}

\examples{
  complete series <- process_all(de_tha_d, include_variables = T)
}
