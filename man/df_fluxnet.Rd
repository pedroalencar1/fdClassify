\name{df_fluxnet}
\alias{df_fluxnet}
\title{Get dataframe of relevant variables for FD analysis from a FLUXNET2015 file}
\usage{
df_fluxnet(filename, timestep, soil_level = 1)
}

\description{
Function to set the dataframe of relevant variables for FD analysis from a FLUXNET2015 file
}

\arguments{
\item{filename}{string, the name of the csv file to be read. If it is not in the same directory, add the path.}
\item{timestep}{string, \code{"day"} or \code{"hour"} for daily totals or sub-daily measurements}
\item{soil_level}{positive integer, the index of the sensor of soil water content. Usually varies from 1 to 4}
}

\details{
The positioning of soil sensors is not uniform across sites. Please confirm the sensor position in the FLUXNET2015 documentation. The output is a single dataframe with all values and timestamps. Missing data are replaced by \code{NA}.
}

\examples{
de_tha_d <- df_fluxnet('FLX_DE-Tha_FLUXNET2015_SUBSET_DD_21996-2014_1-4.csv', 'day', soil_level = 1)
}
