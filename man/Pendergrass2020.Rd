\name{Pendergrass2020}
\alias{Pendergrass2020}
\title{Function - Flash Drought classification via Pendergrass et al., 2020}
\usage{
Pendergrass2020(data.et0, limit.up = 10)
}
\description{
Function of classify as FD based on Pendergrass et al., 2020.
}

\arguments{
\item{data.et0}{a dataframe with two columns, time and potential evapotranspiration (ET0) respectivelly}
\item{limit.up}{a non-negative integer. The threshold for recuperation durin the FD to still be flagged as such}
}

\details{
The output is a list with two data.frames. One with a time-stamped ET0, percentile and flash drought flagging. The second a summary with each FD event, it's begging, onset and end dates and its duration in pentads.

}

\examples{
Pendergrass2020(df)
}
