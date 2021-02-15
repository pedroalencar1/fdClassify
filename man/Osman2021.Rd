\name{Osman2021}
\alias{Osman2021}
\title{Function - Flash Drought classification via Osman et al., 2021}
\usage{
Osman2021(data)
}
\description{
Function of classify as FD based on Osman et al., 2021.
}

\arguments{
\item{data}{a dataframe with two columns, time and soil water content (SWC) respectivelly}
}

\details{
The output is a list with two data.frames. One with a time-stamped soil water content, percentile and flash drought flagging. The second a summary with each FD event, it's begging, onset and end dates and its duration in pentads.

}

\examples{
Osman2021(df)
}
