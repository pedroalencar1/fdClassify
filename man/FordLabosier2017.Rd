\name{FordLabosier2017}
\alias{FordLabosier2017}
\title{Function - Flash Drought classification via Foster and Labosier 2017}
\usage{
FordLabosier2017(data)
}
\description{
Function of classify as FD based on Foster and Labosier2017.
}

\arguments{
\item{data}{a dataframe with two columns, time and soil water content (SWC) respectivelly}
}

\details{
The output is a list with two data.frames. One with a time-stamped soil water content, percentile and flash drought flagging. The second a summary with each FD event, it's begging, onset and end dates and its duration in pentads.

}

\examples{
FordLabosier2017(df)
}
