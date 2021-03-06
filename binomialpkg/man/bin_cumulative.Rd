% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_stats133.R
\name{bin_cumulative}
\alias{bin_cumulative}
\title{bin_cumulative}
\usage{
bin_cumulative(n, p)
}
\arguments{
\item{n}{The number of trials}

\item{p}{The probability in 1 trial}
}
\value{
A data frame with success, probability, and cumulative probability
}
\description{
Creates a data frame, has two classes 'bincum' and 'data.frame'
}
\examples{
bin_cumulative(n = 5, p = 0.5)
}
