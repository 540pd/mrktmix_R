% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{events}
\alias{events}
\title{Events Data (Internal Use)}
\format{
A data frame with 12 rows and a variable number of columns (depending on the number of weeks).
Each column has a name corresponding to a date in the format \code{Date_YYYY_MM_DD} with a binary
indication of whether the event occurred on that date.
}
\usage{
data(events)
}
\description{
A dataset containing one-hot encoded representations of dates corresponding
to the advertising dataset. Each column represents a unique week, and this
data is used internally for modeling event impacts on sales during package development.
}
\examples{
# This data is for internal development use only and not exported.
data(events)
}
\keyword{datasets}
