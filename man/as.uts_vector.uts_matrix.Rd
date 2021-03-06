% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uts_vector.R
\name{as.uts_vector.uts_matrix}
\alias{as.uts_vector.uts_matrix}
\title{Convert uts_matrix to uts_vector}
\usage{
\method{as.uts_vector}{uts_matrix}(x, USE.NAMES = TRUE, ...)
}
\arguments{
\item{x}{a \code{"uts_matrix"} object.}

\item{USE.NAMES}{logical. Whether to assign sensible names to the output based on the row and column names of \code{x}.}

\item{\dots}{further arguments passed to or from methods.}
}
\description{
Convert a \code{\link{uts_matrix}} to a \code{\link{uts_vector}} by dropping all \code{uts_matrix}-specific attributes.
}
\examples{
as.uts_vector(ex_uts_matrix())
as.uts_vector(ex_uts_matrix(), USE.NAMES=FALSE)
}
\seealso{
\code{\link{uts_matrix}} for the opposite functionality, i.e. for converting a \code{"uts_vector"} to a \code{"uts_matrix"}.
}
