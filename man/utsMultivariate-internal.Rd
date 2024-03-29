% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal.R
\name{utsMultivariate-internal}
\alias{utsMultivariate-internal}
\title{Internal Functions}
\description{
The internal functions listed below might be of interest to developers seeking to extend the package functionality.
}
\details{
\code{uts_vector} and \code{uts_matrix} methods that exist primarily because they also work for \R's other time series classes:
\itemize{
  \item \code{\link{is.uts_vector}}
  \item \code{\link{is.uts_matrix}}
  \item \code{\link{mean.uts_vector}}
  \item \code{\link{median.uts_vector}}
  \item \code{\link{print.uts_vector}}
  \item \code{\link{print.uts_matrix}}
  \item \code{\link{sd.uts_vector}}
  \item \code{\link{summary.uts_vector}}
  \item \code{\link{summary.uts_matrix}}
}

Helper functions:
\itemize{
  \item \code{\link{Ops_uts_vector}}
  \item \code{\link{plot_single_uts_vector}}
}


Methods that are not applicable to unevenly spaced time series. These are provided so that methods intended for \code{\link{ts}} objects in base \R are not accidentally applied to \code{"uts_vector"} and \code{"uts_matrix"} objects:
\itemize{
  \item \code{\link{as.ts.uts_vector}}
  \item \code{\link{cycle.uts_vector}}
  \item \code{\link{frequency.uts_vector}}
}
}
