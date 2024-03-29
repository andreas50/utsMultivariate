% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uts_matrix.R
\name{uts_matrix}
\alias{uts_matrix}
\alias{is.uts_matrix}
\title{Unevenly-spaced Time Series Matrix}
\usage{
uts_matrix(data = uts(), nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)

is.uts_matrix(x)
}
\arguments{
\item{data}{a \code{\link{uts}}, or a \code{\link{uts_vector}} containing at least one time series.}

\item{nrow}{the desired number of rows.}

\item{ncol}{the desired number of columns.}

\item{byrow}{logical. If \code{FALSE} (the default) the matrix is filled by columns, otherwise the matrix is filled by rows.}

\item{dimnames}{a \code{\link{dimnames}} attribute for the matrix: \code{NULL} or a \code{list} of length 2 giving the row and column names respectively.}

\item{x}{an \R object.}
}
\value{
An object of class \code{"uts_matrix"}.
}
\description{
Create a matrix of unevenly spaced time series (\code{"uts_matrix"}).

\code{is.uts_matrix} returns \code{TRUE} if its argument is a \code{"uts_matrix"} object.
}
\details{
If there are too few elements in data to fill the matrix, then the elements in data are recycled. However, compared to \code{\link{matrix}} in base \R, this function makes fewer guesses about how to coerce and recycle the data, and instead throws an error.

Class \code{"uts_matrix"} inherits from class \code{"uts_vector"}. Hence, a \code{"uts_matrix"} supports all of the methods of a \code{"uts_vector"}, such as \code{\link{first}}, \code{\link{last}}, \code{\link{start}}, \code{\link{end}}, etc., even though no such methods exist specifically for a \code{"uts_matrix"}.
}
\note{
An abstract class \code{"uts_virtual"} exists from which \code{"uts"}, \code{"uts_vector"}, and \code{"uts_matrix"} inherit: it is used to allow operations such as subtraction to mix the classes.
}
\examples{
# Create using a single "uts" with recycling
uts_matrix(ex_uts(), 2, 3)

# Create using "uts_vector" with recycling
uts_matrix(ex_uts_vector(), 2, 3)
uts_matrix(ex_uts_vector(), 2, 2, byrow=TRUE)
uts_matrix(ex_uts_vector(), ncol=2)

# Empty "uts_matrix"
uts_matrix(nrow=2, ncol=3, dimnames=list(c("a", "b"), c("X", "Y", "Z")))

# One of the dimensions is of length zero
uts_matrix(nrow=0, ncol=4)
uts_matrix(nrow=4, ncol=0)
uts_matrix(nrow=0)

# The first test returns TRUE, the others return FALSE
is.uts_matrix(uts_matrix())
is.uts_matrix(uts_vector())
is.uts_matrix(ex_uts())
}
\keyword{classes}
\keyword{internal}
\keyword{ts}
