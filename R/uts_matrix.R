####################
# UTS_MATRIX class #
####################

#' Unevenly-spaced Time Series Matrix
#' 
#' Create a matrix of unevenly spaced time series (\code{"uts_matrix"}).
#' 
#' If there are too few elements in data to fill the matrix, then the elements in data are recycled. However, compared to \code{\link{matrix}} in base \R, this function makes fewer guesses about how to coerce and recycle the data, and instead throws an error.
#' 
#' Class \code{"uts_matrix"} inherits from class \code{"uts_vector"}. Hence, a \code{"uts_matrix"} supports all of the methods of a \code{"uts_vector"}, such as \code{\link{first}}, \code{\link{last}}, \code{\link{start}}, \code{\link{end}}, etc., even though no such methods exist specifically for a \code{"uts_matrix"}.
#' 
#' @note A virtual class \code{"uts_virtual"} exists from which \code{"uts"}, \code{"uts_vector"}, \code{"uts_matrix"}, and \code{"uts_data_frame"} inherit: it is used to allow operations such as subtraction to mix the classes.
#'
#' @return An object of class \code{"uts_matrix"}.
#' @param data a \code{\link{uts}}, or a \code{\link{uts_vector}} containing at least one time series.
#' @param nrow the desired number of rows.
#' @param ncol the desired number of columns.
#' @param byrow logical. If \code{FALSE} (the default) the matrix is filled by columns, otherwise the matrix is filled by rows.
#' @param dimnames a \code{\link{dimnames}} attribute for the matrix: \code{NULL} or a \code{list} of length 2 giving the row and column names respectively.
#' 
#' @keywords ts classes
#' @examples
#' # Create using a single "uts" with recycling
#' uts_matrix(ex_uts(), 2, 3)
#' 
#' # Create using "uts_vector" with recycling
#' uts_matrix(ex_uts_vector(), 2, 3)
#' uts_matrix(ex_uts_vector(), 2, 2, byrow=TRUE)
#' uts_matrix(ex_uts_vector(), ncol=2)
#' 
#' # Empty "uts_matrix"
#' uts_matrix(nrow=2, ncol=3, dimnames=list(c("a", "b"), c("X", "Y", "Z")))
#' 
#' # One of the dimensions is of length zero
#' uts_matrix(nrow=0, ncol=4)
#' uts_matrix(nrow=4, ncol=0)
#' uts_matrix(nrow=0)
#' 
#' # The first tests returns TRUE, the others return FALSE
#' is.uts_matrix(uts_matrix())
#' is.uts_matrix(uts_vector())
#' is.uts_matrix(ex_uts())
uts_matrix <- function(data=uts(), nrow=1, ncol=1, byrow=FALSE, dimnames=NULL)
{
  # Argument checking
  if (is.na(nrow) || nrow < 0 || is.infinite(nrow))
    stop("Invalid 'nrow' value")
  if (is.na(ncol) || ncol < 0 || is.infinite(ncol))
    stop("Invalid 'ncol' value")
  
  # Determine number of time series to work with
  if (is.uts(data))
    num_ts <- 1 * (nrow * ncol > 0)
  else if (is.uts_vector(data))
    num_ts <- length(data)
  else
    stop("The 'data' for a 'uts_matrix' needs to be a 'uts' or 'uts_vector'")
  
  # Check that nrow/ncol value compatible with the number of time series
  if (!missing(nrow) && (nrow > 1) && ((nrow * ncol) %% max(1, num_ts) > 0))
    stop("'data' length not a multiple or sub-multiple of the number of rows")
  if (!missing(ncol) && (ncol > 1) && ((nrow * ncol) %% max(1, num_ts) > 0))
    stop("'data' length not a multiple or sub-multiple of the number of columns")
  
  # In case not provided, guess nrows and ncols
  if (missing(nrow) && (num_ts >= ncol))
    nrow <- num_ts / max(1, ncol)
  else if (missing(ncol) && (num_ts >= nrow))
    ncol <- num_ts / max(1, nrow)
  if (num_ts > nrow * ncol)
    stop("'data' too long to fit into 'uts_matrix' of provided dimensions")
  
  # Recycle data
  if (is.uts(data)) {
    out <- rep(data, nrow * ncol)
  } else if (is.uts_vector(data)) {
    if (byrow)
      out <- rep(data, each = (nrow * ncol) %/% num_ts)
    else
      out <- rep(data, times = (nrow * ncol) %/% num_ts)
  }
  
  # Assign attributes
  class(out) <- c("uts_matrix", class(uts_vector()))
  dim(out) <- c(nrow, ncol)
  dimnames(out) <- dimnames
  out
}


#' @rdname uts_matrix
#' 
#' @description \code{is.uts_matrix} returns \code{TRUE} if its argument is a \code{"uts_matrix"} object.
#' 
#' @param x an \R object.
#' 
#' @keywords internal
is.uts_matrix <- function(x)
{
  inherits(x, "uts_matrix")
}

