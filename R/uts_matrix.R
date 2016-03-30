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
#' @note An abstract class \code{"uts_virtual"} exists from which \code{"uts"}, \code{"uts_vector"}, and \code{"uts_matrix"} inherit: it is used to allow operations such as subtraction to mix the classes.
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
#' # The first test returns TRUE, the others return FALSE
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
  
  # Special case of zero rows and/or columns
  if (nrow == 0 || ncol == 0) {
    # Return 0x0 matrix, if the other dimension has not been provided
    if (missing(nrow) || missing(ncol))
      nrow <- ncol <- 0
    out <- list()
    dim(out) <- c(nrow, ncol)
    dimnames(out) <- dimnames
    class(out) <- c("uts_matrix", class(uts_vector()))
    return(out)
  }
  
  # Determine number of time series to work with
  if (is.uts(data))
    num_ts <- 1
  else if (is.uts_vector(data))
    num_ts <- length(data)
  else
    stop("The 'data' for a 'uts_matrix' needs to be a 'uts' or 'uts_vector'")
  
  # Check that nrow/ncol value compatible with the number of time series
  if (!missing(nrow) && (nrow > 1) && ((nrow * ncol) %% num_ts > 0) && (num_ts %% (nrow * ncol) > 0))
    stop("'data' length not a multiple or sub-multiple of the number of rows")
  if (!missing(ncol) && (ncol > 1) && ((nrow * ncol) %% num_ts > 0) && (num_ts %% (nrow * ncol) > 0))
    stop("'data' length not a multiple or sub-multiple of the number of columns")
  
  # In case not provided, guess nrows and ncols
  if (missing(nrow) && (num_ts >= ncol))
    nrow <- num_ts / ncol
  else if (missing(ncol) && (num_ts >= nrow))
    ncol <- num_ts / nrow
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


#' Coerce to a Data Frame
#'
#' Flatten a \code{\link{uts_matrix}} to a \code{\link{data.frame}}.
#' 
#' @note Only time series with atomic observation values can be coerced to a \code{data.frame}.
#' @note This method is helpful for saving a multivariate time series to a human-readable text file.
#' 
#' @param x a \code{"uts_matrix"} object.
#' @param method either \code{"long"} or \code{"wide"}, determining the shape of the output:
#' \itemize{
#'   \item \code{"long"}: a \code{data.frame} with one row for each observation for each time series in \code{x}. The \code{data.frame} has four columns denoting the source of each observation (i.e. from which row and column of \code{x} is the observation from?), the observation time, and the observation value.
#'   \item \code{"wide"}: a \code{data.frame} with one column for each time series in \code{x}. 
#' }
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso The \code{\link{uts_matrix_long}} and \code{\link{uts_matrix_wide}} constructors provide exactly the opposite funcitonality, i.e. they convert data in \emph{long} and \emph{wide} format, respectively, to a \code{uts_matrix}.
#' @examples
#' as.data.frame(ex_uts_matrix())
#' as.data.frame(ex_uts_matrix(), method="long")
as.data.frame.uts_matrix <- function(x, ..., method="wide")
{
  # Call method for uts_vector
  out <- as.data.frame.uts_vector(x, ..., method=method)
  
  # Extract row and columns names
  rnames <- rownames(x)
  if (is.null(rnames))
    rnames <- seq_len(nrow(x))
  cnames <- colnames(x)
  if (is.null(cnames))
    cnames <- seq_len(ncol(x))
  
  # Insert names to indentify rows and columns of observations
  if (method == "wide") {
    colnames(out)[-1] <- paste0("[", rep(rnames, length(cnames)), ", ", rep(cnames, each=length(rnames)), "]")
  } else if (method == "long") {
    row_pos <- ((out$name - 1) %% nrow(x)) + 1
    col_pos <- (out$name - row_pos) / ncol(x) + 1
    out <- data.frame(row=rnames[row_pos], column=cnames[col_pos], out[,-1])
  }
  out
}


