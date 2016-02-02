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
#' # The first tests returns TRUE, the others return FALSE
#' is.uts_matrix(uts_matrix())
#' is.uts_matrix(uts_vector())
#' is.uts_matrix(ex_uts())
uts_matrix <- function(data=uts(), nrow=1, ncol=1, byrow=FALSE, dimnames=NULL)
{
  # Argument checking
  if (is.na(nrow) || nrow <= 0 || is.infinite(nrow))
    stop("Invalid 'nrow' value")
  if (is.na(ncol) || ncol <= 0 || is.infinite(ncol))
    stop("Invalid 'ncol' value")
  
  # Determine number of time series to work with
  if (is.uts(data))
    num_ts <- 1
  else if (is.uts_vector(data)) {
    num_ts <- length(data)
    if (num_ts == 0)
      stop("The 'data' needs to contain at least one time series")
  } else
    stop("The 'data' for a 'uts_matrix' needs to be a 'uts' or 'uts_vector'")
  
  # Check that nrow/ncol value compatible with the number of time series
  if (!missing(nrow) && (nrow > 1) && ((nrow * ncol) %% num_ts > 0))
    stop("'data' length not a multiple or sub-multiple of the number of rows")
  if (!missing(ncol) && (ncol > 1) && ((nrow * ncol) %% num_ts > 0))
    stop("'data' length not a multiple or sub-multiple of the number of columns")
  
  # Guess nrows and ncols
  if (missing(nrow))
    nrow <- num_ts / ncol
  else if (missing(ncol))
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


#' Create uts_matrix from long tabular data
#' 
#' Create a \code{"uts_matrix"} from \emph{long} (also known as \emph{narrow}) tabular data. Data in this format has four different columns; the observation values, the observation times, and the row and column name (also known as \emph{record} and \emph{field}) of each observation.
#' 
#' @return An object of class \code{"uts_matrix"}. The number of rows is given by to the number of records (distinct \code{rownames}), while the number of columns is given by the number of fields (distinct \code{colnames}).
#' @param values a vector observation values.
#' @param times a \code{\link{POSIXct}} object. The matching observation times.
#' @param rownames a character vector. The the row name (also known as \emph{record}) of each observation.
#' @param colnames a character vector. The the column names (also known as \emph{field}) of each observation.
#' 
#' @keywords ts classes
#' @examples 
#' uts_matrix_long(values=1:5, times=as.POSIXct("2015-01-01") + days(1:5),
#'   colnames=c("A", "A", "B", "B", "A"), rownames=c("c", "d", "d", "d", "d"))
uts_matrix_long <- function(values, times, rownames, colnames)
{
  # Argument checking
  if (length(values) != length(times))
    stop("The number of observation values does not match the number of observation times")
  if (length(values) != length(rownames))
    stop("The number of observation values does not match the number of row names")
  if (length(values) != length(colnames))
    stop("The number of observation values does not match the number of column names")
  if (!is.POSIXct(times))
    stop("The observation time vector is not a POSIXct object")
  
  # Order data chronologically
  o <- order(times)
  times <- times[o]
  values <- values[o] 
  rownames <- rownames[o]
  colnames <- colnames[o]
  
  # Allocate memory for output
  rnames <- unique(rownames)
  cnames <- unique(colnames)
  nrows <- length(rnames)
  ncols <- length(cnames)
  out <- uts_matrix(uts(), ncol=ncols, nrow=nrows, dimnames=list(rnames, cnames))
  
  # Determine values and times for each (row,column) pair
  keys <- paste(rep(rnames, ncols), rep(cnames, each=nrows), sep="$$$")
  all_keys <- paste(rownames, colnames, sep="$$$")
  pos <- match(all_keys, keys)
  values_ij <- split(values, pos)
  times_ij <- split(times, pos)

  # Insert individual UTS
  index <- as.numeric(names(values_ij))
  for (j in seq_along(index))
    out[[index[j]]] <- uts(values_ij[[j]], times_ij[[j]])
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

