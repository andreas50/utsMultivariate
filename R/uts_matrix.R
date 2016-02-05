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
#' # One of the dimensions has length zero
#' uts_matrix(nrow=0, ncol=4)
#' uts_matrix(nrow=4, ncol=0)
#' uts_matrix(nrow=0, ncol=0)
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
    num_ts <- 1
  else if (is.uts_vector(data))
    num_ts <- length(data)
  else
    stop("The 'data' for a 'uts_matrix' needs to be a 'uts' or 'uts_vector'")
  
  # Check that nrow/ncol value compatible with the number of time series
  if (!missing(nrow) && (nrow > 1) && ((nrow * ncol) %% num_ts > 0))
    stop("'data' length not a multiple or sub-multiple of the number of rows")
  if (!missing(ncol) && (ncol > 1) && ((nrow * ncol) %% num_ts > 0))
    stop("'data' length not a multiple or sub-multiple of the number of columns")
  
  # Guess nrows and ncols
  if (missing(nrow) && (ncol > 0))
    nrow <- num_ts / ncol
  else if (missing(ncol) && (nrow > 0))
    ncol <- num_ts / nrow
  if ((num_ts > nrow * ncol) && (nrow * ncol > 0))
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
#' Create a \code{"uts_matrix"} from \emph{long} (also known as \emph{narrow}) tabular data. Data in this format has four different columns; the observation values, the observation times, the entity name of each observation (e.g. person, country), and the attribute/field name of each observation (e.g. which one of serveral economic indicators, which one of several blood measurement values).
#' 
#' @return An object of class \code{"uts_matrix"}. The time series in row \code{entity_name} and column \code{field_name} contains all observations of such entity for such field.
#' @return The number of rows is given by to the number of distinct entity names (parameter \code{names}), while the number of columns is given by the number of distinct attribute/field names (parameter \code{fields}).
#' @param values a vector observation values.
#' @param times a \code{\link{POSIXct}} object. The matching observation times.
#' @param names a character vector. The the matching entity names for the observations. By default, the names of \code{values} are used.
#' @param fields a character vector. The the matching attribute/field names for the observations.
#' 
#' @seealso \code{\link{uts_matrix_wide}}
#' @keywords ts classes
#' @examples
#' values <- c(A=1, A=2, B=3, B=4, A=5)
#' times <- as.POSIXct("2016-01-01") + dhours(1:5)
#' uts_matrix_long(values, times, fields=c("c", "d", "d", "d", "d"))
#' 
#' 
#' uts_matrix_long(values=1:5, times=as.POSIXct("2015-01-01") + days(1:5),
#'   names=c("A", "A", "B", "B", "A"), fields=c("c", "d", "d", "d", "d"))
uts_matrix_long <- function(values, times, names=base::names(values), fields)
{
  # Argument checking
  if (length(values) != length(times))
    stop("The number of observation values does not match the number of observation times")
  if (length(values) != length(names))
    stop("The number of observation values does not match the number of entity names")
  if (length(values) != length(fields))
    stop("The number of observation values does not match the number of attribute/field names")
  if (!is.POSIXct(times))
    stop("The observation time vector is not a POSIXct object")
  
  # Order data chronologically
  o <- order(times)
  times <- times[o]
  values <- values[o] 
  names <- names[o]
  fields <- fields[o]
  
  # Allocate memory for output
  rnames <- sort(unique(names))
  cnames <- sort(unique(fields))
  nrows <- length(rnames)
  ncols <- length(cnames)
  out <- uts_matrix(uts(), ncol=ncols, nrow=nrows, dimnames=list(rnames, cnames))
  
  # Determine values and times for each (row,column) pair
  keys <- paste(rep(rnames, ncols), rep(cnames, each=nrows), sep="$$$")
  all_keys <- paste(names, fields, sep="$$$")
  pos <- match(all_keys, keys)
  values_ij <- split(values, pos)
  times_ij <- split(times, pos)

  # Insert individual UTS
  index <- as.numeric(names(values_ij))
  for (j in seq_along(index))
    out[[index[j]]] <- uts(values_ij[[j]], times_ij[[j]])
  out
}


#' Create uts_matrix from wide tabular data
#' 
#' Create a \code{"uts_matrix"} from \emph{wide} tabular data (see \href{https://en.wikipedia.org/wiki/Wide_and_narrow_data}{Wikipedia}). For data in this format, each row is a vector of observations (also known as a \emph{record}) for a specific entity (e.g. person, country) at a specific time point. The record consists of measurements across multiple attributes/fields (e.g. several economic indicators, several blood measurement values).
#' 
#' @return An object of class \code{"uts_matrix"}. The time series in row \code{entity_name} and column \code{field_name} contains all observations of such entity for such field.
#' @return The number of rows is given by to the number of distinct entity names (parameter \code{names}), while the number of columns is given by the number of attributes/fields, i.e. the  number of columns of \code{values}.
#' @param values a matrix or data.frame of observation values.
#' @param times a \code{\link{POSIXct}} object. The matching observation times for the records (i.e. rows of \code{values}).
#' @param names a character vector. The the matching entity names for the records. By default, the row names of \code{values} are used.
#' @param fields a character vector. The the attribute/field names of the records. By default, the column names of \code{values} are used.
#' 
#' @seealso \code{\link{uts_matrix_long}}
#' @keywords ts classes
#' @examples 
#' values <- matrix(1:8, 4, 2)
#' rownames(values) <- c("CH", "CH", "FR", "US")
#' colnames(values) <- c("population", "size")
#' times <- as.POSIXct("2016-01-01") + days(1:4)
#' uts_matrix_wide(values, times)
#' 
#' # Same, but manually provide entity names
#' uts_matrix_wide(values, times, names=c("China", "China", "France", "USA"))
uts_matrix_wide <- function(values, times, names=base::rownames(values), fields=base::colnames(values))
{
  # Argument checking
  if (!is.matrix(values) && !is.data.frame(values))
    stop("The observation values are not stored in a matrix or data.frame")
  if (nrow(values) != length(times))
    stop("The number of observation rows does not match the number of observation times")
  if (nrow(values) != length(names))
    stop("The number of observation rows does not match the number of entity names")
  if (ncol(values) != length(fields))
    stop("The number of observation columns does not match the number of attribute/field names")
  if (!is.POSIXct(times))
    stop("The observation time vector is not a POSIXct object")
  
  # Allocate memory for output
  if (length(names) > 0)
    rnames <- sort(unique(names))
  else
    rnames <- NULL
  if (length(fields) > 0)
    cnames <- sort(unique(fields))
  else
    cnames <- NULL
  nrows <- length(rnames)
  ncols <- length(cnames)
  out <- uts_matrix(uts(), ncol=ncols, nrow=nrows, dimnames=list(rnames, cnames))

  # Insert data one column at a time
  for (col in seq_len(ncols)) {
    out_col <- uts_vector_long(values[, col], times, names=names)
    for (row in seq_len(nrows))
      out[[row + (col-1)*nrows]] <- out_col[[row]]
  }
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

