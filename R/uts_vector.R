####################
# UTS_VECTOR class #
####################

#' Unevenly-spaced Time Series Vector
#' 
#' Create a vector of unevenly spaced time series (\code{"uts_vector"}).
#' 
#' @note An abstract class \code{"uts_virtual"} exists from which \code{"uts"}, \code{"uts_vector"}, and \code{"uts_matrix"} inherit: it is used to allow operations such as subtraction to mix the classes.
#'
#' @return An object of class \code{"uts_vector"}.
#' @param \dots zero or more \code{\link{uts}} objects.
#' 
#' @seealso \code{\link{rep.uts}}, \code{\link{uts_vector_long}}, \code{\link{uts_vector_wide}} for alternative constructors.
#' 
#' @keywords ts classes
#' @examples
#' # Two equivalent constructors
#' uts_vector(apples=ex_uts(), oranges=ex_uts2())
#' c(apples=ex_uts(), oranges=ex_uts2())
#' 
#' # Create "uts_vector" out of both "uts" and "uts_vector" objects
#' c(ex_uts(), ex_uts_vector(), kiwis=ex_uts2())
#' 
#' # Empty "uts_vector"
#' uts_vector()
#' 
#' # The first two test return TRUE, the other returns FALSE
#' is.uts_vector(uts_vector())
#' is.uts_matrix(uts_matrix())      # "uts_matrix" inherits from "uts_vector"
#' is.uts_vector(ex_uts())
uts_vector <- function(...)
{
  c.uts(...)
}


#' @describeIn uts_vector constructor for \code{"uts_vector"} object out of \code{"uts"} objects.
c.uts <- function(...)
{
  # Check arguments
  args <- list(...)
  if (!all(sapply(args, function(x) is.uts(x) || is.uts_vector(x))))
    stop("Not all arguments are 'uts' objects")
  
  # Allocate memory for output
  num_uts <- 0
  out <- list()
  names <- c()

  # Merge arguments
  for (num_arg in seq_along(args)) {
    # Merge "uts"
    obj <- args[[num_arg]]
    if (is.uts(obj)) {
      num_uts <- num_uts + 1
      if (length(names(args)[num_arg]) > 0)   # Extract name of "uts"
        names <- c(names, names(args)[num_arg])
      else
        names <- c(names, "")
      out[[num_uts]] <- obj
    }
    
    # Merge "uts_vector"
    if (is.uts_vector(obj)) {
      for (k in seq_along(obj)) {
        num_uts <- num_uts + 1
        if (length(names(obj)[k]) > 0)  # Extract name of k-th "uts_vector" element
          names <- c(names, names(obj)[k])
        else
          names <- c(names, "")
        out[[num_uts]] <- obj[[k]]
      }
    }
  }

  # Set attributes
  if (any(names != ""))
    names(out) <- names
  class(out) <- c("uts_vector", "uts_virtual")
  out
}


#' @describeIn uts_vector constructor for \code{"uts_vector"} object out of \code{"uts"} and other \code{"uts_vector"} objects.
c.uts_vector <- function(...)
{
  c.uts(...)
}


#' @rdname uts_vector
#' 
#' @description \code{is.uts_vector} returns \code{TRUE} if its argument is a \code{"uts_vector"} object.
#' 
#' @param x an \R object.
#' 
#' @keywords internal
is.uts_vector <- function(x)
{
  inherits(x, "uts_vector")
}


#' Repeat uts and uts_vector
#' 
#' Create a \code{"uts_vector"} by replicating the individual \code{"uts"} of the input \code{x}.
#' 
#' This method is a wrapper around \code{\link{rep}} in base \R that makes sure the returned object is of class \code{"uts_vector"}.
#'
#' @return An object of class \code{"uts_vector"}.
#' @param x a \code{"uts"} or \code{"uts_vector"}.
#' @param \dots further arguments passed to \code{\link{rep}} in base \R.
#' 
#' @examples
#' # Repeat "uts"
#' rep(ex_uts(), 4)
#' 
#' # Repeat "uts_vector"
#' rep(ex_uts_vector(), times=3)
#' rep(ex_uts_vector(), each=3)
rep.uts <- function(x, ...)
{
  out <- base::rep(list(x), ...)
  do.call(c.uts, out)
}


#' @rdname rep.uts
rep.uts_vector <- function(x, ...)
{
  out <- base::rep(unclass(x), ...)
  do.call(c.uts_vector, out)
}


#' Observation Times
#' 
#' Get the sorted union of observation times of a \code{"uts_vector"} object.
#' 
#' @param x a \code{"uts_vector"} object.
#' @param tolerance a non-negative number, indicating the tolerance for numerical noise in the observation times. Observation times less than this threshold apart are treated as identical.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link[uts]{time.uts}}, \code{\link[uts]{sorted_union}}
#' @examples
#' time(ex_uts_vector())
#' time(ex_uts_vector2())
time.uts_vector <- function(x, tolerance=.Machine$double.eps ^ 0.5, ...)
{
  # Merge time points
  times <- uts()$time
  for (x_j in x)
    times <- sorted_union(times, x_j$times, tolerance=tolerance)
  
  # Use POSIXTct attributes from first time series
  if (length(x) > 0)
    attributes(times) <- attributes(x[[1]]$times)
  times
}


#' Coerce to a Data Frame
#'
#' Flatten a \code{\link{uts_vector}} to a \code{\link{data.frame}}.
#' 
#' @note Only time series with atomic observation values can be coerced to a \code{data.frame}.
#' @note This method is helpful for saving a multivariate time series to a human-readable text file.
#' 
#' @param x a \code{"uts_vector"} object.
#' @param method either \code{"long"} or \code{"wide"}, determining the shape of the output:
#' \itemize{
#'   \item \code{"long"}: a \code{data.frame} with one row for each observation for each time series in \code{x}. The \code{data.frame} has three columns denoting the source of each observation (i.e. from which time series of \code{x} is the observation from?), the observation time, and the observation value.
#'   \item \code{"wide"}: a \code{data.frame} with one column for each time series in \code{x}. 
#' }
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso The \code{\link{uts_vector_long}} and \code{\link{uts_vector_wide}} constructors provide exactly the opposite funcitonality, i.e. they convert data in \emph{long} and \emph{wide} format, respectively, to a \code{uts_vector}.
#' @examples
#' as.data.frame(ex_uts_vector())
#' as.data.frame(ex_uts_vector(), method="long")
as.data.frame.uts_vector <- function(x, ..., method="wide")
{
  # Argument checking
  if (!all(sapply(ex_uts_vector(), function(x) is.atomic(x$values))))
    stop("Only time series with atomic observation values can be coerced to a data.frame")
  
  # Extract time series names
  num_ts <- length(x)
  ts_names <- names(x)
  if (is.null(ts_names))
    ts_names <- seq_len(num_ts)
  
  # Flatten the data
  if (method == "wide") {
    # Extract observation values
    times <- time(x)
    out <- as.data.frame(sample_values(x, times, drop=FALSE, max_dt=ddays(0)), stringsAsFactors=FALSE)
    colnames(out) <- ts_names
    
    # Combine with observation times
    out <- cbind(time=times, out, stringsAsFactors=FALSE)
  } else if (method == "long") {
    out <- lapply(x, as.data.frame, ...)
    for (j in 1:num_ts)
      out[[j]] <- cbind(name=ts_names[j], out[[j]], stringsAsFactors=FALSE)
    out <- do.call(rbind, out)
    rownames(out) <- NULL
  } else
    stop("Unknown 'method'")
  out
}


#' Convert uts_matrix to uts_vector
#' 
#' Convert a \code{\link{uts_matrix}} to a \code{\link{uts_vector}} by dropping all \code{uts_matrix}-specific attributes.
#' 
#' @param x a \code{"uts_matrix"} object.
#' @param USE.NAMES logical. Whether to assign sensible names to the output based on the row and column names of \code{x}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{uts_matrix}} for the exactly the opposite functionality, i.e. for converting a \code{"uts_vector"} to a \code{"uts_matrix"}.
#' @examples
#' as.uts_vector(ex_uts_matrix())
#' as.uts_vector(ex_uts_matrix(), USE.NAMES=FALSE)
as.uts_vector.uts_matrix <- function(x, USE.NAMES=TRUE, ...)
{
  # Remove uts_matrix attributes
  out <- x
  attr(out, "dim") <- NULL
  attr(out, "dimnames") <- NULL
  class(out) <- class(uts_vector())
  
  # Use row- and column names to get names for output
  if (USE.NAMES && (length(x) > 0)) {
    rnames <- rownames(x)
    if (is.null(rnames))
      rnames <- seq_len(nrow(x))
    cnames <- colnames(x)
    if (is.null(cnames))
      cnames <- seq_len(ncol(x))
    
    names(out) <- paste0("[", rep(rnames, length(cnames)), ", ", rep(cnames, each=length(rnames)), "]")
  }
  out
}

