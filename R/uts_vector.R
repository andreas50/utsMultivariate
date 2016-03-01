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
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link[uts:time.uts]{time.uts}}
#' @examples
#' time(ex_uts_vector())
#' time(ex_uts_vector2())
time.uts_vector <- function(x, ...)
{
  # Merge time points
  times <- uts()$time
  for (x_j in x)
    times <- sorted_union(times, x_j$times, tolerance=.Machine$double.eps ^ 0.5)
  
  # Use POSIXTct attributes from first time series
  if (length(x) > 0)
    attributes(times) <- attributes(x[[1]]$times)
  times
}


#' Coerce to a Data Frame
#'
#' Flatten a \code{"uts_vector"} to a \code{data.frame}.
#' 
#' @note Only time series with atomic observation values can be coerced to a \code{data.frame}.
#' 
#' @param x a \code{"uts_vector"} object.
#' @param method either \code{"long"} or \code{"wide"}, determining the shape of the output:
#' \itemize{
#'   \item \code{"long"}: a \code{data.frame} with one row for each observation from one of the time series in \code{x}. The \code{data.frame} has three columns denoting the source of each observation (i.e. from which time series of \code{x} is the observation from)?, the observation time, and the observation value.
#'   \item code{"wide"}: a \code{data.frame} with one column for each time series in \code{x}. 
#' }
#' @param \dots arguments passed to \code{\link{format.POSIXct}}.
#' 
#' @seealso The \code{\link{uts_vector_long}} and \code{\link{uts_vector_wide}} constructors provide exactly the opposite funcitonality, i.e. they convert data in "long" and "wide" format, respectively, to a \code{uts_vector}.
#' @examples
#' as.data.frame(ex_uts_vector())
#' as.data.frame(ex_uts_vector(), method="long", format="%Y-%m-%d")
as.data.frame.uts_vector <- function(x, method="wide", ...)
{
  # Argument checking
  if (!all(sapply(ex_uts_vector(), function(x) is.atomic(x$values))))
    stop("Only time series with atomic observation values can be coerced to a data.frame")
  
  # Extract time series names
  num_ts <- length(x)
  ts_names <- names(x)
  if (is.null(ts_names))
    ts_names <- as.character(seq_len(num_ts))
  
  # Flatten the data
  if (method == "wide") {
    # Extract observation values
    times <- time(x)
    stop("Not working yet. Need sample_values.uts_vector()")
    out <- as.data.frame(sample_values(x, times, drop=FALSE, max_dt=ddays(0)))
    colnames(out) <- ts_names
    
    # Combine with observation times
    out <- cbind(time=format(times, ...), out)
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

