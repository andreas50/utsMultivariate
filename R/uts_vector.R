####################
# UTS_VECTOR class #
####################

#' Unevenly-spaced Time Series Vector
#' 
#' Create a vector of unevenly spaced time series (\code{"uts_vector"}).
#' 
#' @note A virtual class \code{"uts_virtual"} exists from which \code{"uts"}, \code{"uts_vector"}, \code{"uts_matrix"}, and \code{"uts_data_frame"} inherit: it is used to allow operations such as subtraction to mix the classes.
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


#' Create uts_vector from wide tabular data
#' 
#' Create a \code{"uts_vector"} from \emph{wide} tabular data (see \href{https://en.wikipedia.org/wiki/Wide_and_narrow_data}{Wikipedia}). For data in this format, values in the same row are measurements at the same time point, either across multiple entities (e.g. persons, countries) or of multiple attributes (e.g. several economic indicators, blood work results). Values in the same column are measurements of the same entity or attribute over time.
#' 
#' @return An object of class \code{"uts_vector"}. The number of time series is equal to the number of columns of \code{values}. The length of each time series is equal to the number of rows of \code{values}.
#' @param values a matrix or data.frame. Each row is a vector of observations at a specific time point.
#' @param times a \code{\link{POSIXct}} object. The observation times of the rows of \code{values}.
#' @param names a character vector. The entity/attribute names of the columns of \code{values}. By default, the column names of \code{values} are used.
#' 
#' @keywords ts classes
#' @seealso \code{\link{uts_vector_long}}
#' @examples 
#' values <- data.frame(apples=1:10, oranges=letters[1:10],
#'   bananas=month.name[1:10], stringsAsFactors=FALSE)
#' uts_vector_wide(values, times=as.POSIXct("2015-01-01") + ddays(1:10))
uts_vector_wide <- function(values, times, names=colnames(values))
{
  # Argument checking
  if (!is.matrix(values) && !is.data.frame(values))
    stop("The data is not in matrix or data.frame format")
  if (nrow(values) != length(times))
    stop("The number of sources values does not match the number of observation times")
  if (!is.null(names) && (length(names) != ncol(values)))
    stop("The number of observation entities does not match the number of source names")
  if (!is.POSIXct(times))
    stop("The observation time vector is not a POSIXct object")
  
  # Order data chronologically
  o <- order(times)
  times <- times[o]
  values <- values[o,,drop=FALSE]

  # Generate "uts_vector"
  out <- uts_vector()
  for (j in seq_len(ncol(values)))
    out[[j]] <- uts(values[, j], times)
  names(out) <- names
  out
}


#' Create uts_vector from long tabular data
#' 
#' Create a \code{"uts_vector"} from \emph{long} (also known as \emph{narrow}) tabular data. Data in this format has three different columns; the observation values, the observation times, and the entity/attribute name (e.g. person, country, economic indicator) of each observation.
#' 
#' @return An object of class \code{"uts_vector"} with length given by to the number of distinct sources.
#' @param values a vector observation values.
#' @param times a \code{\link{POSIXct}} object. The matching observation times.
#' @param names a character vector. The matching entity/attribute names of the observations. By default, the names of \code{values} are used.
#' 
#' @keywords ts classes
#' @seealso \code{\link{uts_vector_wide}}
#' @examples 
#' values <- c(a=5, b=6, a=6, a=7)
#' times <- as.POSIXct("2016-01-01") + dhours(1:4)
#' uts_vector_long(values, times)
#' 
#' uts_vector_long(values=1:10, times=as.POSIXct("2016-01-01") + days(1:10),
#'   names=c("a", "a", "a", "a", "a", "c", "c", "b", "b", "b"))
uts_vector_long <- function(values, times, names=base::names(values))
{
  # Argument checking
  if (length(values) != length(times))
    stop("The number of observation values does not match the number of observation times")
  if (!is.POSIXct(times))
    stop("The observation time vector is not a POSIXct object")
  if (length(values) != length(names))
    stop("The length of the source names does not match the number of observation values")
  
  # Order data chronologically
  o <- order(times)
  times <- times[o]
  values <- values[o] 
  names <- names[o]
  
  # Determine list of indices for each unique name
  indices <- split(seq_along(names), names)

  # Insert data
  out <- uts_vector()
  for (j in seq_along(indices)) {
    # Insert UTS for j-th name
    pos <- indices[[j]]
    out[[j]] <- uts(values[pos], times[pos])
  }
  names(out) <- names(indices)
  out  
}
