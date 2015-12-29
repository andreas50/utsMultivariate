####################
# UTS_VECTOR class #
####################

#' Unevenly-spaced Time Series Vector
#' 
#' Create a vector of unevenly spaced time series (\code{"uts_vector"}).
#'
#' @return An object of class \code{"uts_vector"}.
#' @param \dots zero or more \code{\link{uts}} objects.
#' 
#' @seealso \code{\link{uts_vector_wide}}, \code{uts_vector_long} for alternative constructors.
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
  class(out) <- c("uts_vector", "list")
  out
}


#' @describeIn uts_vector constructor for \code{"uts_vector"} object out of \code{"uts"} and other \code{"uts_vector"} objects.
c.uts_vector <- function(...)
{
  c.uts(...)
}


#' Create uts_vector from wide tabular data
#' 
#' Create a \code{"uts_vector"} from wide tabular data (see \href{https://en.wikipedia.org/wiki/Wide_and_narrow_data}{Wikipedia}). With this input format, values in the same row are measurements at the same point in time, while values in the same column are measurements of the same variable.
#' 
#' @return An object of class \code{"uts_vector"}. The number of time series is equal to the number of columns of \code{values}. The length of each time series is equal to the number of rows of \code{values}.
#' @param values a matrix or data.frame. Each row represents a vector of observations at a specific time point.
#' @param times a \code{\link{POSIXct}} object. The observation times of the rows of \code{values}.
#' @param names a character vector, indicating the source of each column in \code{values}. By default, the column names of \code{values} are used.
#' 
#' @keywords ts classes
#' @examples 
#' data <- data.frame(apples=1:10, oranges=letters[1:10], bananas=month.name[1:10])
#' uts_vector_wide(data, times=as.POSIXct("2015-01-01") + ddays(1:10))
uts_vector_wide <- function(values, times, names=colnames(values))
{
  # Argument checking
  if (!is.matrix(values) && !is.data.frame(values))
    stop("The data is not in matrix or data.frame format")
  if (nrow(values) != length(times))
    stop("The number of observation value vectors does not match the number of observation times")
  if (!is.null(names) && (length(names) != ncol(values)))
    stop("The numnber of observation variables does not match the number of variable names")
  
  # Order data by chronologically
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


#' Is Object a uts_vector?
#' 
#' Return \code{TRUE} if and only if the argument is a \code{"uts_vector"} object.
#'  
#' @param x an \R object.
#' 
#' @keywords internal
#' @examples
#' is.uts_vector(uts_vector())
#' is.uts_vector(ex_uts_vector())
#' is.uts_vector(5)
is.uts_vector <- function(x)
{
  inherits(x, "uts_vector")
}


#' Print Values
#' 
#' @param x an object of class \code{"uts_vector"}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
#' @seealso \code{\link[base:print]{print}}
#' @examples
#' uts_vector()
#' print(uts_vector(a=ex_uts(), b=ex_uts2()))
print.uts_vector <- function(x, ...)
{
  # Special case of empty "uts_vector"
  num_uts <- length(x)
  if (num_uts == 0) {
    cat("-----------------\n")
    cat("uts_vector object\n")
    cat("-----------------\n")
    cat("No time series available at this time\n")
    return()
  }
  
  # Determine start and end times
  start <- as.character(start(x), usetz=TRUE)
  end <- as.character(end(x), usetz=TRUE)

  # Extract time series stats
  Name <- names(x)
  if (length(Name) < num_uts)
    Name <- rep(NA, num_uts)
  #stats <- data.frame(Name, Datapoints=lengths(x), start, end)   # requires R (>= 3.2.0)
  stats <- data.frame(Name, Datapoints=sapply(x, length), start, end)
  rownames(stats) <- 1:num_uts

  # Print nice description
  cat("\nIndividual time series characteristics:\n")
  cat("---------------------------------------\n")
  print(stats)
}



