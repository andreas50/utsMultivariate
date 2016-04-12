###################################################
# Sampling values from a multivariate time series #
###################################################

#' Sample Values
#' 
#' Sample each time series in a \code{"uts_vector"} at given sampling times and combine the sampled values into a \code{matrix} or \code{data.frame}.
#'
#' @return A \code{matrix} if all sampled observation values are of the same \code{\link{type}}, and a \code{data.frame} otherwise. However, when using argument \code{drop=TRUE} the result may be simplified further.
#' @param x a \code{"uts_vector"} object where each individual \code{"uts"} as atomic observation values.
#' @param time_points a strictly increasing sequence of \code{\link{POSIXct}} date-times.
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest possible dimension. See the examples and \code{\link{drop}}.
#' @param \dots arguments passed to \code{\link[uts]{sample_values.uts}} for sampling from the individual time series.
#' 
#' @examples
#' times <- as.POSIXct(c("2007-11-09", "2007-11-10"))
#' 
#' # Sample with last-point and linear interpolation
#' sample_values(ex_uts_vector(), times)
#' sample_values(ex_uts_vector(), times, interpolation="linear")
#' 
#' # Sample with and without dropping of length-one dimensions
#' sample_values(ex_uts_vector(), times[1])
#' sample_values(ex_uts_vector(), times[1], drop=FALSE)
#' 
#' # Store sampled values in data.frame if of different type
#' x <- ex_uts()
#' y <- uts(letters[1:6], x$times)
#' utsv <- c(x, y)
#' sample_values(utsv, times)
#' 
#' # Error, because not all time series have atomic observation values
#' \dontrun{sample_values(ex_uts_vector2(), times)}
sample_values.uts_vector <- function(x, time_points, ..., drop=TRUE)
{ 
  # Argument checking
  is_atomic <- sapply(x, function(x) is.atomic(x$values))
  if (!all(is_atomic))
    stop("Not all time series have atomic observation values")
  
  # Sample each time series
  out <- lapply(x, sample_values, time_points, ...)
      
  # Combine results to matrix if sampled values are of same type, and data.frame otherwise
  types <- sapply(out, typeof)
  if (length(unique(types)) <= 1)
    out <- do.call(cbind, out)
  else
    out <- do.call(data.frame, out)
  colnames(out) <- names(x)
  
  # Drop length-one dimensions
  if (drop)
    out <- drop(out)
  out
}


#' Extract or Replace Parts of a uts_vector
#'
#' The accessor method (\code{"["}) extracts either (i) a sub-sampled time series vector, (ii) a subset of the time series vector, or (iii) both. The replacement method (\code{"[<-"}) inserts observation values at the provided observation times, replacing observations values for already existing observation times (if any).
#' 
#' @param x a \code{"uts)vector"} object.
#' @param i either a strictly increasing sequence of \code{\link{POSIXct}} date-times, or a \code{"uts"} or \code{"uts_vector"} with \code{\link{logical}} observation values.
#' @param j index specifying the time series to extract or replace. The index can be a \code{numeric} or \code{character} vector or empty (missing) or \code{NULL}. Numeric values are coerced to integer as by \code{\link{as.integer}} (and hence truncated towards zero). Character vectors will be matched to the \code{\link{names}} of the object.
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest possible dimension.
#' @param \dots further arguments passed to \code{\link[uts]{sample_values.uts}}.
#' 
#' @examples
#' # Extract subset of time series vector
#' x <- ex_uts_vector()
#' x[, 1]
#' x[, "oranges", drop=FALSE]
#' x[, 2:1]
#' 
#' # Subsampling using a POSIXct vector
#' x <- ex_uts_vector()
#' times <- as.POSIXct(c("2007-11-08 11:01:00", "2007-11-09 15:16:00"))
#' x[times]
#' x[times, interpolation="linear"]
#' 
#' # Subsampling using a "uts" with logical observation values
#' x <- ex_uts_vector()
#' x[ex_uts() > 48]
#' 
#' # Subsampling and subsetting at the same time
#' # 1.) using separate subsampling and subsetting indicies
#' x[times, "oranges"]
#' x[times, c(1, 2, 1)]
#' # 2.) using "uts_vector" with logical observation values
#' x[x > 48]
`[.uts_vector` <- function(x, i, j, drop=TRUE, ...)
{
  # Extract subset time series vector
  if (!missing(j)) {
    x <- unclass(x)
    x <- x[j]
    x[!sapply(x, is.uts)] <- NULL
    
    if (length(x) == 0)
      return(uts_vector())
    else if ((length(x) == 1) && drop)
      x <- x[[1]]
    else
      class(x) <- class(uts_vector())
  }
  if (missing(i))
    return(x)
  
  # Check argument consistency
  num_ts <- ifelse(is.uts_vector(x), length(x), 1)
  num_selector_i <- ifelse(is.POSIXct(i) | is.uts(i), 1, length(i))
  if ((num_ts > 1) && (num_selector_i > 1) && (num_ts != num_selector_i))
    stop("The dimension of the 'uts_vector' the and sampling points `i` do not match")
  
  # Special case if dimension of 'x' was dropped
  if (is.uts(x))
    return(x[i, ...])
  
  # Sample each element of 'x'
  if (is.POSIXct(i) || is.uts(i))
    sapply(x, "[", i, ...)
  else {
    for (k in seq_len(num_ts))
      x[[k]] <- x[[k]][i[[k]], ...]
    x
  }
}


#' @rdname sub-.uts_vector
#' 
#' @param value either (i) a vector of observation values, or a \code{"uts"} or \code{"uts_vector"}
#' 
#' @examples
#' # Replace subset of time series vector with a new time series
#' x <- ex_uts_vector()
#' x[, "oranges"] <- uts(values=50, times=Sys.time())
#' x[, "nuts"] <- head(ex_uts(), 2)
#' x$apples <- NULL
#' 
#' # Same, but use "uts_vector" for replacing
#' x <- c(ex_uts_vector(), nuts=ex_uts())
#' x[, 1:2] <- c(uts(), ex_uts())
#' x[, c("apples", "oranges")] <- NULL
`[<-.uts_vector` <- function(x, i, j, ..., value)
{
  # Replace subset time series vector
  if (missing(i)) {
    x <- unclass(x)
    if (is.uts(value))
      value <- uts_vector(value)
    x[j] <- value
    class(x) <- class(uts_vector())
    return(x)
  }
}


