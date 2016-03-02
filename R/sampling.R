###################################################
# Sampling values from a multivariate time series #
###################################################

#' Sample Values
#' 
#' Sample observation values from a \code{"uts_vector"} at given sampling times.
#'
#' @return A \code{matrix} if all sampled observation values are of the same \code{\link{type}}, and a \code{data.frame} otherwise. However, when using argument \code{drop=TRUE} the result may be simplified further.
#' @param x a \code{"uts_vector"} object where each individual \code{"uts"} as atomic observation values.
#' @param time_points a strictly increasing sequence of \code{\link{POSIXct}} date-times.
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest possible dimension. See the examples and \code{\link{drop}}.
#' @param \dots arguments passed to \code{\link[uts:sample_values]{sample_values.uts}} for sampling from the individual time series.
#' 
#' @examples
#' times <- as.POSIXct(c("2007-11-09", "2007-11-10"))
#' 
#' # Sample with last-point and linear interpolation
#' sample_values(ex_uts_vector(), times)
#' sample_values(ex_uts_vector(), times, method="linear")
#' 
#' # Sample with and without dropping of length-one dimensions
#' sample_values(ex_uts_vector(), times[1])
#' sample_values(ex_uts_vector(), times[1], drop=FALSE)
#' 
#' # Error, because cannot sample a time series with non-atomic observation values
#' \dontrun{sample_values(ex_uts_vector2(), times)}
#' 
#' # Error, because only numeric time series can be linearly interpolated
#' \dontrun{sample_values(ex_uts2(), as.POSIXct("2007-01-01"), method="linear")}
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
  
  # Drop length-one dimensions
  if (drop)
    out <- drop(out)
  out
}

