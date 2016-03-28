#' Summary Group Methods for uts_vector
#' 
#' Apply the \code{\link{Summary}} methods in base \R{} to the time series of \code{"uts_vector"} objects.
#' 
#' @param x a \code{"uts_vector"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{groupGeneric}}, \code{\link[uts]{Summary.uts}}
#' @examples
#' # Get the smallest observation value of each time, ignoring NAs
#' min(ex_uts_vector(), na.rm=TRUE)
#' 
#' # Check which time series have any observation value larger than 49
#' any(ex_uts_vector() > 49)
Summary.uts_vector <- function(x, ...)
{
  sapply(x, .Generic, ...)
}
