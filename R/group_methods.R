#' Summary Group Methods for uts_vector
#' 
#' Apply the \code{\link{Summary}} methods in base \R to the individual time series of a \code{"uts_vector"} or \code{"uts_matrix"} object.
#' 
#' @param x a \code{"uts_vector"} or \code{"uts_matrix"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{groupGeneric}}, \code{\link[uts]{Summary.uts}}
#' @examples
#' # Get the smallest observation value of each time, ignoring NAs
#' min(ex_uts_vector(), na.rm=TRUE)
#' 
#' # Check which time series have any observation value larger than 49
#' any(ex_uts_vector() > 49)
#' #which(ex_uts_matrix() > 49)
Summary.uts_vector <- function(x, ...)
{
  sapply(x, .Generic, ...)
}


#' Math Group Methods for uts_vector
#' 
#' Apply the \code{\link{Math}} methods in base \R{} to the observation values of \code{"uts_vector"} objects.
#' 
#' @param x a \code{"uts_vector"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{groupGeneric}}
#' 
#' @examples
#' # Take the base-2 logarithm of the observation values and return the corresponding "uts_vector"
#' log(ex_uts_vector(), base=2)
#' 
#' # Calculate the cumulative sum of the observation values and return the corresponding "uts_vector"
#' cumsum(ex_uts_vector())
Math.uts_vector <- function(x, ...)
{
  sapply(x, .Generic, ...)
}
if (0) {
  methods("Math")
}


