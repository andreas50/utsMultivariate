#####################################
# First and last observation values #
#####################################

#' First and Last Observation Values
#' 
#' Get the first and last observation value, respectively, of each time series.
#' 
#' @return \code{first()} returns the first observation value of each time series.
#' @param x a \code{"uts_vector"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' first(ex_uts_vector())
#' last(ex_uts_vector2())
#' 
#' first(ex_uts_matrix())
first.uts_vector <- function(x, ...)
{
  sapply(x, first, ...)
}


#' @rdname first.uts_vector
#' 
#' @return \code{last()} returns the last observation value of each time series.
last.uts_vector <- function(x, ...)
{
  sapply(x, last, ...)
}
