####################################
# First and last observation times #
####################################

#' First and Last Observation Times
#' 
#' Get the first and last observation time, respectively, of each time series.
#' 
#' @return \code{end()} returns the first observation time of each time series.
#' @param x a \code{"uts_vector"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' start(ex_uts_vector())
start.uts_vector <- function(x, ...)
{
  if (length(x) > 0) {
    # cannot use sapply() because class attribute gets lost
    do.call("c", lapply(x, start))
  } else
    as.POSIXct(character())
}


#' @rdname start.uts_vector
#' 
#' @return \code{end()} returns the last observation time of each time series.
#' 
#' @examples 
#' end(ex_uts_vector2())
end.uts_vector <- function(x, ...)
{
  if (length(x) > 0) {
    # cannot use sapply() because class attribute gets lost
    do.call("c", lapply(x, end))
  } else
    as.POSIXct(character())
}