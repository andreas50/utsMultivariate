####################################
# First and last observation times #
####################################

#' First and Last Observation Times
#' 
#' Get the first and last observation time, respectively, of each time series.
#' 
#' @return \code{start()} returns the first observation time of each time series as a \code{\link{POSIXct}} object. The time zone is determined by the first time series.
#' @param x a \code{"uts_vector"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' start(ex_uts_vector())
#' end(ex_uts_vector2())
#' 
#' # Return vector, because POSIXct objects cannot be stored in a matrix
#' start(ex_uts_matrix())
start.uts_vector <- function(x, ...)
{
  if (length(x) > 0) {
    # cannot use sapply() because the attributes are dropped by the call to unlist()
    tmp <- lapply(x, start)
    out <- do.call("c", tmp)
    with_tz(out, tz(tmp[[1]]))
  } else
    as.POSIXct(character())
}


#' @rdname start.uts_vector
#' 
#' @return \code{end()} returns the last observation time of each time series as a \code{\link{POSIXct}} object. The time zone is determined by the first time series.
end.uts_vector <- function(x, ...)
{
  if (length(x) > 0) {
    # cannot use sapply() because the class attribute gets lost
    tmp <- lapply(x, end)
    out <- do.call("c", tmp)
    with_tz(out, tz(tmp[[1]]))
  } else
    as.POSIXct(character())
}
