##############################################################
# Methods that calculate summaries of the observation values #
##############################################################

#' Summary of Time Series Values
#' 
#' Apply \code{\link{summary}} from base \R to the observation values of a multivariate time series.
#' 
#' @note This method primarily exists because \code{\link{summary.default}} produces an error message.
#'  
#' @param object a \code{"uts_vector"} or \code{"uts_matrix"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
#' @seealso \code{\link[uts]{summary.uts}}
#' @examples
#' summary(ex_uts_vector())
#' summary(ex_uts_matrix())
summary.uts_vector <- function(object, ...)
{
  out <- sapply(object, summary, ...)
  if (is.matrix(out))
    out <- t(out)
  out
}


#' @rdname summary.uts_vector
summary.uts_matrix <- function(object, ...)
{
  summary(as.uts_vector(object), ...)
}
