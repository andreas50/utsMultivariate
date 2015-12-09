#################################
# apply() and related functions #
#################################

#' Generic sapply function
#'
#' The function is needed, because \code{\link[base:sapply]{sapply}} of base \R is not generic.
#' 
#' @note
#' As recommended in Section 7.1 ("Adding new generics") of "Writing R Extensions", the implementation of \code{\link{sapply.default}} has been made a wrapper around \code{\link[base:sapply]{base::sapply}}.
#' 
#' @param X an \R object.
#' 
#' @keywords internal
sapply <- function(X, ...) UseMethod("sapply")


#' @describeIn sapply simply calls the default implementation of base \R
#' @keywords internal
sapply.default <- function(X, ...) base::sapply(X, ...)


#' Apply a Function over a Multivariate Time Series
#' 
#' @param X a \code{"uts_vector"} object.
#' @param FUN the function to be applied to each element of \code{X}.
#' @param \ldots optional arguments to \code{FUN}.
#' 
#' @examples
#' sapply(ex_uts_vector(), length)
#' sapply(ex_uts_vector(), range)
#' sapply(ex_uts_vector(), log)
sapply.uts_vector <- function(X, FUN, ...)
{
  # Call base::sapply()
  out <- base::sapply(X, FUN=FUN, ...)

  # Simpify output further, if possible
  is_uts <- sapply(out, is.uts)
  if (all(is_uts))
    out <- do.call("c", out)
  out
}
