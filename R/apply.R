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
#' @param further arguments passed to or from methods.
#' 
#' @keywords internal
sapply <- function(X, ...) UseMethod("sapply")


#' @describeIn sapply simply calls the default implementation of base \R
#' @keywords internal
sapply.default <- function(X, ...) base::sapply(X, ...)


#' Apply a Function over a Multivariate Time Series
#' 
#' A wrapper around \code{\link[base:sapply]{sapply}} in base \R. If possible, it further simplifies the output to a \code{"uts_vector"}, \code{"uts_matrix"}, or \code{"uts_data_frame"}.
#' 
#' @param X a \code{"uts_vector"} object.
#' @param \ldots arguments passed to \code{sapply} in base \R.
#' 
#' @examples
#' # Same results as sapply() in base R
#' sapply(ex_uts_vector(), length)
#' sapply(ex_uts_vector(), range)
#' 
#' # Results that are further simplified to a "uts_vector"
#' sapply(ex_uts_vector(), log)
#' sapply(ex_uts_vector2(), lag_t, ddays(1))
sapply.uts_vector <- function(X, ...)
{
  out <- base::sapply(X, ...)

  # Simpify output further, if possible
  is_uts <- sapply(out, is.uts)
  if (all(is_uts) && (length(out) > 0))
    out <- do.call("c", out)
  out
}

