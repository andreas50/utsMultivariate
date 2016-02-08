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
#' A wrapper around \code{\link[base:sapply]{sapply}} in base \R. If possible, it further simplifies the output to a \code{"uts_vector"} or \code{"uts_matrix"}.
#' 
#' @param X a \code{"uts_vector"} or \code{"uts_amtrixr"} object.
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
#' 
#' # Results that are further simplified to a matrix "uts_matrix"
#' sapply(ex_uts_matrix(), length)
#' sapply(ex_uts_matrix(), sqrt)
sapply.uts_vector <- function(X, ...)
{
  out <- base::sapply(X, ...)

  # Simpify output further, if possible
  is_uts <- sapply(out, is.uts)
  if (all(is_uts) && (length(out) > 0))
    out <- do.call("c", out)
  out
}


#' @rdname sapply.uts_vector
sapply.uts_matrix <- function(X, ...)
{
  out <- NextMethod()
  
  # Return result if cannot simpified further
  if (length(X) != length(out))
    return(out)
  is_uts <- sapply(out, is.uts)
  is_atomic <- sapply(out, is.atomic)
  if (!all(is_atomic) && !all(is_uts))
    return(out)
  
  # If possible, simplify output to matrix or uts_matrix
  attributes(out) <- attributes(X)
  if (!all(is_uts))  # coerce from uts_matrix to matrix
    class(out) <- "matrix"
  out
}

