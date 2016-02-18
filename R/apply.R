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
#' Same as \code{\link[base:sapply]{sapply}} in base \R, but further simplifies the output to a \code{"uts_vector"} or \code{"uts_matrix"} if possible.
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
#' # Results that are further simplified to a "uts_matrix"
#' sapply(ex_uts_matrix(), length)
#' sapply(ex_uts_matrix(), sqrt)
sapply.uts_vector <- function(X, ...)
{
  # Cannot call base::sapply(), because base::simplify2array() over-simplifies for certain output dimensions (see unit tests)
  out <- base::lapply(X, ...)
  if (length(out) == 0)
    return(out)

  # Simplify output only if no time series involved
  is_uts <- sapply(out, is.uts)
  is_uts_vector <- sapply(out, is.uts_vector)
  if (!any(is_uts) && !any(is_uts_vector))
    return(simplify2array(out))
  
  # Simpify output further, if possible
  if (all(is_uts))
    out <- do.call("c", out)
  out
}


#' @rdname sapply.uts_vector
sapply.uts_matrix <- function(X, ...)
{
  out <- NextMethod()
  
  # Return result if cannot simpified further
  if ((length(X) != length(out)) || (length(out) == 0))
    return(out)
  is_uts <- sapply(out, is.uts)
  is_base_type <- sapply(out, function(x) is.atomic(x) && !is.object(x))
  if (!all(is_uts) && !all(is_base_type))
    return(out)
  
  # If possible, simplify output to matrix or uts_matrix
  attributes(out) <- attributes(X)
  if (!all(is_uts))  # coerce from uts_matrix to matrix
    class(out) <- "matrix"
  out
}

