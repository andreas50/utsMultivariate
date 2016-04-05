#' Ops Group Methods for uts_vector
#' 
#' @keywords internal
#' @examples
#' # Unary operators
#' -ex_uts_vector()
#' !(ex_uts_vector() > 48)
#' 
#' # Binary operators
#' ex_uts_vector() + 20
#' ex_uts_vector() + ex_uts()
#' ex_uts_vector() + ex_uts_vector()
Ops_uts_vector <- function(e1, e2, .Generic)
{
  # Unary operator
  if (missing(e2)) {
    if (length(e1) > 0)
    	e1 <- sapply(e1, .Generic)
		return(e1)
  }

  # Binary operator
  if (is.uts_vector(e1) && is.uts_vector(e2)) {
    if (length(e1) != length(e2))
      stop("The two uts_vectors need to be of same length in order to apply the element-wise operator '",
        .Generic, "'", sep="")
    out <- e1
    for (j in 1:length(e1))
      out[[j]] <- do.call(.Generic, list(e1[[j]],  e2[[j]]))
  }  else if (is.uts_vector(e1)) {
    if (!is.uts(e2) && (length(e2) != 1))
      stop("Group methods 'Ops' between a 'uts_vector' and other objects work only for objects of length one")
    out <- e1
    for (j in 1:length(e1))
      out[[j]] <- do.call(.Generic, list(e1[[j]],  e2))
  } else {
    if (!is.uts(e1) && (length(e1) != 1))
      stop("Group methods 'Ops' between a 'uts_vector' and other objects work only for objects of length one")
    out <- e2
    for (j in 1:length(e2))
      out[[j]] <- do.call(.Generic, list(e1,  e2[[j]]))
  }
  out
}

