#' Ops Group Methods for uts_vector
#' 
#' 
#' @keywords internal
#' @examples
#' # Unary operators
#' -ex_uts()
#' !(ex_uts() > 48)
#' 
#' # Binary operators
#' ex_uts() + ex_uts()
#' ex_uts() + 20
#' 20 + ex_uts()
Ops_uts_vector <- function(x, x2, .Generic)
{
  # Unary operator
  if (missing(x2)) {
    if (length(x) > 0)
    	x <- sapply(x, .Generic)
		return(x)
  }
  
  # Binary operator
  if (is(x, "uts_vector") && is(x2, "uts_vector")) {
    # uts_vector Ops uts_vector
    if (length(x) != length(x2))
      stop("The two uts_vectors need to be of same length in order to apply the element-wise operator '",
        .Generic, "'", sep="")
    out <- x
    for (j in 1:length(x))
      out[[j]] <- do.call(.Generic, list(x[[j]],  x2[[j]]))
  }  else if (is.numeric(x2)) {
    # uts_vector Ops numeric
    out <- x
    for (j in 1:length(x))
      out[[j]] <- do.call(.Generic, list(x[[j]],  x2))
  } else {
    # numeric Ops uts_vector
    out <- x2
    for (j in 1:length(x2))
      out[[j]] <- do.call(.Generic, list(x,  x2[[j]]))
  }
  out
}



