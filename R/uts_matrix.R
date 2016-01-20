####################
# UTS_MATRIXclass #
####################

#' Unevenly-spaced Time Series Matrix
#' 
#' Create a matrix of unevenly spaced time series (\code{"uts_matrix"}).
#' 
#' @note A virtual class \code{"uts_virtual"} exists from which \code{"uts"}, \code{"uts_vector"}, \code{"uts_matrix"}, and \code{"uts_data_frame"} inherit: it is used to allow operations such as subtraction to mix the classes.
#'
#' @return An object of class \code{"uts_matrix"}.
#' @param data a \code{\link{uts}} or \code{\link{uts_vector}}.
#' @param nrow the desired number of rows.
#' @param ncol the desired number of columns.
#' @param byrow logical. If FALSE (the default) the matrix is filled by columns, otherwise the matrix is filled by rows.
#' @param dimnames A \code{\link{dimnames}} attribute for the matrix: NULL or a list of length 2 giving the row and column names respectively.
#' 
#' @keywords ts classes
#' @examples
#' # Create using a single "uts" with recycling
#' uts_matrix(ex_uts(), 2, 3)
#' 
#' # Create using "uts_vector" with recycling
#' uts_matrix(ex_uts(), 2, 3)
#' uts_matrix(ex_uts(), 2, 3, byrow=TRUE)
#' 
#' # Empty "uts_matrix"
#' uts_matrix(nrow=2, ncol=3)
uts_matrix <- function(data=uts(), nrow=1, ncol=1, byrow=FALSE, dimnames=NULL)
{
  # Recycle data
  # -) NEXT: need rep.uts, rep.uts_vector
  
  
  
}