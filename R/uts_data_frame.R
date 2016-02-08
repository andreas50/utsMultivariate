########################
# UTS_DATA_FRAME class #
########################

#' Unevenly-spaced Time Series Data.Frame
#' 
#' Create a data.frame of unevenly spaced time series (\code{"uts_data_frame"}).
#' 
#' Compared to \code{\link{data.frame}} in base \R, this function makes fewer guesses about how to coerce and recycle the data, and instead throws an error.
#' 
#' @note A pure virtual (abstract) class \code{"uts_virtual"} exists from which \code{"uts"}, \code{"uts_vector"}, \code{"uts_matrix"}, and \code{"uts_data_frame"} inherit: it is used to allow operations such as subtraction to mix the classes.
#'
#' @return An object of class \code{"uts_data_frame"}.
#' @param \dots these arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#' @param row.names a character or integer vector. The row names to use.
#' 
#' @keywords ts classes
#' @examples
#' uts_data_frame(a=ex_uts_vector(), b=ex_uts())
#' 
#' # The first test returns TRUE, the others return FALSE
#' is.uts_data_frame(uts_data_frame(a=ex_uts_vector()))
#' is.uts_data_frame(uts_matrix())
#' is.uts_data_frame(uts_vector())
#' is.uts_data_frame(uts())
uts_data_frame <- function(..., row.names=NULL)
{
}


#' @rdname uts_data_frame
#' 
#' @description \code{is.uts_data_frame} returns \code{TRUE} if its argument is a \code{"uts_data_frame"} object.
#' 
#' @param x an \R object.
#' 
#' @keywords internal
is.uts_data_frame <- function(x)
{
  inherits(x, "uts_data_frame")
}
