####################################
# Example multivariate time series #
####################################

#' Example Multivariate Time Series
#' 
#' Create time series vectors that can be used for code examples and testing.
#' 
#' @return \code{ex_uts_vector()} returns a numeric \code{"uts_vector"} with two time series.
#' 
#' @examples
#' ex_uts_vector()
ex_uts_vector <- function()
{
  c(
    apples = ex_uts(),
    oranges = lag_t(ex_uts() * 1.1, dhours(1.5))
  )
}


#' @rdname ex_uts_vector
#' 
#' @return \code{ex_uts2()} returns a non-numeric \code{"uts_vector"} with two time series.
#' 
#' @examples
#' ex_uts_vector2()
ex_uts_vector2 <- function()
{
  c(
    cats = ex_uts2(),
    dogs = ex_uts()
  )
}
