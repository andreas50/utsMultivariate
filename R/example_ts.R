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
    oranges = head(lag_t(ex_uts() * 1.1, dhours(1.5)), 5L)
  )
}


#' @rdname ex_uts_vector
#' 
#' @return \code{ex_uts_vector2()} returns a non-numeric \code{"uts_vector"} with two time series.
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


#' @rdname ex_uts_vector
#' 
#' @return \code{ex_uts_matrix()} returns a two-by-two numeric \code{"uts_matrixr"}.
#' 
#' @examples
#' ex_uts_matrix()
ex_uts_matrix <- function()
{
  utsv <- c(
    ex_uts_vector(),
    sapply(ex_uts_vector() ^ 1.1, lag_t, dminutes(30))
  )
  
  out <- uts_matrix(utsv, 2L, 2L, dimnames=list(c("apples", "oranges"), c("red", "green")))
  out
}

