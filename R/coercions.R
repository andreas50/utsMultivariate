######################################################################
# Convert objects back and forth between other R time series classes #
######################################################################

############################
# Coercion to "uts_vector" #
############################

#' Coercion to uts_vector
#' 
#' Convert univariate and multivariate time series objects from other R package to \code{"uts_vector"} objects.
#'
#' @return An object of class \code{"uts_vector"}.
#' @param x a time series object of appropriate type.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' # Convert a bivariate "ts"
#' ts1 <- ts(matrix(1:20, 10, 2), start=c(2016, 1), frequency=12, names=c("apples", "oranges"))
#' utsv <- as.uts_vector(ts1)
#' utsv[[1]]
#' utsv$oranges
#' 
#' # Convert a 3-dimensional "zoo"
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   zoo1 <- zoo::zoo(matrix(1:12, 4, 3), as.Date("2003-01-01") + 0:3)
#'   utsv <- as.uts_vector(zoo1)
#'   utsv[[1]]
#' }
#' 
#' @seealso \code{\link[uts:as.uts]{as.uts}} (in package \code{uts}) for converting just univariate time series.
as.uts_vector <- function(x, ...) UseMethod("as.uts_vector")


#' @describeIn as.uts_vector convert a \code{\link[stats:ts]{ts}} object
as.uts_vector.ts <- function(x, ...)
{
  # Extract values and times
  times <- date_decimal(as.numeric(time(x)), tz="")
  values <- as.matrix(x)
  
  # Round times for monthly and quarterly frequency
  freq <- tsp(x)[3]
  if (freq %in% c(4, 12))
    times <- floor_date(times + days(5), unit="month")
  uts_vector_wide(values, times)
}


#' @describeIn as.uts_vector convert a \code{\link[zoo:zoo]{zoo}} object
as.uts_vector.zoo <- function(x, ...)
{
  times <- as.POSIXct(as.character(attr(x, "index")))
  values <- as.matrix(x)
  colnames(values) <- colnames(x)
  
  uts_vector_wide(x, times)
}



