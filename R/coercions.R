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
#' # Convert a 2-dimensional "fts" and remove NA observations
#' if (requireNamespace("fts", quietly = TRUE)) {
#'   x <- fts::fts(index=seq(from=Sys.Date(), by="months", length.out=24),data=1:24)
#'   y <- fts::fts(index=seq(from=Sys.Date(), by="months", length.out=12),data=13:24)
#'   fts1 <- cbind(x, y)
#'   na.omit(as.uts_vector(fts1))
#' }
#' 
#' # Convert a 3-dimensional "xts"
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   xts1 <- xts::xts(matrix(1:12, 4, 3), as.Date("2003-01-01") + 0:3)
#'   as.uts_vector(xts1)
#' }
#' 
#' # Convert a 3-dimensional "zoo"
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   zoo1 <- zoo::zoo(matrix(1:12, 4, 3), as.Date("2003-01-01") + 0:3)
#'   utsv <- as.uts_vector(zoo1)
#'   utsv[[1]]
#' }
#' 
#' @seealso \code{\link[uts]{as.uts}} for converting just univariate time series.
as.uts_vector <- function(x, ...) UseMethod("as.uts_vector")


#' @describeIn as.uts_vector convert a \code{\link{ts}} object
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


#' @describeIn as.uts_vector convert an \code{\link[fts]{fts}} object
as.uts_vector.fts <- function(x, ...)
{
  as.uts_vector.zoo(x, ...)
}


#' @describeIn as.uts_vector convert a \code{\link[xts]{xts}} object
as.uts_vector.xts <- function(x, ...)
{
  times <- as.POSIXct(time(x))
  values <- as.matrix(x)
  colnames(values) <- colnames(x)
  
  uts_vector_wide(x, times)
}


#' @describeIn as.uts_vector convert a \code{\link[zoo]{zoo}} object
as.uts_vector.zoo <- function(x, ...)
{
  times <- as.POSIXct(as.character(attr(x, "index")))
  values <- as.matrix(x)
  colnames(values) <- colnames(x)
  
  uts_vector_wide(x, times)
}



##############################
# Coercion from "uts_vector" #
##############################

#' Coercion to zoo
#' 
#' @return A \code{\link[zoo]{zoo}} object.
#' @param x a \code{"uts_vector"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   # Bivariate uts_vector with synchronized observation times
#'   utsv <- c(a=ex_uts(), b=ex_uts() + 3)
#'   zoo::as.zoo(utsv)
#'   
#'   # # Bivariate uts_vector with non-synchronized observation times
#'   zoo::as.zoo(ex_uts_vector())
#' }
as.zoo.uts_vector <- function(x, ...)
{
  if (!requireNamespace("zoo", quietly=TRUE))
    stop("Package 'zoo' needed for this function to work")
  
  data <- as.data.frame(x)
  zoo::zoo(data[, -1], data$time)
}


#' Coercion to xts
#' 
#' @return A \code{\link[xts]{xts}} object.
#' @param x a \code{"uts_vector"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' if (requireNamespace("xts", quietly = TRUE)) {
#'   xts::as.xts(ex_uts_vector())
#' }
as.xts.uts_vector <- function(x, ...)
{
  if (!requireNamespace("xts", quietly=TRUE))
    stop("Package 'xts' needed for this function to work")
  
  data <- as.data.frame(x)
  xts::xts(data[, -1], data$time)
}


#' Coercion to fts
#' 
#' @return An \code{\link[fts]{fts}} object.
#' @param x a \code{"uts_vector"} object.
#' 
#' @examples
#' if (requireNamespace("fts", quietly = TRUE)) {
#'   fts::as.fts(ex_uts_vector())
#' }
as.fts.uts_vector <- function(x)
{
  if (!requireNamespace("fts", quietly=TRUE))
    stop("Package 'fts' needed for this function to work")
  
  data <- as.data.frame(x)
  fts::fts(data$time, data[, -1])
}

