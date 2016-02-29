##################################
# Print methods for uts* classes #
##################################

#' Print Multivariate Time Series
#' 
#' Print basic information (the number of data points, and the first and last observation time) about the individual \code{"uts"} in a multivariate time series.
#' 
#' @param x an object used to select a method.
#' @param print_times boolean. For two-dimensional time series objects, whether to print the start and end times of the individual time series.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
#' @seealso \code{\link[base:print]{print}} in base \R.
#' @examples
#' # Print "uts_vector"
#' uts_vector()
#' print(uts_vector(a=ex_uts(), b=ex_uts2()))
#' 
#' # Print "uts_matrix"
#' ex_uts_matrix()
#' print(ex_uts_matrix(), print_times=TRUE)
print.uts_vector <- function(x, ...)
{
  # Special case of zero length
  num_uts <- length(x)
  if (num_uts == 0) {
    cat("uts_vector(0)\n")
    return(invisible(x))
  }
  
  # Determine start and end times
  start <- as.character(start(x), usetz=TRUE)
  end <- as.character(end(x), usetz=TRUE)

  # Extract time series stats
  Name <- names(x)
  if (length(Name) < num_uts)
    Name <- rep(NA, num_uts)
  #stats <- data.frame(Name, Datapoints=lengths(x), start, end)   # requires R (>= 3.2.0)
  stats <- data.frame(Name, Datapoints=sapply(x, length), start, end)
  rownames(stats) <- 1:num_uts

  # Print nice description
  print(stats)
  invisible(x)
}


#' @rdname print.uts_vector
print.uts_matrix <- function(x, print_times=FALSE, ...)
{
  # Special case of 0x0 dimension
  if ((nrow(x) == 0) && (ncol(x) == 0)) {
    cat("<0 x 0 uts_matrix>\n")
    return(invisible(x))
  }  
  
  # Determine time series stats
  start <- as.character(start(x), usetz=TRUE)
  end <- as.character(end(x), usetz=TRUE)
  data_points <- sapply(x, length)
  
  # Print nice description in matrix form
  if (print_times)
    des_uts <- paste(data_points, start, end, sep=", ")
  else
    des_uts <- data_points
  out <- matrix(paste0("uts[", des_uts, "]"), nrow(x), ncol(x))
  dimnames(out) <- dimnames(x)
  print(out, quote=FALSE)
  invisible(x)
}

