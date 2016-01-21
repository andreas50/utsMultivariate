##################################
# Print methods for uts* classes #
##################################

#' Print Values
#' 
#' Print basic information (the number of data points, and the first and last observation time) about the individual \code{"uts"} in a multivariate time series object.
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
#' utsm <- uts_matrix(ex_uts(), 2, 3)
#' utsm
#' print(utsm, print_times=TRUE)
#' 
#' # Print "uts_data_frame"
print.uts_vector <- function(x, ...)
{
  # Special case of empty "uts_vector"
  num_uts <- length(x)
  if (num_uts == 0) {
    cat("-----------------\n")
    cat("uts_vector object\n")
    cat("-----------------\n")
    cat("No time series available at this time\n")
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
  cat("\nIndividual time series characteristics:\n")
  cat("---------------------------------------\n")
  print(stats)
  invisible(x)
}


#' @rdname print.uts_vector
print.uts_matrix <- function(x, print_times=FALSE, ...)
{
    # Special case of empty "uts_matrix"
  num_uts <- length(x)
  if (num_uts == 0) {
    cat("-----------------\n")
    cat("uts_matrix object\n")
    cat("-----------------\n")
    cat("No time series available at this time\n")
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

