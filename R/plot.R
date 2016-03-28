#################################################
# Plotting methods for multivariate time series #
#################################################

#' Plot a uts_vector
#' 
#' Plot the individual time series of a \code{"uts_vector"} using \code{\link[uts]{plot.uts}}.
#' 
#' @param x a \code{"uts_vector"} object with numeric or logical observation values.
#' @param ask logical. If \code{TRUE} (and the \R session is interactive) the user is asked for input, before a new figure is drawn.
#' @param \dots arguments passed to \code{\link[uts]{plot.uts}}.
#' 
#' @examples
#' # plot all time series at once
#' old <- par(mfrow=c(1, 2))
#' plot(ex_uts_vector())
#' plot(ex_uts_vector(), max_dt=dhours(12), type="b")   # don't connect points more than 12 hours apart
#' par(old)
#' 
#' # plot one time series at a time
#' plot(ex_uts_vector(), ask=TRUE)
plot.uts_vector <- function(x, ..., ask=getOption("device.ask.default"))
{
  # Remove time series with zero observations
  len <- sapply(x, length)
  x <- x[len > 0]
  if (length(x) == 0)
    stop("Nothing to plot")
  
  # Optionally, ask for user input before plotting each time series
  if (ask && interactive()) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  
  # Plot all time series
  for (x_j in x)
     plot(x_j, ...)
}

