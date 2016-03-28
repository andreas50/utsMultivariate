#################################################
# Plotting methods for multivariate time series #
#################################################

#' Plot a uts_vector
#' 
#' Plot the individual time series of a \code{"uts_vector"} using \code{\link[uts]{plot.uts}}.
#' 
#' @param x a \code{"uts_vector"} object with numeric or logical observation values.
#' @param plot.type \code{"multiple"} plots the time eries on multiple plots, while \code{"single"} superimposes them on a single plot
#' @param ask logical. If \code{TRUE} (and the \R session is interactive) the user is asked for input, before a new figure is drawn.
#' @param legend boolean. For \code{plot.type="single"}, whether to add a \code{\link{legend}} to the plot.
#' @param legend.x,legend.y the x and y co-ordinates to be used to position the legend. See \code{\link{legend}}.
#' @param \dots arguments passed to \code{\link[uts]{plot.uts}}.
#' 
#' @seealso
#' @examples
#' # plot all time series at once
#' old <- par(mfrow=c(1, 2))
#' plot(ex_uts_vector())
#' plot(ex_uts_vector(), max_dt=dhours(12), type="b")   # don't connect points more than 12 hours apart
#' par(old)
#' 
#' # plot one time series at a time
#' plot(ex_uts_vector(), ask=TRUE)
#' 
#' # plot multiple time series in a single plot by superimposing the plots
#' plot(ex_uts_vector(), plot.type="single")
plot.uts_vector <- function(x, ..., plot.type="multiple", ask=getOption("device.ask.default"),
  legend=TRUE, legend.x="topright", legend.y=NULL)
{
  # Argument checking
  if (!(plot.type %in% c("multiple", "single")))
    stop("Unknown 'plot.type'")
  
  # Remove time series with zero observations
  len <- sapply(x, length)
  x <- x[len > 0]
  if (length(x) == 0)
    stop("Nothing to plot")
  
  # Call helper function for super-imposed plots
  if ( plot.type == "single") {
    plot_single_uts_vector(x, ..., legend=legend, legend.x=legend.x, legend.y=legend.y)
    return(invisible())
  }
  
  # Optionally, ask for user input before plotting each time series
  if (ask && interactive()) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  
  # Plot all time series
  for (x_j in x)
    plot(x_j, ...)
}


#' Plot a uts_vector in a single plot
#' 
#' A helper function that implements \code{plot.uts_vector} for argument \code{plot.type="single"}.
#' 
#' @param x a \code{"uts_vector"} object with numeric or logical observation values.
#' @param \dots arguments passed to \code{\link[uts]{plot.uts}}.
#' @param legend boolean. Whether to add a legend to the plot.
#' @param legend.x,legend.y the x and y co-ordinates to be used to position the legend.
#' 
#' @seealso \code{\link{matplot}}
#' @examples
#' plot_single_uts_vector(ex_uts_vector())
plot_single_uts_vector <- function(x, ..., legend=TRUE, legend.x="topright", legend.y=NULL)
{
  
}

