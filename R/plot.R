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
#' @seealso \code{\link{plot.uts}}
#' @examples
#' # Plot all time series at once
#' old <- par(mfrow=c(1, 2))
#' plot(ex_uts_vector())
#' plot(ex_uts_vector(), max_dt=dhours(12), type="b")   # don't connect points more than 12 hours apart
#' par(old)
#' 
#' # Plot one time series at a time
#' plot(ex_uts_vector(), ask=TRUE)
#' 
#' # Plot multiple time series in a single plot by superimposing the plots
#' plot(ex_uts_vector(), plot.type="single")
plot.uts_vector <- function(x, plot.type="single", ..., ask=getOption("device.ask.default"),
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
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis
#' @param col,lty,lwd,pch,type graphical parameters. See \code{\link{plot.default}}.
#' @param legend boolean. Whether to add a legend to the plot.
#' @param legend.x,legend.y the x and y co-ordinates to be used to position the legend.
#' @param \dots arguments passed to \code{\link[uts]{plot.uts}}.
#' 
#' @seealso \code{\link{matplot}}
#' @keywords internal
#' @examples
#' plot_single_uts_vector(ex_uts_vector())
#' plot_single_uts_vector(ex_uts_vector(), type="o")
plot_single_uts_vector <- function(x, ..., xlab="", ylab="",
  col=seq_along(x), lty=1, lwd=1, pch=1, type="l",
  legend=TRUE, legend.x="topright", legend.y=NULL)
{
  # Remove time series with zero observations
  len <- sapply(x, length)
  if (0) {
    #x <- x[len > 0]    # not implemented yet
  } else {
    for (l in rev(len))
      x[[l]] <- NULL
  }
  num_ts <- length(x)
  if (num_ts == 0)
    stop("Nothing to plot")
  
  # Set up empty plotting canvas
  tmp_x <- c(min(start(x)), max(end(x)))
  tmp_y <- range(range(x))
  plot(tmp_x, tmp_y, type="n", xlab=xlab, ylab=ylab, ...)
  
  # Recycle arguments
  if (length(col) < num_ts) 
    col <- rep_len(col, num_ts)
  if (length(lty) < num_ts) 
    lty <- rep_len(lty, num_ts)
  if (length(lwd) < num_ts) 
    lwd <- rep_len(lwd, num_ts)
  if (length(pch) < num_ts) 
    pch <- rep_len(pch, num_ts)
  if (length(type) < num_ts) 
    type <- rep_len(type, num_ts)
  
  # Plot the individual time series
  for (j in seq_along(x))
    plot(x[[j]], plot.new=FALSE, col=col[j], lty=lty[j], lwd=lwd[j], pch=pch[j], type=type[j], ...)
  
  # Add legend
  if (legend) {
    # extract names
    names <- names(x)
    if (is.null(names))
      names <- as.character(1:length(uts_vector))
    
    # need to clean 'pch' and 'lty' paramters, because effect different for legend() than plot()
    legend.pch <- pch
    legend.pch[!(type %in% c("b", "o", "p"))] <- -1
    #
    legend.lty <- lty
    legend.lty[type %in% "p"] <- NA
    
    #browser()
    legend(legend.x, legend.y, legend=names, xjust=1, yjust=1, inset=c(0.01),
      col=col, lty=legend.lty, lwd=lwd, pch=legend.pch)
  }
}
