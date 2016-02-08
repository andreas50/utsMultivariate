##########################
# Wide/Long constructors #
##########################

##############
# uts_vector #
##############

#' Create uts_vector from wide tabular data
#' 
#' Create a \code{"uts_vector"} from \emph{wide} tabular data (see \href{https://en.wikipedia.org/wiki/Wide_and_narrow_data}{Wikipedia}). For data in this format, values in the same row are measurements at the same time point, either across multiple entities (e.g. persons, countries) or of multiple attributes (e.g. several economic indicators, blood work results). Values in the same column are measurements of the same entity or attribute over time.
#' 
#' @return An object of class \code{"uts_vector"}. The number of time series is equal to the number of columns of \code{values}. The length of each time series is equal to the number of rows of \code{values}.
#' @param values a matrix or data.frame. Each row is a vector of observations at a specific time point.
#' @param times a \code{\link{POSIXct}} object. The observation times of the rows of \code{values}.
#' @param names a character vector. The entity/attribute names of the columns of \code{values}. By default, the column names of \code{values} are used.
#' 
#' @keywords ts classes
#' @seealso \code{\link{uts_vector_long}}
#' @examples 
#' values <- data.frame(apples=1:10, oranges=letters[1:10],
#'   bananas=month.name[1:10], stringsAsFactors=FALSE)
#' uts_vector_wide(values, times=as.POSIXct("2015-01-01") + ddays(1:10))
uts_vector_wide <- function(values, times, names=colnames(values))
{
  # Argument checking
  if (!is.matrix(values) && !is.data.frame(values))
    stop("The data is not in matrix or data.frame format")
  if (nrow(values) != length(times))
    stop("The number of sources values does not match the number of observation times")
  if (!is.null(names) && (length(names) != ncol(values)))
    stop("The number of observation entities does not match the number of source names")
  if (!is.POSIXct(times))
    stop("The observation time vector is not a POSIXct object")
  
  # Order data chronologically
  o <- order(times)
  times <- times[o]
  values <- values[o,,drop=FALSE]

  # Generate "uts_vector"
  out <- uts_vector()
  for (j in seq_len(ncol(values)))
    out[[j]] <- uts(values[, j], times)
  names(out) <- names
  out
}


#' Create uts_vector from long tabular data
#' 
#' Create a \code{"uts_vector"} from \emph{long} (also known as \emph{narrow}) tabular data. Data in this format has three different columns; the observation values, the observation times, and the entity/attribute name (e.g. person, country, economic indicator) of each observation.
#' 
#' @return An object of class \code{"uts_vector"} with length given by to the number of distinct sources.
#' @param values a vector observation values.
#' @param times a \code{\link{POSIXct}} object. The matching observation times.
#' @param names a character vector. The matching entity/attribute names of the observations. By default, the names of \code{values} are used.
#' 
#' @keywords ts classes
#' @seealso \code{\link{uts_vector_wide}}
#' @examples 
#' values <- c(a=5, b=6, a=6, a=7)
#' times <- as.POSIXct("2016-01-01") + dhours(1:4)
#' uts_vector_long(values, times)
#' 
#' uts_vector_long(values=1:10, times=as.POSIXct("2016-01-01") + days(1:10),
#'   names=c("a", "a", "a", "a", "a", "c", "c", "b", "b", "b"))
uts_vector_long <- function(values, times, names=base::names(values))
{
  # Argument checking
  if (length(values) != length(times))
    stop("The number of observation values does not match the number of observation times")
  if (!is.POSIXct(times))
    stop("The observation time vector is not a POSIXct object")
  if (length(values) != length(names))
    stop("The length of the source names does not match the number of observation values")
  
  # Order data chronologically
  o <- order(times)
  times <- times[o]
  values <- values[o] 
  names <- names[o]
  
  # Determine list of indices for each unique name
  indices <- split(seq_along(names), names)

  # Insert data
  out <- uts_vector()
  for (j in seq_along(indices)) {
    # Insert UTS for j-th name
    pos <- indices[[j]]
    out[[j]] <- uts(values[pos], times[pos])
  }
  if (length(out) > 0)
    names(out) <- names(indices)
  out  
}



##############
# uts_matrix #
##############

#' Create uts_matrix from long tabular data
#' 
#' Create a \code{"uts_matrix"} from \emph{long} (also known as \emph{narrow}) tabular data. Data in this format has four different columns; the observation values, the observation times, the entity name of each observation (e.g. person, country), and the attribute/field name of each observation (e.g. which one of serveral economic indicators, which one of several blood measurement values).
#' 
#' @return An object of class \code{"uts_matrix"}. The time series in row \code{entity_name} and column \code{field_name} contains all observations of such entity for such field.
#' @return The number of rows is given by to the number of distinct entity names (parameter \code{names}), while the number of columns is given by the number of distinct attribute/field names (parameter \code{fields}).
#' @param values a vector observation values.
#' @param times a \code{\link{POSIXct}} object. The matching observation times.
#' @param names a character vector. The the matching entity names for the observations. By default, the names of \code{values} are used.
#' @param fields a character vector. The the matching attribute/field names for the observations.
#' 
#' @seealso \code{\link{uts_matrix_wide}}
#' @keywords ts classes
#' @examples
#' values <- c(A=1, A=2, B=3, B=4, A=5)
#' times <- as.POSIXct("2016-01-01") + dhours(1:5)
#' uts_matrix_long(values, times, fields=c("c", "d", "d", "d", "d"))
#' 
#' 
#' uts_matrix_long(values=1:5, times=as.POSIXct("2015-01-01") + days(1:5),
#'   names=c("A", "A", "B", "B", "A"), fields=c("c", "d", "d", "d", "d"))
uts_matrix_long <- function(values, times, names=base::names(values), fields)
{
  # Argument checking
  if (length(values) != length(times))
    stop("The number of observation values does not match the number of observation times")
  if (length(values) != length(names))
    stop("The number of observation values does not match the number of entity names")
  if (length(values) != length(fields))
    stop("The number of observation values does not match the number of attribute/field names")
  if (!is.POSIXct(times))
    stop("The observation time vector is not a POSIXct object")
  
  # Special case of zero observations
  if (length(values) == 0)
    return(uts_matrix(nrow=0, ncol=0))
  
  # Order data chronologically
  o <- order(times)
  times <- times[o]
  values <- values[o] 
  names <- names[o]
  fields <- fields[o]
  
  # Allocate memory for output
  rnames <- sort(unique(names))
  cnames <- sort(unique(fields))
  nrows <- length(rnames)
  ncols <- length(cnames)
  out <- uts_matrix(uts(), ncol=ncols, nrow=nrows, dimnames=list(rnames, cnames))
  
  # Determine values and times for each (row,column) pair
  keys <- paste(rep(rnames, ncols), rep(cnames, each=nrows), sep="$$$")
  all_keys <- paste(names, fields, sep="$$$")
  pos <- match(all_keys, keys)
  values_ij <- split(values, pos)
  times_ij <- split(times, pos)

  # Insert individual UTS
  index <- as.numeric(names(values_ij))
  for (j in seq_along(index))
    out[[index[j]]] <- uts(values_ij[[j]], times_ij[[j]])
  out
}


#' Create uts_matrix from wide tabular data
#' 
#' Create a \code{"uts_matrix"} from \emph{wide} tabular data (see \href{https://en.wikipedia.org/wiki/Wide_and_narrow_data}{Wikipedia}). For data in this format, each row is a vector of observations (also known as a \emph{record}) for a specific entity (e.g. person, country) at a specific time point. The record consists of measurements across multiple attributes/fields (e.g. several economic indicators, several blood measurement values).
#' 
#' @return An object of class \code{"uts_matrix"}. The time series in row \code{entity_name} and column \code{field_name} contains all observations of such entity for such field.
#' @return The number of rows is given by to the number of distinct entity names (parameter \code{names}), while the number of columns is given by the number of attributes/fields, i.e. the  number of columns of \code{values}.
#' @param values a matrix or data.frame of observation values.
#' @param times a \code{\link{POSIXct}} object. The matching observation times for the records (i.e. rows of \code{values}).
#' @param names a character vector. The the matching entity names for the records. By default, the row names of \code{values} are used.
#' @param fields a character vector. The the attribute/field names of the records. By default, the column names of \code{values} are used.
#' 
#' @seealso \code{\link{uts_matrix_long}}
#' @keywords ts classes
#' @examples 
#' values <- matrix(1:8, 4, 2)
#' rownames(values) <- c("CH", "CH", "FR", "US")
#' colnames(values) <- c("population", "size")
#' times <- as.POSIXct("2016-01-01") + days(1:4)
#' uts_matrix_wide(values, times)
#' 
#' # Same, but manually provide entity names
#' uts_matrix_wide(values, times, names=c("China", "China", "France", "USA"))
uts_matrix_wide <- function(values, times, names=base::rownames(values), fields=base::colnames(values))
{
  # Argument checking
  if (!is.matrix(values) && !is.data.frame(values))
    stop("The observation values are not stored in a matrix or data.frame")
  if (nrow(values) != length(times))
    stop("The number of observation rows does not match the number of observation times")
  if (nrow(values) != length(names))
    stop("The number of observation rows does not match the number of entity names")
  if (ncol(values) != length(fields))
    stop("The number of observation columns does not match the number of attribute/field names")
  if (!is.POSIXct(times))
    stop("The observation time vector is not a POSIXct object")
  
  # Allocate memory for output
  if (length(names) > 0)
    rnames <- sort(unique(names))
  else
    rnames <- NULL
  if (length(fields) > 0)
    cnames <- sort(unique(fields))
  else
    cnames <- NULL
  nrows <- length(rnames)
  ncols <- length(cnames)
  out <- uts_matrix(uts(), ncol=ncols, nrow=nrows, dimnames=list(rnames, cnames))

  # Insert data one column at a time
  for (col in seq_len(ncols)) {
    out_col <- uts_vector_long(values[, col], times, names=names)
    for (row in seq_len(nrows))
      out[[row + (col-1)*nrows]] <- out_col[[row]]
  }
  out
}



##################
# uts_data_frame #
##################

#' Create uts_data_frame from long tabular data
#' 
#' Create a \code{"uts_data_frame"} from \emph{long} (also known as \emph{narrow}) tabular data. Data in this format has four different columns; the observation values, the observation times, the entity name of each observation (e.g. person, country), and the attribute/field name of each observation (e.g. which one of serveral economic indicators, which one of several blood measurement values).
#' 
#' @return An object of class \code{"uts_data_frame"}. The time series in row \code{entity_name} and column \code{field_name} contains all observations of such entity for such field.
#' @return The number of rows is given by to the number of distinct entity names (parameter \code{names}), while the number of columns is given by the number of distinct attribute/field names (parameter \code{fields}).
#' @param values a vector observation values.
#' @param times a \code{\link{POSIXct}} object. The matching observation times.
#' @param names a character vector. The the matching entity names for the observations. By default, the names of \code{values} are used.
#' @param fields a character vector. The the matching attribute/field names for the observations.
#' 
#' @seealso \code{\link{uts_matrix_long}}
#' @keywords ts classes
#' @examples
#' values <- c(A=1, A=2, B=3, B=4, A=5)
#' times <- as.POSIXct("2016-01-01") + dhours(1:5)
#' uts_data_frame_long(values, times, fields=c("c", "d", "d", "d", "d"))
#' 
#' 
#' uts_data_frame_long(values=1:5, times=as.POSIXct("2015-01-01") + days(1:5),
#'   names=c("A", "A", "B", "B", "A"), fields=c("c", "d", "d", "d", "d"))
uts_data_frame_long <- function(values, times, names=base::names(values), fields)
{
  
}

