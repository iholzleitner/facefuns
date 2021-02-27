#' Bind arrays
#'
#' Binds two or more p x k matrices or p x k x n arrays into one three-dimensional array. Dimension names are set by those of first array/matrix entered
#'
#' @param ... Matrices/arrays to be bound, separated by commas
#'
#' @return Returns new array
#' @export
#'
#' @note See \link[Morpho]{bindArr} for more flexible function that allows to bind along different dimensions
#'
#' @examples
#'
#' data(londonSet_aligned)
#'
#' new_array <- bind_arrays(LondonSet_aligned, LondonSet_aligned)
#'

bind_arrays <- function(...) {

  args <- list(...)
  len <- length(args)

  if (len < 2){
    stop("You submitted only one element")
  } else {
    new_array <- bind_array_helper(args[[1]], args[[2]])
  }

  if (len > 2) {
    for (i in 3:len) {
      new_array <- bind_array_helper(new_array, args[[i]])
    }
  }

  return(new_array)

}

#' Bind array helper
#'
#' @param x Matrix or array 1
#' @param y Matrix or array 2
#'
#' @return Returns three-dimensional array containing elements x and y
#' @keywords internal
#'
bind_array_helper <- function(x, y) {

  # CONVERT MATRICES TO ARRAY
  if (is.matrix(x)) {
    x <- array(x, dim = c(dim(x), 1))
    xdim <- dim(x)
  } else if (is.array(x)) {
    xdim <- dim(x)
  } else {
    stop("X is neither a matrix nor an array")
  }

  if (is.matrix(y)) {
    y <- array(y, dim = c(dim(y), 1))
    ydim <- dim(y)
  } else if (is.array(y)) {
    ydim <- dim(y)
  } else {
    stop("Y is neither a matrix nor an array")
  }

  # CHECK WHETHER DIMENSIONS ARE THE SAME
  if (!identical(xdim[1:2], ydim[1:2])) {
    stop("X and Y are of different dimensions")
  }

  d1 <- xdim[[1]]
  d2 <- xdim[[2]]
  d3 <- xdim[[3]] + ydim[[3]]
  new_dim <- c(d1, d2, d3)

  # CHECK DIMENSION NAMES
  xnames <- dimnames(x)
  ynames <- dimnames(y)

  if(!(identical(xnames[[1]], ynames[[1]]) &&
       identical(xnames[[2]], ynames[[2]]))){
    warning("X and Y's dimensions are named differently. Names of X have been used")
  }

  d1.names <- xnames[[1]]
  d2.names <- xnames[[2]]
  d3.names <- c(xnames[[3]], ynames[[3]])
  new_dimnames <- list(d1.names, d2.names, d3.names)

  # CREATE NEW ARRAY
  new_array <- array(NA,
                     dim = new_dim,
                     dimnames = new_dimnames)

  new_array[,, 1:xdim[[3]]] <- x
  new_array[,, (xdim[[3]]+1):new_dim[[3]]] <- y

  return(new_array)
}
