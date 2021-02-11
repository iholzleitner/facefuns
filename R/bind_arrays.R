#' Bind two arrays
#'
#' @param array1 Three-dimensional array
#' @param array2 Three-dimensional array
#' @param message Give warning if arrays are named inconsistently
#'
#' @return Returns new array
#' @export
#'
bind_arrays <- function (array1, array2, message = TRUE) {

  if(dim(array1)[[1]] != dim(array2)[[1]] ||
     dim(array1)[[2]] != dim(array2)[[2]]){
    stop("Your arrays can't be bound: they are not of the same dimensions")
  }

  d1 <- dim(array1)[[1]]
  d2 <- dim(array1)[[2]]
  d3 <- dim(array1)[[3]] + dim(array2)[[3]]
  new_dim <- c(d1, d2, d3)

  if (message == TRUE &&
      !all(dimnames(array1)[[1]] == dimnames(array2)[[1]]) ||
      !all(dimnames(array1)[[2]] == dimnames(array2)[[2]])) {
    warning("Your arrays' dimensions are named differently. Names of array1 have been used.")}

  d1.names <- dimnames(array1)[[1]]
  d2.names <- dimnames(array1)[[2]]
  d3.names <- c(dimnames(array1)[[3]], dimnames(array2)[[3]])

  new_dimnames <- list(d1.names, d2.names, d3.names)

  new_array <- array(data = numeric(),
                     dim = new_dim,
                     dimnames = new_dimnames)

  new_array[,, 1:dim(array1)[[3]]] <- array1
  new_array[,, (dim(array1)[[3]]+1):(dim(array1)[[3]]+dim(array2)[[3]])] <- array2

  return(new_array)
}
