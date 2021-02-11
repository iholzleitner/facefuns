#' Convert 3D array into matrix
#'
#' @param x A three-dimensional array (p x k x n) of landmark coordinates
#'
#' @return Returns two-dimensional matrix with n rows and p x k columns. If input has labels, these will be preserved.
#' @export
#'
#' @examples
#' data("LondonSet_aligned")
#' data_matrix <- convert_array_to_matrix(LondonSet_aligned)
convert_array_to_matrix <- function(x){

  # TODO - use aperm() for changing dimension orders (so its xyz rather than xxxxxyyyyyzzzzz)

  # CHECK INPUT ----
  if (!is.array(x) || length(dim(x))!=3 || (dim(x)[2]<2 | dim(x)[2]>3)) {
    stop("Input is not an array or of the wrong dimensions")
  }

  # DEFINE DIMS ----
  p <- dim(x)[1]
  k <- dim(x)[2]
  n <- dim(x)[3]

  dimnames = list(dimnames(x)[[3]],
                  as.vector(outer(dimnames(x)[[2]], dimnames(x)[[1]], paste0)))

  # CONVERT ----
  temp <- matrix(data = x,
                 ncol = (p * k),
                 byrow = T)

  # reorder columns. wee bit clumsy, innit.
  x.coords <- temp[, 1:p]
  y.coords <- temp[, (p + 1):(p * 2)]

  if (k==2){
    data_matrix <- matrix(data = rbind(x.coords, y.coords),
                          nrow = n,
                          dimnames = dimnames)
  } else if (k==3) {
    z.coords <- temp[, (p*2 + 1):(p * 3)]
    data_matrix <- matrix(data = rbind(x.coords, y.coords, z.coords),
                          nrow = n,
                          dimnames = dimnames)
  }

  return(data_matrix)

}
