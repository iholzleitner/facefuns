#' Helper function to check if input is array of right dimensions for shape analyses
#'
#' @param x
#'
#' @return TRUE or FALSE
#' @keywords internal
#'
is_shape_array <- function(x) {
  # Anything else this should be checking for?
  if (is.array(x) &&
      length(dim(x)) == 3 &&
      (dim(x)[[2]] == 2 | dim(x)[[2]] == 3)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
