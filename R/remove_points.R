#' Remove points/landmarks from 2-D templates
#'
#' @description
#' \lifecycle{experimental}
#'
#' Delete points/landmarks from array of 2-D landmark templates
#'
#' @param lmdata Array of dimensions p (number of landmarks) x k (dimensionality of data, only 2 allowed) x n (number of specimens)
#' @param points You can specify points as numeric vector. If your original templates are standard FRL templates (p=189), you can also specify features by their FRL names (see \link[facefuns]{frl_features})
#' @param relabel_points If false, old landmark labels will be retained
#' @param plot Plot new landmark templates
#' @param quiet Print how many points were deleted
#'
#' @return Array of new dimensions p (reduced number of landmarks) x k x n
#' @export
#'
#' @examples
#'
#' path_to_tem <- system.file("extdata", "tem", package="facefuns")
#'
#' data <- read_lmdata(lmdata = path_to_tem,
#'                     plot = TRUE)
#'
#' new_data <- remove_points(lmdata = data,
#'                           points = "frlgmm",
#'                           plot = TRUE)
#'
remove_points <- function(lmdata, points, relabel_points = TRUE, plot = TRUE, quiet = FALSE){

  # CHECK LMDATA ----
  if (!is_shape_array(lmdata) || (is_shape_array(lmdata) & dim(lmdata)[[2]] == 3)){
    stop("Deleting points failed. Your landmark data is of the wrong dimensions")
  }

  n <- dim(lmdata)[[3]]

  # CHECK POINTS ----
  if (is.numeric(points)){
    points_to_del <- points
  } else if (is.character(points)) {
    points_to_del <- frl_features(points) + 1
  }

  # DELETE POINTS ----
  new_lmdata <- sapply(seq_len(n),
                       function(x) lmdata[-points_to_del, , x],
                       simplify = "array")

  # PLOT NEW TEMPLATES ----
  if (plot) {
    geomorph::plotAllSpecimens(new_lmdata)
  }

  # PRINT MESSAGE ----
  if (quiet == FALSE){
    cat(paste0(length(points_to_del), " landmarks were deleted."))
  }

  # RELABEL POINTS ----
  if (relabel_points == TRUE){
    d1.names <- 1:dim(new_lmdata)[[1]]
  } else{
    d1.names <- dimnames(new_lmdata)[[1]]
  }

  # ASSIGN DIMNAMES ----
  d2.names <- dimnames(lmdata)[[2]]
  d3.names <- dimnames(lmdata)[[3]]
  new_dimnames <- list(d1.names, d2.names, d3.names)
  dimnames(new_lmdata) <- new_dimnames

  return(new_lmdata)

}
