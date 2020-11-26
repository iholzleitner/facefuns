#' Calculate vector score
#'
#' Function projects data onto vector from anchor one (score of 0) to anchor two (score of 1). For all three arguments, input must be two-dimensional matrix with rows = specimen, and columns = PC scores.
#'
#' @param data PC scores of face(s) for which vector scores are to be calculated
#' @param anchor1 PC scores of face(s) which will constitute lower anchor point
#' @param anchor2 PC scores of face(s) which will constitute upper anchor point
#'
#' @return Returns tibble with columns "id" and "vectorScore". If data contained rownames, these will be saved as ids.
#' @export
#'
calcVS <- function(data, anchor1, anchor2){

  # check if data is matrix
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  if (!is.matrix(anchor1)) {
    anchor1 <- as.matrix(anchor1)
  }
  if (!is.matrix(anchor2)) {
    anchor2 <- as.matrix(anchor2)
  }

  # check if all three files have the same number of columns
  if (!(ncol(data) == ncol(anchor1) && ncol(data) == ncol(anchor2))) {
    stop("All three input files must have the same number of PCs/columns")
  }

  # Calculate (col)means of anchors
  anchor1_m <- t(apply(anchor1, 2, mean))
  anchor2_m <- t(apply(anchor2, 2, mean))

  # Calculate vector and normalize it
  vec <- anchor2_m - anchor1_m
  norm_dist <- sqrt(sum(vec ^ 2))
  norm_vec <- vec / norm_dist

  # Function to compute vector score for each row of input matrix
  vec_score <- function(matrix) {
    z1 <- matrix - anchor1_m
    z2 <- z1 / norm_dist
    z3 <- z2 * norm_vec
    sum(z3)
  }

  # Apply function to each row of data matrix
  vec_scores <- apply(data, 1, vec_score)

  # Get ids. If there are none because input did not have rownames, make some.
  if(is.null(names(vec_scores))) {
    id <- seq(1:length(vec_scores))
  } else {
    id <- names(vec_scores)
  }

  # Save output as tibble
  vs <- tibble::tibble(
    id = id,
    # maybe remove unname again? but it felt untidy to have named vector in tibble
    vectorScore = unname(vec_scores)
  )

  return(vs)

}
