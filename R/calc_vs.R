#' Calculate vector score
#'
#' Function projects data onto vector from anchor one (score of 0) to anchor two (score of 1). For all three arguments, input must be two-dimensional data frame or matrix with rows = specimens, and columns = PC scores.
#'
#' @param data PC scores of face(s) for which vector scores are to be calculated
#' @param anchor1 PC scores of face(s) which will constitute lower anchor point
#' @param anchor2 PC scores of face(s) which will constitute upper anchor point
#'
#' @return Returns tibble with columns "id" and "VS". If data contained rownames, these will be saved as ids.
#' @export
#'
#' @examples
#' # CALCULATE FEMALE-MALE VECTOR SCORES
#' data("LondonSet_scores")
#' data("LondonSet_info")
#'
#' fem <- LondonSet_scores %>%
#'   dplyr::filter(row.names(LondonSet_scores) %in%
#'   LondonSet_info$face_id[which(LondonSet_info$face_sex == "female")])
#'
#' mal <- LondonSet_scores %>%
#'   dplyr::filter(row.names(LondonSet_scores) %in%
#'   LondonSet_info$face_id[which(LondonSet_info$face_sex == "male")])
#'
#' calc_vs(LondonSet_scores, fem, mal)
#'
calc_vs <- function(data, anchor1, anchor2){

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

  # ...
  vec_scores <- apply(data, 1, function(matrix) {
    z1 <- matrix - anchor1_m
    z2 <- z1 / norm_dist
    z3 <- z2 * norm_vec
    sum(z3)
  })

  # Get ids. If there are none because input did not have rownames, make some.
  if(is.null(names(vec_scores))) {
    id <- seq_along(vec_scores)
  } else {
    id <- names(vec_scores)
  }

  # Save output as tibble
  vs <- tibble::tibble(
    id = id,
    # maybe remove unname again? but it felt untidy to have named vector in tibble
    VS = unname(vec_scores)
  )

  return(vs)

}
