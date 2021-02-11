#' Calculate averageness
#'
#' Functions calculates averageness/distinctiveness as each template's distance from sample average
#'
#' @param data Facefuns object or three-dimensional array of dimensions p, k (2 or 3), and n (minimum = 2)
#'
#' @return Returns tibble with distinctiveness and averageness (reversed distinctiveness) scores
#' @export
#' @examples
#' data(LondonSet_aligned)
#' data(mirroredlandmarks)
#' calc_avg(LondonSet_aligned)
calc_avg <- function (data) {

  if (class(data)[1] == "facefuns_obj") {

    old_array <- data$array
    average <- data$average

  } else {
    if (!is_shape_array(data)) {stop("Your input is neither a facefuns object nor a three-dimensional array containing 2-D or 3-D landmarks")}
    if (!dim(data)[[3]] > 1) {stop("Your input must contain more than one specimen")}

    old_array <- data
    average <- geomorph::mshape(data)

  }


  # SET UP -----
  # ... very clunky: all we do is add average to existing array
  new_dim <- c(dim(old_array)[[1]],
               dim(old_array)[[2]],
               dim(old_array)[[3]]+1)

  new_dimnames <- list(dimnames(old_array)[[1]],
                       dimnames(old_array)[[2]],
                       c("average", dimnames(old_array)[[3]]))

  new_array <- array(data = numeric(),
                     dim = new_dim,
                     dimnames = new_dimnames)

  new_array[,, 1] <- average
  new_array[,, 2:dim(new_array)[[3]]] <- old_array

  # CONVERT TO MATRIX ----
  data_matrix <- convert_array_to_matrix(new_array)

  # CREATE PAIRS_TABLE ----
  pairs_table <- data.frame(
    id = dimnames(old_array)[[3]],
    average = rep("average", dim(old_array)[[3]])
  )

  # calc_ed to get distinctiveness/averageness scores ----
  dist_scores  <- calc_ed(data_matrix, pairs_table) %>%
    dplyr::select(-2) %>%
    dplyr::rename("id" = 1,
                  "dist" = "EuclideanDistance") %>%
    dplyr::mutate(avg = (.01 + max(.$dist)) - .$dist)

  return(dist_scores)

}
