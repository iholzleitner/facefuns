#' Calculate averageness
#'
#' Functions calculates averageness/distinctiveness as each template's distance from sample average
#'
#' @param quickstart_obj Quickstart object
#'
#' @return Returns tibble with distinctivness and averageness (reversed distinctiveness) scores
#' @export
#'
calcAVG <- function (quickstart_obj) {

  # SET UP -----
  # ... very clunky
  old_array <- quickstart_obj$array

  new_dim <- c(dim(old_array)[[1]],
               dim(old_array)[[2]],
               dim(old_array)[[3]]+1)

  new_dimnames <- list(dimnames(old_array)[[1]],
                       dimnames(old_array)[[2]],
                       c("average", dimnames(old_array)[[3]]))

  new_array <- array(data = numeric(),
                     dim = new_dim,
                     dimnames = new_dimnames)

  new_array[,, 1] <- quickstart_obj$average
  new_array[,, 2:dim(new_array)[[3]]] <- old_array

  # CONVERT TO MATRIX ----
  data_matrix <- convertArrayToMatrix(new_array)

  # CREATE PAIRS_TABLE ----
  pairs_table <- data.frame(
    id = dimnames(old_array)[[3]],
    average = rep("average", dim(old_array)[[3]])
  )

  # calcED to get distinctiveness/averageness scores ----
  dist_scores  <- calcED(data_matrix, pairs_table) %>%
    dplyr::select(-2) %>%
    dplyr::rename("id" = 1,
                  "dist" = "EuclideanDistance") %>%
    dplyr::mutate(avg = (.01 + max(.data$dist)) - .data$dist)

  return(dist_scores)

}
