#' Calculate Euclidean distance
#'
#' Calculate Euclidean between two sets of matrices, e.g. Procrustes-aligned shape coordinates
#'
#' @param coords_matrix Table containing point coordinates. Rows correspond to specimen,
#'  columns correspond to x/y(/z) coordinates. Rownames need to contain IDs of specimen.
#' @param pairs_table Table containing IDs of specimen for which Euclidean
#'  distance is to be calculated; column 1: specimen A, column 2: specimen B.
#'  Note that IDs in the two tables need to conform; e.g., character in both coords_matrix and pairs.table.
#'
#' @return Returns table listing specimen and their Euclidean distance
#'
#' @examples
#' my_data <- tibble::tibble(
#'  IDs = c(paste0("specimen_", 1:3)),
#'  LM1_x = c(1, 2, 5),
#'  LM1_y = c(1, 1, 1),
#'  LM2_x = c(1, 2, 5),
#'  LM2_y = c(3, 2.5, 2.5),
#'  LM3_x = c(3, 3, 6),
#'  LM3_y = c(2, 2.5, 2.5)
#'   ) %>%
#'  tibble::column_to_rownames("IDs")
#'
#' pairs <- t(combn(paste0("specimen_", 1:3), 2)) %>%
#'  as.data.frame(stringsAsFactors = FALSE)
#'
#' calcED(my_data, pairs)
#'
#' @export

calcED <- function(coords_matrix, pairs_table) {

  R <- nrow(pairs_table)
  ED <- numeric(R)

  for (i in 1:R) {

    # find index of line in coords_matrix that has same ID as specimens Ai/Bi on ith line of pairs_table
    id_A <- which(rownames(coords_matrix) %in% (pairs_table[i, 1]))
    id_B <- which(rownames(coords_matrix) %in% (pairs_table[i, 2]))

    # extract the coordinates for specimens Ai/Bi
    A <- as.matrix(coords_matrix[id_A, ])
    B <- as.matrix(coords_matrix[id_B, ])

    # calculate Euclidean distance, save as ith element in ED vector
    ED[i] <- sqrt(sum((A - B) ** 2))
  }

  ED_table <- tibble::tibble(
    "A" = pairs_table[, 1],
    "B" = pairs_table[, 2],
    "EuclideanDistance" = ED
  )

  return(ED_table)

}
