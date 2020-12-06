#' Calculate fluctuating asymmetry
#'
#' @description
#' \lifecycle{experimental}
#'
#' Calculates fluctuating asymmetry (FA) of landmark templates. FA scores are calculated as Euclidean distance between original and symmetrized templates, corrected for sample directional asymmetry
#'
#' @param data Quickstart object or three-dimensional array of dimensions p, k, and n
#' @param mirr_lms Vector specifying order of mirrored landmarks
#'
#' @return Returns tibble containing ID and FA scores
#'
#' @export
#' @examples
#' data(LondonSet_aligned)
#' data(mirr_lms)
#' calcFA(LondonSet_aligned, mirr_lms)
#'
calcFA <- function (data, mirr_lms) {

  if (class(data) == "quickstart") {
    org <- data$array
  } else if (is.array(data)) {
    org <- data
  } else {
    stop("Your data is neither a quickstart object nor an array")
  }

  # SET UP ----
  mirr <- mirrorTemplates(org, mirr_lms)
  n <- dim(org)[[3]]

  # CREATE ARRAY ----
  # one array to hold them all
  super_array <- array(data = numeric(),
                       dim = c(dim(org)[[1]],
                               dim(org)[[2]],
                               n * 2))
  super_array[,, 1:n] <- org
  super_array[,, (n+1):(2*n)] <- mirr

  # CONDUCT GPA ----
  gpa <- geomorph::gpagen(super_array, print.progress = FALSE)
  data_aligned <- gpa$coords

  # DIRECTIONAL ASYMMETRY ----
  # Klingenberg 2015: the average shape of all the original templates minus the average of all the reflected and relabeled copies
  DA <- geomorph::mshape(data_aligned[,,1:n]) - geomorph::mshape(data_aligned[,,(n+1):(n*2)])

  # FLUCTUATING ASYMMETRY -----
  # Klingenberg 2015: variation of individual asymmetries (difference between original and symmetric, i.e. mean of original and mirrored templates) around the average of directional asymmetry
  FA_score <- sapply(seq_len(dim(org)[[3]]),
                     function(i) sqrt(sum((data_aligned[,,i] - geomorph::mshape(data_aligned[,, c(i, (i+n))]) - DA)**2))
                     )

  fa_table <- tibble::tibble(
    id = dimnames(org)[[3]],
    fa = FA_score)

  return(fa_table)

}
