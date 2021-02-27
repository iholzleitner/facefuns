#' Calculate fluctuating asymmetry
#'
#' @description
#' \lifecycle{maturing}
#'
#' Calculates fluctuating asymmetry (FA) of landmark templates. FA scores are calculated as Euclidean distance between original and symmetrised templates, corrected for directional asymmetry of sample
#'
#' @param data \code{facefuns} object or three-dimensional array of dimensions p, k, and n
#' @param mirroredlandmarks Vector specifying order of mirrored landmarks
#'
#' @return Returns tibble containing ID and FA scores
#'
#' @export
#' @examples
#' data(LondonSet_aligned)
#' data(mirroredlandmarks)
#' calc_fa(LondonSet_aligned, mirroredlandmarks)
#'
calc_fa <- function (data, mirroredlandmarks) {

  if (any(class(data) == "facefuns_obj")) {
    org <- data$aligned
  } else if (is_shape_array(data)) {
    org <- data
  } else {
    stop("Your data is neither a facefuns object nor a three-dimensional array")
  }

  # SET UP ----
  mirr <- mirror_templates(org, mirroredlandmarks)
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
  # Klingenberg 2015, p. 865: the average shape of all the original templates minus the average of all the reflected and relabeled copies
  DA <- geomorph::mshape(data_aligned[,, 1:n]) - geomorph::mshape(data_aligned[,, (n+1):(n*2)])

  # FLUCTUATING ASYMMETRY -----
  # Klingenberg 2015, p. 865: the variation of individual asymmetries around the average of directional asymmetry
  # individual asymmetry = diff between original and reflected templates (equivalent to difference between original and symmetrised templates divided by 2)
  FA_score <- sapply(seq_len(dim(org)[[3]]),
                     function(i) sqrt(sum((data_aligned[,, i] - data_aligned[,, (i+n)] - DA)**2))
                     )

  fa_table <- tibble::tibble(
    id = dimnames(org)[[3]],
    fa = FA_score)

  return(fa_table)

}
