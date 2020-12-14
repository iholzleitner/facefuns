#' Calculate fluctuating asymmetry
#'
#' @description
#' \lifecycle{experimental}
#'
#' Function uses \link[geomorph]{bilat.symmetry} to calculate fluctuating asymmetry component for each template. FA score is calculated as distance between FA component and mean symmetric shape
#'
#' @param data Quickstart object or three-dimensional array of dimensions p, k, and n
#' @param mirr_lms Vector specifying order of mirrored landmarks
#'
#' @return Returns tibble containing ID and asymmetry values
#'
#' @export
#' @examples
#' data(LondonSet_aligned)
#' data(mirr_lms)
#' calcFA(LondonSet_aligned, mirr_lms)
#'
calcFA_geo <- function (data, mirr_lms) {

  if (class(data) == "facefuns_obj") {
    data <- data$array
  } else if (!is_shape_array(data)) {
    stop("Your data is neither a facefuns object nor a three-dimensional array")
  }

  # CREATE LM PAIRS TABLE ----
  lm_pairs <- tibble::tibble(org = as.numeric(dimnames(data)[[1]]),
                             mirr = mirr_lms+1) %>%
    as.matrix(dimnames = NULL)

  sym <- geomorph::bilat.symmetry(A = data,
                                  ind = dimnames(data)[[3]],
                                  object.sym = TRUE,
                                  land.pairs = lm_pairs,
                                  RRPP = TRUE, iter = 149, print.progress = FALSE)

  # CALCULATE PROCRUSTES DISTANCE between FA component and mshape of symmetric templates ----
  # If if read the function correctly, FA component is built from specimen-specific side-deviation plus mean minus DA; so I subtract mean again to be left with FA, is that right??
  # also https://groups.google.com/g/geomorph-r-package/c/V95DGFu1Y3Y/m/XjBfQCLUCAAJ;
  FA_score <- sapply(seq_len(dim(data)[[3]]),
                     function(i) sqrt(sum((sym$FA.component[,, i] - geomorph::mshape(sym$symm.shape))**2))
                     )

  fa_table <- tibble::tibble(
    id = dimnames(data)[[3]],
    fa.geo = FA_score)

  return(fa_table)

}
