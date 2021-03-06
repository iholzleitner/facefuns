#' Calculate fluctuating asymmetry
#'
#' @description
#' \lifecycle{experimental}
#'
#' Function uses \link[geomorph]{bilat.symmetry} to calculate fluctuating asymmetry (FA) component for each template. FA score is calculated as distance between FA component and mean symmetric shape
#'
#' @param data \code{facefuns} object or three-dimensional array of dimensions p, k, and n
#' @param mirroredlandmarks Vector specifying index of mirrored landmarks
#'
#' @return Returns tibble containing ID and FA values
#'
#' @export
#' @examples
#' data(LondonSet_aligned)
#' data(mirroredlandmarks)
#' calc_fageo(LondonSet_aligned, mirroredlandmarks)
#'
calc_fageo <- function (data, mirroredlandmarks) {

  if (any(class(data) == "facefuns_obj")) {
    data <- data$aligned
  } else if (!is_shape_array(data)) {
    stop("Your data is neither a facefuns object nor a three-dimensional array")
  }

  # CREATE LM PAIRS TABLE ----
  lm_pairs <- tibble::tibble(org = as.numeric(dimnames(data)[[1]]),
                             mirr = mirroredlandmarks+1) %>%
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
