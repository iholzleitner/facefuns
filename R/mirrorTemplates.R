#' Mirror landmark templates
#'
#' @param data Array holding landmark data of dimensions p, k, and n. p = number of landmarks, k = dimension (2D or 3D), n = number of specimens.
#' @param mirr_lms Vector specifying order of mirrored landmarks
#'
#' @return Returns array containing mirrored landmark data
#'
#' @export
#' @examples
#' data(LondonSet_aligned)
#' data(mirr_lms)
#'
#' mirr <- mirrorTemplates(LondonSet_aligned, mirr_lms)
#'
#' geomorph::plotAllSpecimens(LondonSet_aligned)
#' geomorph::plotAllSpecimens(mirr)
#'
mirrorTemplates <- function(data, mirr_lms) {

  mirr <- sapply(seq_len(dim(data)[[3]]),
                   function(x) data[,,x] %*% matrix(c(-1, 0, 0, 1), 2, 2, byrow = T),
                   simplify = "array")

  dimnames(mirr) <- dimnames(data)

  # REORDER MIRRORED LANDMARKS
  mirr <- mirr[order(mirr_lms), , ]

  return(mirr)
}


