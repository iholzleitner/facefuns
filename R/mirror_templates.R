#' Mirror landmark templates
#'
#' @param data Array holding landmark data of dimensions p, k, and n. p = number of landmarks, k = dimension (2D or 3D), n = number of specimens.
#' @param mirroredlandmarks Vector specifying order of mirrored landmarks
#'
#' @return Returns array containing mirrored landmark data
#'
#' @export
#' @examples
#' data(LondonSet_aligned)
#' data(mirroredlandmarks)
#'
#' mirr <- mirror_templates(LondonSet_aligned, mirroredlandmarks)
#'
#' geomorph::plotAllSpecimens(LondonSet_aligned)
#' geomorph::plotAllSpecimens(mirr)
#'
mirror_templates <- function(data, mirroredlandmarks) {

  mirr <- sapply(seq_len(dim(data)[[3]]),
                   function(x) data[,,x] %*% matrix(c(-1, 0, 0, 1), 2, 2, byrow = T),
                   simplify = "array")

  # REORDER MIRRORED LANDMARKS
  mirr <- mirr[order(mirroredlandmarks), , ]

  dimnames(mirr) <- dimnames(data)

  return(mirr)
}


