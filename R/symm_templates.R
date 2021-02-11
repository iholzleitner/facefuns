#' Symmetrize landmark templates
#'
#' Mirrors landmark templates for each specimen and calculates mean of original and mirrored template (see also \link[facefuns]{mirror_templates})
#'
#' @param data Array holding landmark data of dimensions p, k, and n. p = number of landmarks, k = dimension (2D or 3D), n = number of specimens.
#' @param mirroredlandmarks Vector specifying order of mirrored landmarks
#'
#' @return Returns array containing symmetrized landmark data
#'
#' @export
#' @examples
#'
#' data(LondonSet_aligned)
#' data(mirroredlandmarks)
#'
#' symm <- symm_templates(LondonSet_aligned, mirroredlandmarks)
#'
#' geomorph::plotAllSpecimens(LondonSet_aligned)
#' geomorph::plotAllSpecimens(symm)
#'
symm_templates <- function(data, mirroredlandmarks){

  mirr <- mirror_templates(data, mirroredlandmarks)

  symm <- sapply(seq_len(dim(data)[[3]]),
                 function(i) {
                   x <- list(data[,,i], mirr[,,i])
                   Reduce(`+`, x) / length(x)},
                 simplify = "array")

  dimnames(symm) <- dimnames(data)

  return(symm)
}

