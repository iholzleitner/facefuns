#' Symmetrize landmark templates
#'
#' Mirrors landmark templates for each specimen and calculates mean of original and mirrored template (see also \link[facefuns]{mirrorTemplates})
#'
#' @param data Array holding landmark data of dimensions p, k, and n. p = number of landmarks, k = dimension (2D or 3D), n = number of specimens.
#' @param mirr_lms Vector specifying order of mirrored landmarks
#'
#' @return Returns array containing symmetrized landmark data
#' @export
#'
#' @examples
#' data(LondonSet_aligned)
#' data(mirr_lms)
#'
#' symm <- symmTemplates(LondonSet_aligned, mirr_lms)
#'
#' geomorph::plotAllSpecimens(LondonSet_aligned)
#' geomorph::plotAllSpecimens(symm)
symmTemplates <- function(data, mirr_lms){

  mirr <- mirrorTemplates(data, mirr_lms)

  symm <- sapply(seq_len(dim(data)[[3]]),
                 function(i) {
                   x <- list(data[,,i], mirr[,,i])
                   Reduce(`+`, x) / length(x)},
                 simplify = "array")

  dimnames(symm) <- dimnames(data)

  return(symm)
}
