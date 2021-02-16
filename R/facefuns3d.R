#' Quick start to shape analyses of 3-D data
#'
#' @description
#' \lifecycle{maturing}
#'
#' Performs some of the routine steps for getting landmark data ready for shape analyses, such as Procrustes alignment and principal component analysis. For more details see vignettes
#' \code{vignette("facefuns", package = "facefuns")} and \code{vignette("intro3d", package = "facefuns")}
#'
#' @param data Three-dimensional array of dimensions p, k, and n. p = number of landmarks, k = dimension (2D or 3D), n = number of specimens
#' @param pc_criterion Criterion used to choose which PCs to retain. See \link[facefuns]{select_pcs}
#' @param quiet If false prints short summary of loaded data
#'
#' @return Returns a list of the following components:
#' \item{array}{Three-dimensional array containing Procrustes-aligned data}
#' \item{average}{Coordinates of sample average for plotting}
#' \item{pc_info}{List of selected PCs (including their SD, variance explained and cumulative variance explained), number of selected PCs, criterion used to select PCs}
#' \item{pc_scores}{Principal component scores}
#' \item{pc_plot}{PCs for plotting. Will by default create list of coordinates for all selected PCs at +/- 3SDs. To create plots of other PCs or at different level of SD, please see \link[facefuns]{plot_2dpcs}}
#' \item{summary}{Short summary of key descriptives}
#'
#' @keywords internal
#'
facefuns3d <- function (data, pc_criterion = "broken_stick", quiet = FALSE){

  # CHECK DATA ----
  # add code to check data is of appropriate dimensions

  # ALIGN DATA ----
  gpa <- Morpho::ProcGPA(data)

  # pull aligned data
  data_aligned <- gpa$rotated

  # PCA ----
  data_matrix <- convert_array_to_matrix(data_aligned)

  pca_output <- stats::prcomp(data_matrix)
  pc_sel <- select_pcs(pca_output = pca_output,
                       method = pc_criterion)

  # save scores
  data_scores <- pca_output$x[, 1:pc_sel$n] %>%
    as.data.frame()

  # MAKE 3D PCS ----
  ref <- gpa$mshape
  vis_pcs <- make_pcs(pca_output = pca_output, ref = ref, which_pcs = 1:pc_sel$n, vis_sd = 3)

  # SUMMARY
  summary <- list(
    data_lm = dim(data_aligned)[[1]],
    data_dim = dim(data_aligned)[[2]],
    data_n = dim(data_aligned)[[3]],
    pc_crit = pc_sel$method,
    pc_nPCs = pc_sel$n
  )

  if (quiet == FALSE) {
    cat(paste0("The loaded data set contains ", summary$data_n, " specimen, containing ", summary$data_lm, " vertices.", "\n",
               "The ", summary$pc_crit, " criterion was used to select ", summary$pc_nPCs, " principal components."))}

  # RETURN----
  to_return <- invisible(list(
    array = data_aligned,
    average = ref,
    pc_info = pc_sel,
    pc_scores = data_scores,
    pc_plot = vis_pcs,
    summary = summary
  ))

  class(to_return) <- c("facefuns_obj", "list")
  return(to_return)

}

#' Print for facefuns_obj
#'
#' @param x a list of class facefuns_obj
#' @param ... arguments passed to or from other methods
#'
#' @return prints summary
#' @keywords internal
#'
print.facefuns_obj <- function(x, ...) {
  invisible(x)
}
