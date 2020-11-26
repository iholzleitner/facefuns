#' Quick start to shape analyses
#'
#' Performs some of the routine steps for getting landmark data ready for shape analyses, such as Procrustes alignment and principal component analysis. For more details see vignette:
#' \code{vignette("intro", package = "facefuns")}
#'
#' @param data Three-dimensional array of dimensions p, k, and n. p = number of landmarks, k = dimension (2D or 3D), n = number of specimens.
#' @param rotate The type of rotation or flip to be performed (after Procrustes alignment, landmark templates might have to be rotated). Specimens can be flipped with respect to x or y axes, or rotated clockwise or counter-clockwise. See \code{geomorphs}'s \link[geomorph]{rotate.coords}
#' @param plot_sample Plot sample to check data. See  \code{geomorphs}'s \link[geomorph]{plotAllSpecimens}
#' @param pc_criterion Criterion used to choose which PCs to retain. See \link[facefuns]{selectPCs}
#'
#' @return Returns a list of the following components:
#' \item{array}{Three-dimensional array containing Procrustes-aligned data}
#' \item{average}{Coordinates of sample average for plotting}
#' \item{pc_info}{List of selected PCs (including their SD, variance explained and cumulative variance explained), number of selected PCs, criterion used to select PCs}
#' \item{pc_scores}{Principal component scores}
#' \item{pc_plot}{PCs for plotting. Will by default create list of coordinates for all selected PCs at +/- 3SDs. To create plots of other PCs or at different level of SD, please see \link[facefuns]{plot2DPCs}}
#' @export
#'
#' @examples
#' path_to_tem <- system.file("extdata", "tem", package="facefuns")
#' remove_points <- c(45:50, 100:104, 116:125, 146:158, 159:164, 165:170, 171:174, 175:179, 184:185)
#'
#' data <- read_lmdata(lmdata = path_to_tem, remove_points = remove_points)
#' shapedata <- quickstart(data = data,
#' rotate = "rotateC",
#' plot_sample = TRUE,
#' pc_criterion = "broken_stick")
#'
#'plot2DPCs(shapedata$pc_plot, shapedata$average)
quickstart <- function (data, rotate = c(NA, "flipX", "flipY", "rotateC", "rotateCC"), plot_sample = TRUE, pc_criterion = "broken_stick") {

  # CHECK DATA ----
  # add code to check data is of appropriate dimensions

  # ALIGN DATA ----
  gpa <- geomorph::gpagen(data, print.progress = FALSE)

  # rotate coords ----
  data_aligned <- gpa$coords

  rotate <- match.arg(rotate)

  if (!is.na(rotate)){
    for (rtype in rotate) {
      data_aligned <- geomorph::rotate.coords(data_aligned, type = rtype)
    }}

  # re-assiagn dimnames ----
  # rotations appears to delete dimnames for 2nd and 3rd array dims??
  dimnames(data_aligned) <- list(dimnames(gpa$coords)[[1]],
                                 dimnames(gpa$coords)[[2]],
                                 dimnames(gpa$coords)[[3]])


  # PLOT ----
  if (plot_sample == TRUE ) {
    geomorph::plotAllSpecimens(data_aligned) %>% cat()}


  # PCA ----
  pca_output <- geomorph::gm.prcomp(data_aligned)
  data_scores <- tibble::as_tibble(pca_output$x)
  # rename columns
  names(data_scores) <- make_id(ncol(data_scores), "PC")
  # re-add IDs to table, remove "ID="
  data_scores <- data_scores %>%
    tibble::add_column(.before = 1,
                       id = gsub("^ID=", "", dimnames(data_aligned)[[3]])) %>%
    tibble::column_to_rownames(var = "id")
  # select PCs
  pc_sel <- selectPCs(pca_output = pca_output, method = pc_criterion)


  # MAKE 2D PCS ----
  ref <- geomorph::mshape(data_aligned)
  vis_pcs <- make2DPCs(pca_output = pca_output, ref = ref, which_pcs = 1:pc_sel$n, vis_sd = 3)


  # RETURN----
  invisible(list(
    array = data_aligned,
    average = ref,
    pc_info = pc_sel,
    pc_scores = data_scores,
    pc_plot = vis_pcs
  ))

  # TODO ----
  # add summary to print with number of specimen, number of landmarks, selected PCs - what else?

}
