#' Quickstart to shape analyses of 2-D or 3-D data
#'
#' @description
#' \lifecycle{maturing}
#'
#' Performs some of the routine steps for getting landmark data ready for shape analyses, such as Procrustes alignment and principal component analysis. For more details see vignette:
#' \code{vignette("intro", package = "facefuns")}
#'
#' @param data Array of dimensions p, k, and n, where p = number of landmarks/vertices, k = dimensionality of data (2 or 3), n = number of specimens
#' @param pc_criterion Criterion used to choose which PCs to retain. See \link[facefuns]{select_pcs}
#' @param plot_sample Plot sample to check data. See \link[geomorph]{plotAllSpecimens}
#' @param auto_rotate Landmark templates are sometimes no longer upright after Procrustes-alignment. Auto-rotate uses \link[geomorph]{rotate.coords} to guess which type of rotation is required
#' @param quiet If false, prints short summary of loaded data
#'
#' @return Returns a list of the following components:
#' \item{array}{Three-dimensional array containing Procrustes-aligned data}
#' \item{average}{Coordinates of sample average for plotting}
#' \item{pc_info}{List of selected PCs (including their SD, variance explained and cumulative variance explained), number of selected PCs, criterion used to select PCs}
#' \item{pc_scores}{Principal component scores}
#' \item{pc_plot}{PCs for plotting. Will by default create list of coordinates for all selected PCs at +/- 3SDs. To create plots of other PCs or at different level of SD, please see \link[facefuns]{make_pcs} or  \link[facefuns]{plot_2dpcs}}
#' \item{summary}{Short summary of key descriptives}
#'
#' @export
#'
#' @examples
#'
#' ### 2-D DATA
#' path_to_tem <- system.file("extdata", "tem", package="facefuns")
#' remove_points <- c(45:50, 100:104, 116:125, 146:158, 159:164, 165:170, 171:174, 175:179, 184:185)
#'
#' data2d <- read_lmdata(lmdata = path_to_tem, remove_points = remove_points)
#' shapedata2d <- facefuns(data = data2d,
#' pc_criterion = "broken_stick")
#'
#' # Plot sample
#' shapedata2d$summary$plot_sample() #or geomorph::plotAllSpecimens(shapedata2d$array)
#'
#' # Plot PCs
#' plot_2dpcs(shapedata2d$pc_plot, shapedata2d$average)
#'
#' ### 3-D DATA
#' path_to_ply <- system.file("extdata", "ply", package="facefuns")
#'
#' data3d <- read_vertices(path_to_ply)
#' shapedata3d <- facefuns(data = data3d,
#' pc_criterion = "broken_stick")
#'
#'
facefuns <- function(data, pc_criterion = "broken_stick", plot_sample = TRUE,  auto_rotate = TRUE, quiet = FALSE){

  if (is_shape_array(data)) {

    if (dim(data)[[2]] == 2) {

      facefuns2d(data = data, pc_criterion = pc_criterion, plot_sample = plot_sample, auto_rotate = auto_rotate, quiet = quiet)

    } else {

      facefuns3d(data = data, pc_criterion = pc_criterion, quiet = quiet)

    }

  } else {
    stop("Your data needs to be an array of the dimensions p, k and n, where p is the number of landmarks or vertices, k is the dimensionality of your data (2 or 3) and n is the number of specimens")
  }

}
