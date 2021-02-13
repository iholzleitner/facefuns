#' Plot 3D PCs
#'
#' @description
#' \lifecycle{experimental}
#'
#' @param input Object of class \code{prcomp}, \code{gm.prcomp} or PC_list (see \code{make_pcs})
#' @param ref Reference face (sample average is recommended)
#' @param which_pcs Which PCs are to be created. Single number or vector, maximum allowed is 3
#' @param vis_sd Extent of desired manipulation in units of standard deviation
#' @param alpha See \link[alphashape3d]{ashape3d}
#'
#' @return  Returns plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#' path_to_ply <- system.file("extdata", "ply", package="facefuns")
#' data <- read_vertices(path_to_ply)
#' shapedata <- facefuns3d(data = data,
#' pc_criterion = "broken_stick")
#'
#' plot_3dpcs(input = shapedata$pc_plot, which_pcs = 1, ref = shapedata$average, alpha = 2)
#'
#' }
#'
plot_3dpcs <- function (input, ref, which_pcs = 1, vis_sd = 3, alpha = 1.2){

  # LIMIT NUMBER OF PCs ----
  if (length(which_pcs) > 3) {
    stop("This function will only plot up to 1 PC per plot")
  }

  # CHECK INPUT ----
  if ("sdev" %in% names(input)) {

    shapes_list <- make_pcs(input, ref, which_pcs, vis_sd)

  } else if (is.list(input) && "PC_list" %in% class(input)) {

    pcs <- paste0("PC", which_pcs)
    shapes_list <- input[pcs]

  } else {

    stop("Input cannot be read - check it is of class gm.prcomp, prcomp or PC_list")

  }

  # CHECK ALL PCs are available ----
  if (!all(paste0("PC", which_pcs) %in% names(shapes_list))) {
    stop("At least one of the PCs you are trying to plot is out of range")
  }

  # CREATE MESHES ----
  list_of_meshes <- rapply(shapes_list, function(x){
    convert_points_to_mesh(x, alpha, FALSE)
  }, how = "list")

  # PLOT MESHES ----
  # Not everyone wants the rglwidget maybe - make optional
  rgl::open3d()
  rgl::mfrow3d(length(which_pcs), 2, sharedMouse = TRUE)

  lapply(list_of_meshes, lapply, function(x){
    rgl::plot3d(x, type="wire", col="gray", box=FALSE, axes=FALSE, xlab="", ylab="", zlab="", specular="black")
  })

  rgl::rglwidget()

  # ADD TEXTURE!


}
