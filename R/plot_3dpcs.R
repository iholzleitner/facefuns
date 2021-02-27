#' Plot 3-D PCs
#'
#' @description
#' \lifecycle{experimental}
#'
#' Create and plot principal components of 3-D mesh data. If path to reference mesh is provided, existing mesh information will be used to create meshes; otherwise, alpha shape is computed
#'
#' @param input Object of class \code{prcomp}, \code{gm.prcomp} or \code{PC_list} (see \link[facefuns]{make_pcs})
#' @param ref Reference face used to visualize PCs (sample average is recommended)
#' @param which_pcs Which PCs are to be created. Single number or vector (maximum number of PCs to plot is 3)
#' @param vis_sd Extent of desired manipulation in units of standard deviation
#' @param alpha See \link[alphashape3d]{ashape3d}. Tweak value to adjust appearance of your meshes
#' @param ref_mesh Path to reference mesh
#'
#' @return  Returns plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#' path_to_ply <- system.file("extdata", "ply", package="facefuns")
#' data <- read_vertices(path_to_ply)
#' shapedata <- facefuns(data = data)
#'
#' # PLOT MESHES
#' # Will rebuild mesh from point coordinates
#' rgl::open3d()
#' plot_3dpcs(input = shapedata$pc_plot, which_pcs = 1:2, ref = shapedata$average, alpha = 2)
#'
#' # PLOT MESHES WITH REFERENCE MESH
#' # Will rebuild mesh from reference mesh
#' ref_mesh <- paste0(system.file("extdata", "obj", package="facefuns"), "/example.wavefront")
#'
#' rgl::open3d()
#' plot_3dpcs(input = shapedata$pc_plot, which_pcs = 1:2, ref = shapedata$average, ref_mesh = ref_mesh)
#' }
#'
plot_3dpcs <- function (input, ref, which_pcs = 1, vis_sd = 3, alpha = 1.2, ref_mesh = NULL){

  # NOTE TO SELF: add facefuns object as possible input, with ref being optional (à la 2-d version)

  # LIMIT NUMBER OF PCs ----
  if (length(which_pcs) > 3) {
    stop("This function will only plot up to 3 PCs per plot")
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
  # if ref_mesh is NULL, use convert_points_to_mesh; else, use convert_points_to_knownmesh
    if (is.null(ref_mesh)) {

    list_of_meshes <- rapply(shapes_list, function(x){
      convert_points_to_mesh(x, alpha, FALSE)
    }, how = "list")

  } else {
    # CHECK PATH
    if ((file.exists(ref_mesh) & (grepl("\\.obj$", ref_mesh) || grepl("\\.wavefront$", ref_mesh)))) {

      list_of_meshes <- rapply(shapes_list, function(x){
        convert_points_to_knownmesh(x, ref_mesh)
      }, how = "list")

    } else {
      stop("Path to reference mesh cannot be read. Is it an OBJ?")
    }

  }


  # PLOT MESHES ----
  rgl::clear3d(type = c("shapes", "lights"))
  rgl::light3d() # add light source to the scene
  rgl::mfrow3d(length(which_pcs), 2, byrow = FALSE, sharedMouse = TRUE)

  lapply(list_of_meshes, lapply, function(x){
    rgl::next3d(clear = FALSE)
    rgl::shade3d(x,
                 type="wire", col="gray",
                 box=FALSE, axes=FALSE,
                 xlab="", ylab="", zlab="",
                 specular="black")
  })

  rgl::aspect3d("iso")


  # ADD TEXTURE!

}
