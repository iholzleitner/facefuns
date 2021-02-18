#' Convert 3-D coordinates to 3-D mesh based on reference mesh
#'
#' @description
#' \lifecycle{experimental}
#'
#' Build triangle mesh3d object from 3-D coordinates based on reference mesh with homologous vertices
#'
#' @param point_matrix 3-D coordinates to be visualized (rows = vertices, columns = coordinates)
#' @param obj Path to reference mesh in OBJ format
#'
#' @return Object of class \link[rgl]{mesh3d}
#' @export
#'
#' @examples
#' \dontrun{
#' # CREATE EXAMPLE POINT MATRIX
#' path_to_ply <- system.file("extdata", "ply", package="facefuns")
#' data <- read_vertices(path_to_ply)
#'
#' shapedata <- facefuns(data = data,
#'                       pc_criterion = "broken_stick",
#'                       quiet = TRUE)
#'
#' point_matrix <- shapedata$pc_plot$PC1$minus
#'
#' # PATH TO EXAMPLE REFERENCE MESH
#' obj <- paste0(system.file("extdata", "obj", package="facefuns"), "/example.wavefront")
#'
#' # RUN FUNCTION
#' mesh <- convert_points_to_knownmesh(point_matrix = point_matrix,
#'                                     obj = obj)
#'
#' # PLOT RESULT
#' rgl::open3d()
#' rgl::shade3d(mesh,
#'              box = FALSE, axes = FALSE,
#'              xlab = "", ylab = "", zlab = "",
#'              col = "white", specular = "black")
#' }

convert_points_to_knownmesh <- function (point_matrix, obj) {

  reference_mesh <- rgl::readOBJ(obj)

  indices <- reference_mesh$it
  texcoords <- reference_mesh$texcoords

  mesh <- rgl::tmesh3d(vertices = t(cbind(point_matrix, 1)), # add fourth coordinate/homogenous = T
                       indices = indices,
                       texcoords = texcoords,
                       homogeneous = TRUE)

}
