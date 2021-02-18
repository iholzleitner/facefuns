#' Convert 3-D coordinates to 3-D mesh
#'
#' Computes alpha-shape of three-dimensional coordinates, see \href{https://github.com/cran/alphashape3d}{here} for more info
#'
#' @param point_matrix Coordinate data as matrix (individual points, x/y/z coordinates)
#' @param alpha See \link[alphashape3d]{ashape3d}
#' @param showme Return default 3D plot
#'
#' @return 3D mesh
#'
#' @export
#' @examples
#'
#' \dontrun{
#'
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
#' # RUN FUNCTION
#' mesh <- convert_points_to_mesh(point_matrix = point_matrix,
#'                                alpha = 1.2,
#'                                showme = FALSE)
#'
#' # PLOT RESULT
#' rgl::open3d()
#' rgl::plot3d(mesh,
#'             type = "wire", col = "gray",
#'             box = FALSE, axes = FALSE,
#'             xlab = "", ylab = "", zlab = "",
#'             specular = "black")
#' rgl::aspect3d("iso")
#' }
#'
#'
convert_points_to_mesh <- function(point_matrix, alpha, showme = TRUE) {

  shape.alpha <- alphashape3d::ashape3d(point_matrix, alpha = alpha)
  selrows <- shape.alpha$triang[,9] >= 2
  tr <- shape.alpha$triang[selrows, c("tr1", "tr2", "tr3")]

  mesh <- rgl::tmesh3d(
    vertices = t(shape.alpha$x),
    indices = t(tr),
    homogeneous = FALSE
  )

  if (showme == TRUE){
    rgl::plot3d(mesh,
                type="wire", col="gray",
                box=FALSE, axes=FALSE,
                xlab="", ylab="", zlab="")
  }

  return(mesh)
}
