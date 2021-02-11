#' Convert 3D coordinates to 3D mesh
#'
#' Computes alpha-shape of three-dimensional coordinates, see \href{https://github.com/cran/alphashape3d}{here} for more info.
#'
#' @param point.matrix Coordinate data as matrix (individual points, x/y/z coordinates)
#' @param alpha See \link[alphashape3d]{ashape3d}
#' @param showme Return default 3D plot
#'
#' @return 3D mesh
#'
#' @examples
#'
#' \dontrun{data(example_3D_face)
#'
#' M.mesh <- convert_points_to_mesh(point.matrix = example_3D_face,
#' alpha = 1.2,
#' showme = FALSE)
#'
#'# Show 3D face in R notebook
#' rgl::open3d()
#' rgl::plot3d(M.mesh,
#'      type = "wire", col = "gray",
#'       box = FALSE, axes = FALSE,
#'       xlab = "", ylab = "", zlab = "",
#'       specular = "black")
#' rgl::aspect3d("iso")
#' rgl::rglwidget()
#' }
#'
#' @export
convert_points_to_mesh <- function(point.matrix, alpha, showme = TRUE) {

  shape.alpha <- alphashape3d::ashape3d(point.matrix, alpha = alpha)
  selrows <- shape.alpha$triang[,9] >= 2
  tr <- shape.alpha$triang[selrows, c("tr1", "tr2", "tr3")]

  mesh<-rgl::tmesh3d(
    vertices = t(shape.alpha$x),
    indices = t(tr),
    homogeneous = FALSE
  )

  if (showme == TRUE){
    rgl::plot3d(mesh, type="wire", col="gray", box=FALSE, axes=FALSE, xlab="", ylab="", zlab="")
  }

  return(mesh)
}
