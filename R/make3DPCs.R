#' Make and plot 3D principal components
#'
#' @description
#' \lifecycle{experimental}
#'
#' @param pca_output Prcomp output
#' @param numberOfPCs How many PCs to visualize
#' @param visSD How many standard deviations to visualize, e.g. 3
#' @param mean_shape Reference coordinates on which visualization will be based
#' @param plot Plot or not
#' @param return Return list of PCs
#'
#' @return List of 3D meshes visualizing PCs according to requested SDs; e.g., if numberOfPCs was set to 3, six meshes will be returned - PCs 1, 2, and 3 at plus and minus visSD
#' @export
#'
make3DPCs <- function(pca_output, numberOfPCs, visSD, mean_shape, plot = TRUE, return = TRUE) {

  shapes <- shape_names <- NULL

  for (i in 1:(numberOfPCs)) {

    multiplier <- pca_output$sdev[i] * visSD

    minus <- as.vector(t(mean_shape)) - as.matrix(multiplier * (t(pca_output$rotation[, i])))
    plus <- as.vector(t(mean_shape)) + as.matrix(multiplier * (t(pca_output$rotation[, i])))

    shapes <- rbind(shapes, minus, plus)
    shape_names <- c(shape_names,
                     paste("PC", i, "_minus", visSD, "SD", sep = ""),
                     paste("PC", i, "_plus", visSD, "SD", sep = "")
    )
  }
  shapes <- geomorph::arrayspecs(shapes, dim(mean_shape)[1], dim(mean_shape)[2])
  shapes <- lapply(seq(dim(shapes)[3]), function(x) shapes[, , x])
  names(shapes) <- shape_names

  mesh_list <- list()

  for (i in 1:length(shapes)){
    mesh_list[[i]] <- convertPointsToMesh(shapes[[names(shapes)[i]]], 1.2, FALSE)
  }

  names(mesh_list) <- shape_names

  if (plot==TRUE){
    rgl::open3d()
    rgl::mfrow3d(numberOfPCs, 2, sharedMouse = TRUE)
    for (i in 1:length(shapes)){
      rgl::plot3d(mesh_list[[i]], type="wire", col="gray", box=FALSE, axes=FALSE, xlab="", ylab="", zlab="", specular="black")
    }
    rgl::rglwidget()
  }

  if (return==TRUE){
    return(mesh_list)
  }
}
