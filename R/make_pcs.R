#' Create list of PCs
#'
#' @description
#' \lifecycle{maturing}
#'
#'
#' @param pca_output Object of class \code{prcomp} or \code{gm.prcomp}
#' @param ref 2-D or 3-D coordinates of reference face
#' @param which_pcs Which PCs are to be created. Single number or vector, maximum length is 3
#' @param vis_sd Extent of desired manipulation in units of standard deviation
#'
#' @return A list of 2-D or 3-D coordinates for the reference face at \code{-vis_SD} and \code{vis_SD} standard deviations for each principal component
#' @export
#' @examples
#'
#' data(LondonSet_aligned)
#' data_aligned <- LondonSet_aligned
#' pca_output <- geomorph::gm.prcomp(data_aligned)
#' ref <- geomorph::mshape(data_aligned)
#' pc_list <- make_pcs(pca_output, ref, which_pcs = 1:3, vis_sd = 3)
#'
make_pcs <- function (pca_output, ref, which_pcs, vis_sd) {

  if ("sdev" %in% names(pca_output) && "rotation" %in% names(pca_output)) {

    list_of_pcs <- lapply(which_pcs, function(i) {

      # Calculate amount of manipulation
      multiplier <- pca_output$sdev[i] * vis_sd

      # Apply manipulation to reference face (minus/plus)
      minus <- as.vector(t(ref)) - as.matrix(multiplier * (t(pca_output$rotation[, i])))
      plus <- as.vector(t(ref)) + as.matrix(multiplier * (t(pca_output$rotation[, i])))
      shapes <- rbind(minus, plus)

      # Convert to array
      shapes <- geomorph::arrayspecs(shapes, dim(ref)[1], dim(ref)[2])

      # Convert to list
      shapes <- lapply(seq(dim(shapes)[3]), function(x) shapes[, , x])
      names(shapes) <- c("minus", "plus")

      return(shapes)

    })

    names(list_of_pcs) <- paste0("PC", which_pcs)
    class(list_of_pcs) <- "PC_list"

    return(list_of_pcs)

  } else {

    stop("Incorrect input format")

  }

}
