#' Basic output for face shape data
#'
#' Function performs some of the routine steps for getting landmark data ready for shape analyses. Data can be imported from a directory of Webmorph template files, or by reading an existing TPS file. For more details see vignette:
#' \code{vignette("intro", package = "facefuns")}
#'
#' @param path_to_data Path to .tps file or directory containing .tem files
#' @param p Number of landmarks images were delineated with. Defaults to value for FRL standard template.
#' @param remove_points If reading from template files, it can be specified whether any landmarks should be removed. If no value is entered, all landmarks are retained.
#' @param rotate The type of rotation or flip to be performed. Specimens can be flipped with respect to x or y axes, or rotated clockwise or counter-clockwise. See \code{geomorphs}'s \link[geomorph]{rotate.coords}
#' @param plot_all Plot landmark coordinates of sample. See \code{geomorphs}'s \link[geomorph]{plotAllSpecimens}
#' @param pc_criterion Criterion used to choose which PCs to retain. See \link[facefuns]{selectPCs}
#'
#' @return Returns a list of the following:
#' \item{data}{Three-dimensional array containing original data}
#' \item{aligned}{Three-dimensional array containing Procrustes-aligned/rotated data}
#' \item{scores}{Principal component scores for each face}
#' \item{pcs}{List of selected PCs, their importance and method used to select PCs}
#' \item{ref}{Coordinates of sample average}
#' \item{plot}{Coordinates of sample average at -3SD and +3SD for each selected PC}
#'
#' @export
#' @examples
#' path_to_tem <- system.file("extdata", "tem", package="facefuns")
#' x <- basics(path_to_data = path_to_tem,
#'             p = 189,
#'             remove_points = NA,
#'             rotate = "rotateC",
#'             plot = TRUE,
#'             pc_criterion = "broken_stick"
#'             )
#'
#' remove_points <- c(45:50, 100:104, 116:125, 146:158, 159:164, 165:170, 171:174, 175:179, 184:185)
#'
#' x <- basics(path_to_data = path_to_tem,
#'             p = 189,
#'             remove_points = remove_points,
#'             rotate = "rotateC",
#'             plot = TRUE,
#'             pc_criterion = "broken_stick"
#'             )
#'
basics <- function (path_to_data, p = 189, remove_points = NA, rotate = c(NA, "flipX", "flipY", "rotateC", "rotateCC"), plot_all = TRUE, pc_criterion = "broken_stick") {

  ## ---------
  # LOAD DATA
  # ---------
  # if path_to_data is tps file, load; otherwise create tps file
  if (stringr::str_detect(path_to_data, ".tps")) {
    data <- geomorph::readland.tps(path_to_data, specID = "ID", warnmsg = FALSE)
  } else {
    # Create TPS
    path_to_tps <- tempfile(fileext = ".tps")

    convertTEMtoTPS(path_to_tem = path_to_data,
                    p = p,
                    remove_points = remove_points,
                    path_to_tps = path_to_tps)
    # Load TPS
    data <- geomorph::readland.tps(path_to_tps, specID = "ID", warnmsg = FALSE)
  }

  # ---------
  # ALIGN DATA
  # ---------
  gpa <- geomorph::gpagen(data, print.progress = FALSE)

  # rotate coords
  data_aligned <- gpa$coords

  rotate <- match.arg(rotate)

  if (!is.na(rotate)){
    for (rtype in rotate) {
      data_aligned <- geomorph::rotate.coords(data_aligned, type = rtype)
    }}

  dimnames(data_aligned)[[3]] <- dimnames(gpa$coords)[[3]]

  # ---------
  # PLOT data
  # ---------
  plot_allsp <- function(){geomorph::plotAllSpecimens(data_aligned)}

  if (plot_all == TRUE) {
    plot_allsp()
  }

  # ---------
  # PCA
  # ---------
  pca_output <- geomorph::gm.prcomp(data_aligned)
  data_scores <- tibble::as_tibble(pca_output$x)
  # rename columns
  names(data_scores) <- make_id(ncol(data_scores), "PC")
  # re-add IDs to table
  data_scores <- data_scores %>%
    tibble::add_column(.before = 1, id = dimnames(data_aligned)[[3]])
  # select PCs
  pc_sel <- selectPCs(pca_output = pca_output, method = pc_criterion)


  # ---------
  # MAKE 2D PCS
  # ---------
  ref <- geomorph::mshape(data_aligned)
  pc_plot <- make2DPCs(pca_output = pca_output, ref = ref, which_pcs = 1:pc_sel$n, vis_sd = 3)


  # ---------
  # RETURN
  # ---------

  to_return <- list(
    data = data,
    aligned = data_aligned,
    scores = data_scores,
    average = ref,
    pc_info = pc_sel,
    pc_plot = pc_plot,
    plot_all = plot_allsp()
  )

  class(to_return) <- "basic_output"

  return(to_return)

#  invisible(list(
#    data = data,
#    aligned = data_aligned,
#    scores = data_scores,
#    pcs = pc_sel,
#    average = ref,
#    plot = pc_plot
#  ))

}

#' Print for basic_output
#'
#' @param x a list of class basic_output
#' @param ... arguments passed to or from other methods
#'
#' @return prints the basic output
#' @export
#'
print.selected_PCs <- function(x, ...) {
  print(x$pc_plotall())
  invisible(x)
}

