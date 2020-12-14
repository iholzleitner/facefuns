#' Quick start to shape analyses
#'
#' @description
#' \lifecycle{maturing}
#'
#' Performs some of the routine steps for getting landmark data ready for shape analyses, such as Procrustes alignment and principal component analysis. For more details see vignette:
#' \code{vignette("intro", package = "facefuns")}
#'
#' @param data Three-dimensional array of dimensions p, k, and n. p = number of landmarks, k = dimension (2D or 3D), n = number of specimens.
#' @param pc_criterion Criterion used to choose which PCs to retain. See \link[facefuns]{selectPCs}
#' @param plot_sample Plot sample to check data. See \link[geomorph]{plotAllSpecimens}
#' @param message Print short summary of loaded data
#' @param auto_rotate Landmark templates are sometimes no longer upright after Procrustes-alignment. Auto-rotate uses \link[geomorph]{rotate.coords} to guess which type of rotation is required.
#'
#' @return Returns a list of the following components:
#' \item{array}{Three-dimensional array containing Procrustes-aligned data}
#' \item{average}{Coordinates of sample average for plotting}
#' \item{pc_info}{List of selected PCs (including their SD, variance explained and cumulative variance explained), number of selected PCs, criterion used to select PCs}
#' \item{pc_scores}{Principal component scores}
#' \item{pc_plot}{PCs for plotting. Will by default create list of coordinates for all selected PCs at +/- 3SDs. To create plots of other PCs or at different level of SD, please see \link[facefuns]{plot2DPCs}}
#' \item{summary}{Short summary of key descriptives}
#'
#' @export
#' @examples
#' path_to_tem <- system.file("extdata", "tem", package="facefuns")
#' remove_points <- c(45:50, 100:104, 116:125, 146:158, 159:164, 165:170, 171:174, 175:179, 184:185)
#'
#' data <- read_lmdata(lmdata = path_to_tem, remove_points = remove_points)
#' shapedata <- quickstart(data = data,
#' pc_criterion = "broken_stick")
#'
#'# Plot sample
#'shapedata$summary$plot_sample() #or geomorph::plotAllSpecimens(shapedata)
#'
#'# Plot PCs
#'plot2DPCs(shapedata$pc_plot, shapedata$average)
quickstart <- function (data, pc_criterion = "broken_stick", plot_sample = TRUE, message = TRUE, auto_rotate = TRUE) {

  # CHECK DATA ----
  # add code to check data is of appropriate dimensions

  # ALIGN DATA ----
  gpa <- geomorph::gpagen(data, print.progress = FALSE)

  # if at least one specimen name contains "ID=", remove it
  if (any(grep("^ID=", dimnames(gpa$coords)[[3]]))) {
    dimnames(gpa$coords)[[3]] <- gsub("^ID=", "", dimnames(gpa$coords)[[3]])
  }

  # pull aligned templates
  data_aligned <- gpa$coords
  dimnames(data_aligned) <- dimnames(gpa$coords)

  # CHECK WHETHER ALIGNED TEMPLATES NEED TO BE ROTATED ----
  # calculate PD between first original, and first Procrustes-aligned template as well as its three rotations and check which one is smallest
  check_rot <- array(data = numeric(),
                     dim = c(dim(data)[[1]],
                             dim(data)[[2]],
                             4))

  rtype <- c(NA, "rotateC", "rotateCC", "flipY")
  check_rot[,, 1] <- gpa$coords[,, 1]
  check_rot[,, 2] <- geomorph::rotate.coords(gpa$coords[,, 1], type = rtype[2])
  check_rot[,, 3] <- geomorph::rotate.coords(gpa$coords[,, 1], type = rtype[3])
  check_rot[,, 4] <- geomorph::rotate.coords(gpa$coords[,, 1], type = rtype[4])

  rot_min <- sapply(seq_len(dim(check_rot)[[3]]),
                    function(i) sqrt(sum((data[,, 1] - check_rot[,, i])**2))
  )

  if (auto_rotate == TRUE && which.min(rot_min) != 1) {
    # rotate
    data_aligned <- geomorph::rotate.coords(data_aligned, type = rtype[which.min(rot_min)])
    dimnames(data_aligned) <- dimnames(gpa$coords)
    # print message saying which rotation was used
    message_rot <- paste0("Templates were rotated using \"", rtype[which.min(rot_min)], "\" after the GPA.")
  } else {
    # print message saying no rotation was used
    message_rot <- paste0("Templates were not rotated after the GPA.")
  }

  # rotate coords ----
  # deleted for now: if the new code works, get rid of entirely
  #  #' @param rotate The type of rotation or flip to be performed (after Procrustes alignment, landmark templates might have to be rotated). Specimens can be flipped with respect to x or y axes, or rotated clockwise or counter-clockwise. See \link[geomorph]{rotate.coords}
  # rotate = c(NA, "flipX", "flipY", "rotateC", "rotateCC"),

  #rotate <- match.arg(rotate)

  #if (!is.na(rotate)){
  #  for (rtype in rotate) {
  #    data_aligned <- geomorph::rotate.coords(data_aligned, type = rtype)
  #    dimnames(data_aligned) <- dimnames(gpa$coords)
  #  }}


  # PLOT ----
  plotSample <- function() {geomorph::plotAllSpecimens(data_aligned)}

  if (plot_sample == TRUE ) {
    plotSample() %>% cat()
  }


  # PCA ----
  pca_output <- geomorph::gm.prcomp(data_aligned)

  # select PCs
  pc_sel <- selectPCs(pca_output = pca_output, method = pc_criterion)

  # save scores
  data_scores <- pca_output$x[, 1:pc_sel$n] %>%
    tibble::as_tibble() %>%
    # tidy colnames
    dplyr::rename_with(~make_id(n = pc_sel$n, "PC")) %>%
    # re-add IDs
    tibble::add_column(.before = 1, id = dimnames(data_aligned)[[3]]) %>%
    tibble::column_to_rownames(var = "id")


  # MAKE 2D PCS ----
  ref <- geomorph::mshape(data_aligned)
  vis_pcs <- make2DPCs(pca_output = pca_output, ref = ref, which_pcs = 1:pc_sel$n, vis_sd = 3)


  # SUMMARY
  summary <- list(
    data_lm = dim(data_aligned)[[1]],
    data_dim = dim(data_aligned)[[2]],
    data_n = dim(data_aligned)[[3]],
    data_r = rtype[which.min(rot_min)],
    pc_crit = pc_sel$method,
    pc_nPCs = pc_sel$n,
    plot_sample = plotSample
  )

  if (message == TRUE) {
    cat(paste0("The loaded data set contains ", summary$data_n, " specimen, delineated with ", summary$data_lm, " ", summary$data_dim,"-D landmarks.", "\n",
               "The ", summary$pc_crit, " criterion was used to select ", summary$pc_nPCs, " principal components.", "\n",
               message_rot))}


  # RETURN----
  to_return <- invisible(list(
    array = data_aligned,
    average = ref,
    pc_info = pc_sel,
    pc_scores = data_scores,
    pc_plot = vis_pcs,
    summary = summary
  ))

  class(to_return) <- "facefuns_obj"
  return(to_return)

}

#' Print for facefuns_obj
#'
#' @param x a list of class quickstart
#' @param ... arguments passed to or from other methods
#'
#' @return prints summary
#' @keywords internal
#'
print.facefuns_obj <- function(x, ...) {
  invisible(x)
}
