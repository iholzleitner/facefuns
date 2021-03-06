#' Quick start to shape analyses of 2-D data
#'
#' @description
#' \lifecycle{maturing}
#'
#' Performs some of the routine steps for getting landmark data ready for shape analyses, such as Procrustes alignment and principal component analysis. For more details see vignette:
#' \code{vignette("intro", package = "facefuns")}
#'
#' @param data Three-dimensional array of dimensions p, k, and n. p = number of landmarks, k = dimension (2D or 3D), n = number of specimens
#' @param remove_points Specify any points/lamdmarks you want to remove. See \link[facefuns]{remove_points} and \link[facefuns]{frl_features}
#' @param pc_criterion Criterion used to choose which PCs to retain. See \link[facefuns]{select_pcs}
#' @param plot_sample Plot sample to check data. See \link[geomorph]{plotAllSpecimens}
#' @param auto_rotate Landmark templates are sometimes no longer upright after Procrustes-alignment. Auto-rotate uses \link[geomorph]{rotate.coords} to guess which type of rotation is required
#' @param quiet Print short summary of loaded data
#'
#' @return Returns a list of the following components:
#' \item{aligned}{Three-dimensional array containing Procrustes-aligned data}
#' \item{average}{Coordinates of sample average for plotting}
#' \item{pc_info}{List of selected PCs (including their SD, variance explained and cumulative variance explained), number of selected PCs, criterion used to select PCs}
#' \item{pc_scores}{Principal component scores}
#' \item{pc_plot}{PCs for plotting. Will by default create list of coordinates for all selected PCs at +/- 3SDs. To create plots of other PCs or at different level of SD, please see \link[facefuns]{plot_2dpcs}}
#' \item{summary}{Short summary of key descriptives}
#'
#' @keywords internal
#'
facefuns2d <- function (data, remove_points = NULL, pc_criterion = "broken_stick", plot_sample = TRUE,  auto_rotate = TRUE, quiet = FALSE) {

  check_data <- data

  # CHECK DATA ----
  if (!is_shape_array(check_data)){
    stop("Your data could not be read. Check its dimensions")
  }

  # REMOVE POINTS ----
  if (!is.null(remove_points)){
    data <- remove_points(check_data,
                              points = remove_points,
                              relabel_points = TRUE,
                              plot = FALSE, quiet = TRUE)

    old_p <- dim(check_data)[[1]]
    new_p <- dim(data)[[1]]
    diff_p <- old_p - new_p

    message_rmp <- paste0("You removed ", diff_p, " landmarks prior to analyses.")

  } else{
    data <- check_data
    message_rmp <- paste0("All landmarks were used in analyses.")
  }

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
  # calculate PD between original and Procrustes-aligned and rotated averages to figure out which (if any) rotation is needed
  check_rot <- array(data = numeric(),
                     dim = c(dim(data)[[1]], dim(data)[[2]], 4))

  rtype <- c(NA, "rotateC", "rotateCC", "flipY")
  check_rot[,, 1] <- apply(gpa$coords, c(1, 2), mean)
  check_rot[,, 2] <- geomorph::rotate.coords(apply(gpa$coords, c(1, 2), mean), type = rtype[2])
  check_rot[,, 3] <- geomorph::rotate.coords(apply(gpa$coords, c(1, 2), mean), type = rtype[3])
  check_rot[,, 4] <- geomorph::rotate.coords(apply(gpa$coords, c(1, 2), mean), type = rtype[4])

  rot_min <- sapply(seq_len(dim(check_rot)[[3]]),
                    function(i) sqrt(sum((geomorph::mshape(data) - check_rot[,, i])**2))
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

  # PLOT ----
  plotSample <- function() {geomorph::plotAllSpecimens(data_aligned)}

  if (plot_sample == TRUE ) {
    plotSample() %>% cat()
  }


  # PCA ----
  # Came across a mysterious error: "Error in La.svd(x, nu, nv): error code 1 from Lapack routine 'dgesdd'"
  # The following tries to catch and work around this error: solved the issue in the data set I encountered it with, but whether that's a reliable fix is unclear!
  pca_output <- tryCatch({
    geomorph::gm.prcomp(data_aligned)
  }, error = function(e) {
    message(paste0(e, " - The R gods seem upset with you, and sent you this message. You should be able to safely ignore it, but do check your output!"))

    # Converting data to matrix, reshuffling rows, and THEN running PCA
    matrix <- convert_array_to_matrix(data_aligned)
    rand <- sample(nrow(matrix))
    shuffled <- matrix[rand,]

    return(stats::prcomp(shuffled))
  })

  # select PCs
  pc_sel <- select_pcs(pca_output = pca_output, method = pc_criterion)

  # save scores
  data_scores <- pca_output$x[, 1:pc_sel$n] %>%
    as.data.frame() %>%
    # tidy colnames
    dplyr::rename_with(~make_id(n = pc_sel$n, "PC"))

  # MAKE 2D PCS ----
  ref <- geomorph::mshape(data_aligned)
  vis_pcs <- make_pcs(pca_output = pca_output, ref = ref, which_pcs = 1:pc_sel$n, vis_sd = 3)


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

  if (quiet == FALSE) {
    cat(paste0(message_rmp, "\n",
               "The loaded data set contains ", summary$data_n, " specimen, delineated with ", summary$data_lm, " ", summary$data_dim,"-D landmarks.", "\n",
               "The ", summary$pc_crit, " criterion was used to select ", summary$pc_nPCs, " principal components.", "\n",
               message_rot))}


  # RETURN----
  to_return <- invisible(list(
    aligned = data_aligned,
    average = ref,
    pc_info = pc_sel,
    pc_scores = data_scores,
    pc_plot = vis_pcs,
    summary = summary
  ))

  class(to_return) <- c("facefuns_obj", "list")
  return(to_return)

}

#' Print for facefuns_obj
#'
#' @param x a list of class facefuns_obj
#' @param ... arguments passed to or from other methods
#'
#' @return prints summary
#' @keywords internal
#'
print.facefuns_obj <- function(x, ...) {
  invisible(x)
}
