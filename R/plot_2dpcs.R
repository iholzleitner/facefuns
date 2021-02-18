#' Plot 2D PCs
#'
#' Convenience function for plotting shape PCs based on \code{geomorph::plotRefToTarget()}. Other than \code{plotRefToTarget}, magnitude of visualized difference in shape between reference face at average and low/high levels of the respective PC is based on standard deviations (instead of range).
#'
#' @param input Object of class \code{prcomp}, \code{gm.prcomp} or PC_list (see \code{make_pcs})
#' @param ref Reference face (sample average is recommended)
#' @param which_pcs Which PCs are to be created. Single number or vector, maximum allowed is 5
#' @param vis_sd Extent of desired manipulation in units of standard deviation
#'
#' @return Returns plot
#' @export
#'
#' @examples
#' data(LondonSet_aligned)
#' pca_output <- geomorph::gm.prcomp(LondonSet_aligned)
#' ref <- geomorph::mshape(LondonSet_aligned)
#' plot_2dpcs(pca_output, ref)
#'
plot_2dpcs <- function (input, ref, which_pcs = 1:3, vis_sd = 3){

  # LIMIT NUMBER OF PCs ----
  if (length(which_pcs) > 5) {
    stop("This function will only plot up to 5 PCs per plot. You can choose a subset of PCs with the which_pcs argument")
  }

  # CHECK INPUT ----
  if ("sdev" %in% names(input)) {

    shapes_list <- make_pcs(input, ref, which_pcs, vis_sd)

  } else if (is.list(input) && "PC_list" %in% class(input)) {

    pcs <- paste0("PC", which_pcs)
    shapes_list <- input[pcs]

  } else {

    stop("Input cannot be read - check it is output from gm.prcomp/prcomp or list of PCs")

  }

  # CHECK PCs are available
  if (!all(paste0("PC", which_pcs) %in% names(shapes_list))) {
    stop("At least one of the PCs you are trying to plot is out of range")
  }

  # CREATE PLOTS----
  # Nifty hack from stackoverflow user cartwheel: store plots as functions (https://stackoverflow.com/a/46961131/6401207)
  plot_list <- rapply(shapes_list, function(x) {
    function() { geomorph::plotRefToTarget(ref, x) }
  }, how = "list")

  # create pair plot for each PC
  pcs <- lapply(seq_along(plot_list), function (x){
    f1 <- plot_list[[x]]$minus
    f2 <- plot_list[[x]]$plus
    pc <- names(plot_list)[[x]]

    pc_pair_plot(f1, f2, pc)
  })

  # AND, LASTLY... combine plots.
    do.call(cowplot::plot_grid, c(pcs, list(ncol = 1)))
}



#' pc_pair_plot
#'
#' Set up specs for plotting pairs of PCs
#'
#' @param f1 Function for first plot
#' @param f2 Function for second plot
#' @param pc Numeric label
#'
#' @return plot
#'
pc_pair_plot <- function(f1, f2, pc) {
  empty <- ggplot2::ggplot() + ggplot2::theme_void()

  plot <- cowplot::plot_grid(
    empty, f1, f2,
    empty, empty, empty, # empty row to control spacing between rows, see *
    labels = c(pc, "", "",
               "", "", ""),
    label_x = c(0, .4, .4),
    label_y = c(.6, .95, .95),
    ncol = 3,
    rel_widths = c(.2, 1, 1,
                   .2, 1, 1),
    rel_heights = c(1,
                    -.15), # *
    scale = 1.3)

  return(plot)
}
