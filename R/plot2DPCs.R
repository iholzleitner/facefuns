#' Plot 2D PCs
#'
#' Convenience function for plotting shape PCs based on \code{geomorph::plotRefToTarget()}. Other than \code{plotRefToTarget}, magnitude of visualized difference in shape between reference face at average and low/high levels of the respective PC is based on standard deviations (instead of range).
#'
#' @param pca_output Output from \code{gm.prcomp}
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
#' plot2DPCs(pca_output, ref)
#'
plot2DPCs <- function (pca_output, ref, which_pcs = 1, vis_sd = 3){

  # For their own good, limit how much data can be entered
  if (length(which_pcs) > 5) {
    stop("This function will only plot up to 5 PCs per plot. You can choose a subset of PCs with the which_pcs argument.")
  }
  # TODO: check if PCs availabe

  shapes_list <- make2DPCs(pca_output, ref, which_pcs, vis_sd)

  # Nifty hack from stackoverflow user cartwheel: store plots as functions (https://stackoverflow.com/a/46961131/6401207)

  # one line that took me 6 hours.
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
#' set up specs for plotting pairs of PCs
#'
#' @param f1 function for first plot
#' @param f2 function for second plot
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
