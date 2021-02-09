#' Convert webmorph stimlist to array (borrowed from webmorph::tems_to_array)
#'
#' @param stimlist Object of class webmorph_list
#'
#' @return Returns 3D array containing template files
#'
#' @author Lisa DeBruine
#'
convertStimlistToArray <- function(stimlist) {
  # check number of points
  n_pts <- lapply(stimlist, `[[`, "points") %>%
    sapply(ncol) %>%
    unique()

  if (length(n_pts) > 1) {
    stop("Each tem must have the same length")
  }

  sapply(stimlist, function(tem) {
    t(tem$points * c(1, -1))
  }) %>%
    array(dim = c(n_pts, 2, length(stimlist)),
          dimnames = list(NULL, c("X", "Y"), names(stimlist)))
}
