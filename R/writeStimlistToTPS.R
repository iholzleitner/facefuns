#' Write webmorph stimlist to TPS (borrowed from webmorph::write_tps)
#'
#' @param stimlist Object of class webmorph_list
#' @param path_to_tps Optional file name of TPS file
#'
#' @return Returns webmorph_list in TPS format
#' @author Lisa DeBruine
#'
writeStimlistToTPS <- function(stimlist, path_to_tps = NULL) {

  tps <- mapply(function(stim, name) {
    pt <- {stim$points * c(1, -1)} %>%
      t() %>% as.data.frame()

    pt_list <- paste(pt[[1]], pt[[2]], sep = "\t") %>%
      paste(collapse = "\n")

    sprintf("LM=%i\n%s\nID=%s",
            ncol(stim$points),
            pt_list,
            name)
  }, stimlist, names(stimlist) %||% seq_along(stimlist)) %>%
    paste(collapse = "\n")

  if (is.null(path_to_tps)) {
    return(tps)
  } else {
    write(tps, path_to_tps)
    invisible(stimlist)
  }
}
