#' Read landmark data
#'
#' @param lmdata Landmark data as webmorph_list, tps file or tem file(s)
#' @param specID If reading TPS file, specify specID (see \code{geomorphs}'s \link[geomorph]{readland.tps})
#' @param remove_points If reading tem file(s), specify if any points should be removed
#' @param path_to_tps If reading temlist or tem file(s), specify if you want to save data as TPS file
#' @param plot Plot all specimens to check data (see \code{geomorphs}'s \link[geomorph]{plotAllSpecimens})
#'
#' @return Returns three-dimensional array of dimensions p, k, and n. p = number of landmarks, k = dimension (2D or 3D), n = number of specimens.
#'
#'
#' @export
#' @examples
#' path_to_tem <- system.file("extdata", "tem", package="facefuns")
#' remove_points <- c(45:50, 100:104, 116:125, 146:158, 159:164, 165:170, 171:174, 175:179, 184:185)
#'
#' data <- read_lmdata(lmdata =  path_to_tem, remove_points = remove_points)
#'
read_lmdata <- function(lmdata, specID = "None", remove_points = NA, path_to_tps = NA, plot = TRUE){

  if (class(lmdata) == "webmorph_list") {
  # Check if path is webmorph_list
    data <- webmorph::tems_to_array(lmdata)

    if (!is.na(path_to_tps)){
      webmorph::write_tps(lmdata, path_to_tps = path_to_tps)
    }


  } else if (file.exists(lmdata) && grepl("\\.tps$", lmdata)) {
    # Check if path is tps file
    data <- geomorph::readland.tps(file = lmdata,
                                   specID = specID,
                                   warnmsg = FALSE)

  } else if (((dir.exists(lmdata) & grepl("\\.tem$", list.files(lmdata)[1])) ||
              file.exists(lmdata) & grepl("\\.tem$", lmdata))){
    # Check if path is directory containing tems or a single tem file
    data <- convertTEMtoTPS(path_to_tem = lmdata,
                            remove_points = remove_points,
                            path_to_tps = path_to_tps)

  } else {
    stop("MEHP! Your data is neither of format webmorph_list, TPS nor tem.")
  }

  if (plot) {
    geomorph::plotAllSpecimens(data)
  }

  return(data)

}
