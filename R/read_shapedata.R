#' Read shape data
#'
#' @param shapedata Temlist, tps file or tem file(s)
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
#' data <- read_shapedata(shapedata =  path_to_tem, remove_points = remove_points)
#'
read_shapedata <- function(shapedata, specID = "None", remove_points = NA, path_to_tps = NA, plot = TRUE){

  if (class(shapedata) == "webmorph_list" && is.na(path_to_tps)) {

    data <- webmorph::tems_to_array(shapedata)

  } else if (class(shapedata) == "webmorph_list" && !is.na(path_to_tps)) {

    data <- webmorph::tems_to_array(shapedata)
    webmorph::write_tps(shapedata, path_to_tps = path_to_tps)

  } else if (file.exists(shapedata) && grepl("\\.tps$", shapedata)) {

    data <- geomorph::readland.tps(file = shapedata,
                                   specID = specID,
                                   warnmsg = FALSE)

  } else if (!class(shapedata) == "webmorph_list" & ((dir.exists(shapedata) & grepl("\\.tem$", list.files(shapedata)[1])) | file.exists(shapedata) & grepl("\\.tem$", shapedata))){

    data <- convertTEMtoTPS(path_to_tem = shapedata, remove_points = remove_points, path_to_tps = path_to_tps)

  } else {
    warning("MEHP! Your data is neither of format webmorph_list, TPS nor tem.")
  }

  if (plot) {
    geomorph::plotAllSpecimens(data)
  }

  data
}
