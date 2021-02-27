#' Read landmark data
#'
#' @param lmdata Path to single file or directory containing landmark data. Accepted file formats are Webmorph template files, a TPS file or a Webmorph list
#' @param specID If reading TPS file, specify specID (see \code{geomorphs}'s \link[geomorph]{readland.tps})
#' @param remove_points If reading TEM files, you can specify vector of points you want removed
#' @param path_to_tps If reading TEM files or a Webmorph list, specify if you want to save data as TPS file
#' @param plot Plot all specimens to check data (see \code{geomorphs}'s \link[geomorph]{plotAllSpecimens})
#'
#' @return Returns three-dimensional array of dimensions p, k, and n. p = number of landmarks, k = dimensionality of data (2D or 3D), n = number of specimens
#'
#'
#' @export
#' @examples
#' path_to_tem <- system.file("extdata", "tem", package="facefuns")
#' data <- read_lmdata(lmdata =  path_to_tem)
#'
#' # NOTE: You can also remove points on reading in your landmark data by specifying their indices
#' # If you are using Webmorph labels to decide which points to remove,
#' # add +1 as Webmorph is 0-based (in R, the index of Webmorph point "0 - left pupil" is 1)
#' remove_points <- c(44:49, 99:103, 115:124, 145:157, 158:163, 164:169, 170:173, 174:178, 183:184) + 1
#' data <- read_lmdata(lmdata =  path_to_tem, remove_points = remove_points)
#'
read_lmdata <- function(lmdata, specID = "None", remove_points = NA, path_to_tps = NA, plot = TRUE){

  if (class(lmdata) == "webmorph_list") {
  # Check if path is webmorph_list
    data <- convert_stimlist_to_array(lmdata)

    if (!is.na(path_to_tps)){
      write_stimlist_to_tps(lmdata, path_to_tps = path_to_tps)
    }


  } else if (file.exists(lmdata) && grepl("\\.tps$", lmdata)) {
    # Check if path is tps file
    data <- geomorph::readland.tps(file = lmdata,
                                   specID = specID,
                                   warnmsg = FALSE)

  } else if (((dir.exists(lmdata) & grepl("\\.tem$", list.files(lmdata)[1])) ||
              file.exists(lmdata) & grepl("\\.tem$", lmdata))){
    # Check if path is directory containing tems or a single tem file
    data <- convert_tem_to_tps(path_to_tem = lmdata,
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
