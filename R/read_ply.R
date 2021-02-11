#' Read vertices from binary PLY files
#'
#'@description
#' \lifecycle{experimental}
#'
#' Uses \link[Morpho]{file2mesh} to read binary_little_endian PLY files and saves vertex data into three-dimensional array
#'
#' @param plydata Path to single PLY or directory containing PLY files
#'
#' @return Returns three-dimensional array of dimensions p, k, and n. p = number of landmarks, k = dimension (3D, as opposed to 2D), n = number of specimens
#' @export
#'
#' @examples
#'
#' path_to_ply <- system.file("extdata", "ply", package="facefuns")
#' data <- read_ply(plydata = path_to_ply)
#'
read_ply <- function(plydata){

  if ((dir.exists(plydata) & grepl("\\.ply$", list.files(plydata)[1])) ||
      file.exists(plydata) & grepl("\\.ply$", plydata)) {

    # GET PATH ----
    if (dir.exists(plydata)) {
      files <- list.files(path = plydata,
                          pattern = "\\.ply$",
                          full.names = TRUE)
    } else if (file.exists(plydata)) {
      files <- plydata
    }

    ids <- gsub("\\.ply$", "", basename(files))

    # READ PLYs into list ----
    mylist <- lapply(seq_len(length(files)), function(i) {
      # Import PLYs
      mesh <- Morpho::file2mesh(files[i])
      # Extract vertices
      vert <- t(mesh$vb[1:3, ]) # Is there a reason why I would keep line 4 (all 1s)? read.ply does!
      return(vert)
    })

    # CONVERT LIST TO ARRAY ----
    k <- dim(mylist[[1]])[1]
    d <- 3
    n <- length(files)

    myarray <- array(as.numeric(unlist(mylist)),
                     dim = c(k, d, n),
                     dimnames = list(NULL, NULL, ids))

    return(myarray)

  } else{

    stop("MEHP! Your data can't be read.")

  }
}
