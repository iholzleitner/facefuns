#' Read vertices from PLY or OBJ files
#'
#' @description
#' \lifecycle{experimental}
#'
#' Uses \link[Morpho]{file2mesh} to read PLY files or \link{readOBJ} to read OBJ files, and saves vertex data into three-dimensional array
#'
#' @param meshdata Path to single PLY/OBJ or directory containing PLY/OBJ files. If both files types are present, PLYs will be prioritised
#'
#' @return Returns array of dimensions p, k, and n. p = number of landmarks, k = dimension of data, n = number of specimens
#' @export
#'
#' @examples
#' 
#' path_to_ply <- system.file("extdata", "ply", package="facefuns")
#' data <- read_vertices(meshdata = path_to_ply)
#' 
#' path_to_obj <- system.file("extdata", "obj", package="facefuns")
#' data <- read_vertices(meshdata = path_to_obj)
#' 
read_vertices <- function(meshdata) {
  
  # an if statement that's not for the faint-hearted... BRACKET ALERT
  if ((dir.exists(meshdata) & (any(grepl("\\.ply$", list.files(meshdata))) || any(grepl("\\.obj$", list.files(meshdata))) || any(grepl("\\.wavefront$", list.files(meshdata))))) ||
      (file.exists(meshdata) & (grepl("\\.ply$", meshdata) || grepl("\\.obj$", meshdata) || grepl("\\.wavefront$", meshdata)))) {
    
    # GET PATH ----
    if (dir.exists(meshdata) & grepl("\\.ply$", list.files(meshdata)[1])) {
      files <- list.files(path = meshdata,
                          pattern = "\\.ply$",
                          full.names = TRUE)
    } else if (dir.exists(meshdata) & any(grepl("\\.obj$", list.files(meshdata)))) {
      files <- list.files(path = meshdata,
                          pattern = "\\.obj$",
                          full.names = TRUE)
    } else if (dir.exists(meshdata) & any(grepl("\\.wavefront$", list.files(meshdata)))) {
      files <- list.files(path = meshdata,
                          pattern = "\\.wavefront$",
                          full.names = TRUE)
    } else if (file.exists(meshdata)) {
      files <- meshdata
    }
    
    if (any(grepl("\\.ply$", files))) {
      ids <- gsub("\\.ply$", "", basename(files))
      # READ PLYs into list ----
      mylist <- lapply(seq_len(length(files)), function(i) {
        # Import PLYs
        mesh <- Morpho::file2mesh(files[i])
        # Extract vertices
        # Is there a reason why I would keep line 4 (all 1s; read.ply keeps it)?
        # EDIT: "homogenous coordinates", an rgl thing - if something goes wrong, come back and look here :)
        vert <- t(mesh$vb[1:3, ])
        
        return(vert)
      })
    } else if (any(grepl("\\.obj$", files)) || any(grepl("\\.wavefront$", files))) {
      
      if (any(grepl("\\.obj$", files))) {
        ids <- gsub("\\.obj$", "", basename(files))
      } else {
        ids <- gsub("\\.wavefront$", "", basename(files))
      }
      
      # READ OBJs into list ----
      mylist <- lapply(seq_len(length(files)), function(i) {
        # Import PLYs
        mesh <- rgl::readOBJ(files[i])
        # Extract vertices
        vert <- t(mesh$vb[1:3, ])
        
        return(vert)
      })
    }
    
    
    # CONVERT LIST TO ARRAY ----
    p <- dim(mylist[[1]])[1]
    k <- 3
    n <- length(files)
    
    p_names <- 1:p
    k_names <- c("X", "Y", "Z")
    n_names <- ids
    
    myarray <- array(as.numeric(unlist(mylist)),
                     dim = c(p, k, n),
                     dimnames = list(p_names, k_names, n_names))
    
    return(myarray)
    
  } else{
    
    stop("MEHP! Your data can't be read.")
    
  }
}
