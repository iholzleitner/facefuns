#' Read Webmorph template files
#'
#' Reads Webmorph templates and converts them to TPS. For more details see the corresponding vignette:
#' \code{vignette("TEMtoTPS", package = "facefuns")}
#'
#' @param path_to_tem Path to directory containing template files
#' @param remove_points Vector containing number of landmarks to be deleted. If no value is entered, all landmarks are retained
#' @param path_to_tps Name and path of TPS file to be created
#'
#' @export

convert_tem_to_tps <- function (path_to_tem, remove_points = NA, path_to_tps = NA) {

  # GET PATH ----
  if (dir.exists(path_to_tem)) {
    temfiles <- list.files(path = path_to_tem,
                           pattern = "\\.tem$",
                           full.names = TRUE)
  } else if (file.exists(path_to_tem)) {
    temfiles <- path_to_tem
  }

  # PROCESS EACH TEMFILE ----
  temlist <- lapply(temfiles, function(temfile) {
    # read and clean  ----
    tem_txt <- readLines(temfile[1]) %>%
      trimws() %>%
      `[`(. != "") %>% # get rid of blank lines
      `[`(substr(., 1, 1) != "#") # get rid of comments

    # extract points ----
    npoints <- as.integer(tem_txt[[1]])
    points <- tem_txt[2:(npoints+1)] %>%
      strsplit("\t", fixed = TRUE) %>%
      sapply(as.numeric)

    # flip templates upside down ----
    points <- points * c(1, -1)

    # remove points ----
    if (!is.na(remove_points[1])) {
      points <- t(points)[-remove_points,] %>% t()
    }

    # get template name ----
    name <- paste0("ID=", sub("\\.tem$", "", basename(temfile)))

    # create list
    tem <- list(
      name = name,
      points = points)

  })

  # CHECK EACH TEM HAS SAME NUMBER OF POINTS ----
  n_pts <- lapply(temlist, `[[`, "points") %>%
    sapply(ncol) %>%
    unique()

  if (length(n_pts) > 1) {
    stop("Each template file must have the same number of landmarks")
  }

  # CONVERT TO ARRAY ----
  n_names <- sapply(temlist, `[[`, "name")
  p_names <- 1:n_pts
  k_names <- c("X", "Y")

  tem_array <- sapply(temlist, function(tem) {
    t(tem$points)}) %>%
    array(dim = c(n_pts, 2, length(temlist)),
          dimnames = list(p_names, k_names, n_names))

  # WRITE TPS FILE ----
  if (!is.na(path_to_tps)) {
    tps <- lapply(temlist, function(tem) {
      pt <- tem$points %>%
        t() %>%
        as.data.frame()

      pt_list <- paste(pt[[1]], pt[[2]], sep = "\t") %>%
        paste(collapse = "\n")

      sprintf("LM=%i\n%s\n%s",
              ncol(tem$points),
              pt_list,
              tem$name)
    }) %>%
      paste(collapse = "\n")

    write(tps, path_to_tps)
  }

  tem_array

}
