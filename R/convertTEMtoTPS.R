#' Convert Webmorph template files to .tps format
#'
#' For more details see the corresponding vignette:
#' \code{vignette("TEMtoTPS", package = "facefuns")}
#'
#' @param path_to_tem Path to directory containing template files
#' @param p Number of landmarks images were delineated with
#' @param remove_points Vector containing number of landmarks to be deleted. If no value is entered, all landmarks are retained
#' @param path_to_tps Name and path of TPS file to be created
#'
#' @export

convertTEMtoTPS <- function (path_to_tem, p, remove_points = NA, path_to_tps) {

  files <- list.files(path_to_tem, pattern="*.tem", full.names = TRUE)

  for(i in 1:length(files)){

    raw <- scan(files[i], character(0), sep = "\n", skip = 1, nlines = p, quiet = TRUE)

    if (!is.na(remove_points[1])) {
      red <- raw[-remove_points]
    } else {
      red <- raw
    }

    # Split each line into separate elements for x and y coords
    rep <- strsplit(red, "\t", fixed = TRUE) %>%
      unlist() %>%
      as.numeric()

    flip <- tibble::tibble(
      # Convert to table
      x = rep[seq(1, length(rep), by = 2 )],
      y = rep[seq(2, length(rep), by = 2 )]
    ) %>%
      # Flip templates upside down
      dplyr::mutate(
        y = .data$y * -1
      ) %>%
      # And convert back to text
      tidyr::unite(t, c(.data$x, .data$y), sep = "\t") %>%
      dplyr::pull()

    newLM <- length(flip)

    # add first line with new number of LMs, and last line with image ID
    temp <- c(paste0("LM=", newLM), flip, paste0("ID=", basename(files[i])))

    # concatenate landmark files
    if (i == 1) {
      data <- temp
    } else {
      data <- c(data, temp)
    }

  }
  writeLines(data, con = path_to_tps, sep = "\n", useBytes = FALSE)
  #return(data)
}
