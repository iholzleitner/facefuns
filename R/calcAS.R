#' Calculate asymmetry
#'
#' Function mirrors templates and runs GPA on original plus mirrored templates. Asymmetry scores are then calculated as Euclidean distance between original template and its symmetrized version (i.e. the mean of original and mirrored template)
#'
#' NOTE: does not distinguish between directional and fluctuating asymmetry
#'
#' @param data Quickstart object or three-dimensional array of dimensions p, k, and n
#' @param mirr_lms Vector specifying order of mirrored landmarks
#'
#' @return Returns tibble containing ID and asymmetry values
#' @export
#'
#' @examples
#' data(LondonSet_aligned)
#' data(mirr_lms)
#' calcAS(LondonSet_aligned, mirr_lms)
#'
calcAS <- function (data, mirr_lms) {

  if (class(data) == "quickstart") {
    org <- data$array
  } else if (is.array(data)) {
    org <- data
  } else {
    stop("Your data is neither a quickstart object nor an array")
  }

  # SET UP
  mirr <- mirrorTemplates(org, mirr_lms)
  n <- dim(org)[[3]]

  # CREATE ARRAY CONTAINING ORIGINAL AND MIRRORED TEMPLATES
  super_array <- array(data = numeric(),
                dim = c(dim(org)[[1]],
                        dim(org)[[2]],
                        n * 2))
  super_array[,, 1:n] <- org
  super_array[,, (n+1):(2*n)] <- mirr

  # CONDUCT GPA
  # NOTE: given that we're just after scores here and correct rotation might differ depending on data set, ignore rotation is off
  gpa <- geomorph::gpagen(super_array, print.progress = FALSE)
  data_aligned <- gpa$coords

  # CALCULATE PROCRUSTES DISTANCE between original template and mshape of original and mirrored template
  # NOTE: re-write without for-loop at some point

  asym_score <- sapply(seq_len(dim(org)[[3]]),
                     function(i) sqrt(sum((data_aligned[,, i] - geomorph::mshape(data_aligned[,, c(i, (i+n))]))**2))
                     )

  asym_table <- tibble::tibble(
    id = dimnames(org)[[3]],
    asym = asym_score)

  return(asym_table)

}
