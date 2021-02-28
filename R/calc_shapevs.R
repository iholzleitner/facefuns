#' Calculate vector scores from Procrustes-aligned landmark templates
#'
#' Function projects data onto vector from anchor one (score of 0) to anchor two (score of 1). See \link[facefuns]{calc_vs}
#'
#' @param data Output from \link[facefuns]{facefuns}
#' @param anchor1_index Vector specifying indices of faces which will constitute lower anchor point
#' @param anchor2_index Vector specifying indices of faces which will constitute upper anchor point
#' @param symm Symmetrize templates prior to calculating vector scores
#' @param mirroredlandmarks Vector specifiying order of mirrored landmarks (required for symmetrizing)
#'
#' @return Returns tibble with columns "id" and "VS". If data contained rownames, these will be saved as ids.
#'
#'@export
#' @examples
#'
#' # READ AND PREP DATA
#' path_to_tem <- system.file("extdata", "tem", package="facefuns")
#'
#' shapedata <- facefuns(data = read_lmdata(lmdata = path_to_tem,
#'                                          plot = FALSE),
#'                       remove_points = "frlgmm",
#'                       plot_sample = FALSE,
#'                       quiet = TRUE)
#'
#' # CREATE ANCHORS, e.g. female and male averages
#' data("LondonSet_info")
#'
#' # Specify indices of faces that will constitute male and female averages
#' fem_i <- gsub("^ID=","", dimnames(shapedata$aligned)[[3]]) %in%
#' LondonSet_info$face_id[which(LondonSet_info$face_sex == "female")]
#'
#' mal_i <- gsub("^ID=", "", dimnames(shapedata$aligned)[[3]]) %in%
#' LondonSet_info$face_id[which(LondonSet_info$face_sex == "male")]
#'
#' # CALCULATE FEMALE-MALE VECTOR SCORES
#' calc_shapevs(shapedata, fem_i, mal_i)
#'
#'
#' # CALCULATE FEMALE-MALE VECTOR SCORES FROM SYMMETRIZED FACES
#' # To symmetrize faces before calculating vector scores,
#' # you will have to provide indices of landmarks after mirroring
#'
#' data("mirroredlandmarks")
#'
#' calc_shapevs(shapedata, fem_i, mal_i,
#'              symm = TRUE, mirroredlandmarks = mirroredlandmarks)
#'
calc_shapevs <- function(data, anchor1_index, anchor2_index, symm = FALSE, mirroredlandmarks){

  # ONLY WORKS FOR WITHIN-SET SCORES; I.E. PCA OF DATA AND ANCHOR FACES COMBINED

  # Check data is of class facefuns
  if (!any(class(data) == "facefuns_obj")) {
    stop("This function only takes facefuns objects as input. Maybe try calc_vs?")
  }

  input <- data

  if (symm == TRUE) {

    # SYMMETRIZE
    data <- symm_templates(input$aligned, mirroredlandmarks)

    # GPA
    gpa <- geomorph::gpagen(data, print.progress = FALSE)
    data_aligned <- gpa$coords
    if (!is.na(input$summary$data_r)){
      data_aligned <- geomorph::rotate.coords(data_aligned, type = input$summary$data_r)
      dimnames(data_aligned) <- dimnames(gpa$coords)}

    # PCA
    pca_output <- geomorph::gm.prcomp(data_aligned)
    pc_sel <- select_pcs(pca_output = pca_output, method = input$summary$pc_crit)

    data_scores <- pca_output$x[, 1:pc_sel$n] %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(~make_id(n = pc_sel$n, "PC")) %>%
      tibble::add_column(.before = 1,
                         id = gsub("^ID=", "", dimnames(data_aligned)[[3]])) %>%
      tibble::column_to_rownames(var = "id")

  } else {
    data_scores <- input$pc_scores
  }

  data_matrix <- as.matrix(data_scores)

  # DEFINE ANCHORS
  anchor1 <- data_scores %>%
    dplyr::filter(anchor1_index) %>%
    as.matrix()

  anchor2 <- data_scores %>%
    dplyr::filter(anchor2_index) %>%
    as.matrix()

  # calc_vs
  calc_vs(data_matrix, anchor1, anchor2)

}
