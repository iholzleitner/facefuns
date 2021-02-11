#' Calculate linear discriminant scores
#'
#' Function uses stepwise LDA to choose best predictors for group membership, and then "regular" LDA to predict discriminant scores.
#'
#' @param data Matrix or data frame containing PC scores of specimen (rows = specimen, cols = PCs). Can also contain an additional column that defines group membership
#' @param group_info Numeric index of column in data that indicates actual group membership, or a dataframe with two columns (col 1 = id matching the rownames in data; col 2 = group)
#'
#' @details First uses \link[klaR]{greedy.wilks} to perform stepwise forward variable selection for classification, then uses selected variables in \link[MASS]{lda}
#'
#' @return Returns tibble with columns "id" and "DS". If data contained rownames, these will be saved as ids
#' @export
#' @examples
#' data(LondonSet_scores)
#' group_info <- LondonSet_info %>%
#' dplyr::select(face_id, face_sex)
#' calc_ds(data = LondonSet_scores, group_info = group_info)
#'
calc_ds <- function (data, group_info) {

  # CHECK INPUT ----
  if (is.vector(group_info, mode = "numeric") && length(group_info) == 1) {
    # If group_info is single number, assume it is index of column in data containing group info

    group <- colnames(data[group_info])
    pcs <- colnames(data)[grepl("^PC", colnames(data))]

  } else if (tibble::has_rownames(data) &&
             is.data.frame(group_info) &&
             nrow(group_info) == nrow(data)) {
    # If group_info is dataframe, left_join it to PC scores using column 1

    data <- data %>%
      tibble::rownames_to_column("id") %>%
      dplyr::left_join(dplyr::rename_with(group_info, .cols=1, ~"id"), by = "id") %>%
      tibble::column_to_rownames("id")

    group <- colnames(data)[!grepl("^PC", colnames(data))]
    pcs <- colnames(data)[grepl("^PC", colnames(data))]

  } else {
    stop("There is a problem with either the data or the group info you have provided.")
  }

  # STEPWISE LDA -----
  # Create formula for stepwise LDA
  steplda_form <- parse(text = paste0(group, " ~ ", paste(pcs, collapse = " + ")))

  steplda <- klaR::greedy.wilks(eval(steplda_form),
                                data = data)

  # PREDICT DISCRIMINANT SCORES -----
  # Create vector naming PCs retained in stepwise LDA and create new formula
  step_pcs <- steplda$results$vars
  lda_form <- parse(text = paste0(group, " ~ ", paste(step_pcs, collapse = " + ")))

  lda <- MASS::lda(eval(lda_form),
                   data = data,
                   na.action = "na.omit",
                   CV = FALSE)


  lda_pred <- stats::predict(lda, data)
  discrimScore <- lda_pred$x

  # Get ids. If there are none, make some.
  if(is.null(dimnames(discrimScore)[[1]])) {
    id <- seq(1:length(discrimScore))
  } else {
    id <- dimnames(discrimScore)[[1]]
  }

  ds <- tibble::tibble(
    id = id,
    # maybe remove unname again? but it felt untidy to have named vector in tibble
    DS = as.vector(discrimScore)
  )

  return(ds)

}
