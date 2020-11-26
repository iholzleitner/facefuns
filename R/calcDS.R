#' Calculate linear discriminant scores
#'
#' Function uses stepwise LDA to choose best predictors for group membership, and then "regular" LDA to predict discriminant scores.
#'
#' @param data Matrix or data frame containing PC scores of specimen (rows = specimen, cols = PCs)
#' @param group_info Numeric index of column in data that indicates actual group membership
#' @param acc Print accuracy of group membership prediction
#'
#' @details First uses \link[klaR]{greedy.wilks} to perform stepwise forward variable selection for classification, then uses selected variables in \link[MASS]{lda}
#'
#' @return Returns tibble with columns "id" and "discrimScore". If data contained rownames, these will be saved as ids. If acc = TRUE, output will be list, with tibble "ds" containing id and vector score, and list "acc" containing accuracy info.
#' @export
#'
calcDS <- function (data, group_info = 1, acc = FALSE) {

  # STEPWISE LDA -----
  # Create formula for stepwise LDA
  group <- colnames(data[group_info])
  pcs <- colnames(data)[grepl("^PC", colnames(data))]

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
    discrimScore = as.vector(discrimScore)
  )


  if (acc == TRUE) {
  # PRINT ACCURACY ----
  lda <- MASS::lda(eval(lda_form),
                   data = data,
                   na.action = "na.omit",
                   CV = TRUE)

  acc_bygroup <- diag(prop.table(table(data[, group_info], lda$class), 1))
  acc_total <- sum(diag(prop.table(table(data[, group_info], lda$class))))

  acc <- list(bygroup = acc_bygroup,
              total = acc_total)

  to_return <- list(
    ds = ds,
    acc = acc
  )

  class(to_return) <- "ds_out"

  } else {
    to_return <- ds
  }

  return(to_return)

}

#' Print for ds_out
#'
#' @param x a list of class ds_out
#' @param ... arguments passed to or from other methods
#'
#' @return prints ds_out
#' @export
#'
print.ds_out<- function(x, ...) {
  print(x$acc)
  invisible(x)
}
