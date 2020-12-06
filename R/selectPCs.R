#' Select principal components
#'
#' \code{selectPCs} returns a list of principal components as chosen by one of three methods (from D.A. Jackson (1993). Ecology, 74, 2204-2214).
#'
#' @param pca_output Output from \code{gm.prcomp} or \code{prcomp}
#' @param method Method used to select PCs. Defaults to the most conservative "broken_stick". Other methods are "kaiser_guttman" (PCs with eigenvalues greater than the mean eigenvalue) and "total_variance" (PCs explaining at least \code{total_var} variance)
#' @param total_var Total variance to choose for "total_variance" method. Defaults to .95
#' @return Returns selected PCs and variance explained
#'
#' @note Code for "broken_stick" method adapted from function \href{http://adn.biol.umontreal.ca/~numericalecology/numecolR/}{"evplot" by Francois Gillet}.
#'
#' @export

selectPCs <- function(pca_output, method = c("broken_stick", "kaiser_guttman", "total_variance"), total_var = .95) {

  # check that "prcomp" is in one class name
  # and that the PCA_output has sdev
  if (!(grepl("prcomp", class(pca_output)) %>% any()) ||
      !"sdev" %in% names(pca_output)) {
    stop("pca_output must be a prcomp object")
  }
  method <- match.arg(method)

  ev <- pca_output$sdev^2
  var <- ev / sum(ev)
  n <- length(ev)

  if (method == "broken_stick" | method == "bs") {

    bsm <- data.frame(j = seq(1:n), p = 0)
    bsm$p[1] <- 1/n
    for (i in 2:n) bsm$p[i] <- bsm$p[i - 1] + (1 / (n + 1 - i))
    bsm$p <- 100 * bsm$p / n
    test <- cbind(100 * ev / sum(ev), bsm$p[n:1])
    n_PCs <- sum(test[, 1] >= test[, 2])

  } else if (method == "kaiser_guttman" |  method == "kg") {
    # return PCs with eigenvalues greater than the mean eigenvalue
    n_PCs <- sum(ev >= mean(ev));

  } else if (method == "total_variance" | method == "tv") {
    # return PCs explaining at least total.var variance
    cumvar <- cumsum(var);
    n_PCs <- sum(cumvar < total_var) + 1;
  }

  # Print PCs and variance explained
  selected <- tibble::tibble(
    "SD" = pca_output$sdev,
    "Variance" = var,
    "Cum Var" = cumsum(var),
    "PC" = paste0("PC", seq(1, n, 1))
  ) %>%
    dplyr::slice(1:n_PCs) %>%
    dplyr::mutate(dplyr::across("SD":"Cum Var", ~round(.x, 3))) %>%
    tibble::column_to_rownames(var = "PC")

  to_return <- list(
    selected = selected,
    n = n_PCs,
    method = method
  )

  class(to_return) <- "selected_PCs"

  return(to_return)
}

#' Print for selected_PCs
#'
#' @param x a list of class selected_PCs
#' @param ... arguments passed to or from other methods
#'
#' @return prints the selected PCs
#' @keywords internal
#'
print.selected_PCs <- function(x, ...) {
  print(x$selected)
  invisible(x)
}
