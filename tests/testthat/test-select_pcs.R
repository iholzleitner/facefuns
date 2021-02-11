test_that("errors", {
  expect_error(select_pcs(), "argument \"pca_output\" is missing, with no default")

  expect_error(suppressWarnings(select_pcs(list())),
               "pca_output must be a prcomp object")
})

test_that("basic works", {
  # unnamed arguments
  pca_output <- geomorph::gm.prcomp(LondonSet_aligned)
  london_pcs <- select_pcs(pca_output)

  expect_equal(names(london_pcs), c("selected", "n", "method"))

  # with named arguments
  london_pcs_named <- select_pcs(pca_output = pca_output)
  expect_equal(london_pcs, london_pcs_named)

  #skip("takes too long")
  #testthat::skip_on_cran()

  expect_equal(london_pcs$n, 8)

  expect_equal(london_pcs$selected$SD[[2]], 0.02557, tol = .001)

  expect_equal(london_pcs$method, "broken_stick")

  expect_equal(
    rownames(london_pcs$selected),
    paste0("PC", 1:8)
  )

  expect_equal(
    colnames(london_pcs$selected),
    c("SD", "Variance", "Cum Var")
  )

   #x <- capture.output(london_pcs)
   #expect_equal(trimws(x[[2]]), "SD Variance Cum Var")

  # error when PCA_output doesn't have sdev item
  expect_error(select_pcs(data_aligned), ".*")
})
