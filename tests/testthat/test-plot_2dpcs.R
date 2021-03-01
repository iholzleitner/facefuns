pca_out <- geomorph::gm.prcomp(LondonSet_aligned)
ref <- geomorph::mshape(LondonSet_aligned)

test_that("warnings", {
  too_many <- "This function will only plot up to 3 PCs per plot. You can choose a subset of PCs with the which_pcs argument"

  expect_error(plot_2dpcs(input = pca_out, ref = ref,  which_pcs = 20:40), too_many)
})

test_that("basic", {
  skip("plotting")

  # defaults (3 plots PC1-3)
  plot1_2_3 <- plot_2dpcs(input = pca_out, ref = ref)

  # 1 plot PC2
  plot2 <- plot_2dpcs(input = pca_out, ref = ref, 2)

  # 2 plots PC1:PC3
  plot3_1 <- plot_2dpcs(input = pca_out, ref = ref, c(3, 1))
})


