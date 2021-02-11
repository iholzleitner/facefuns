data(LondonSet_scores)
data(LondonSet_info)

data <- LondonSet_scores %>%
  as.matrix()

fem <- LondonSet_scores %>%
  dplyr::filter(row.names(LondonSet_scores) %in% LondonSet_info$face_id[which(LondonSet_info$face_sex == "female")]) %>%
  as.matrix()

mal <- LondonSet_scores %>%
  dplyr::filter(row.names(LondonSet_scores) %in% LondonSet_info$face_id[which(LondonSet_info$face_sex == "male")]) %>%
  as.matrix()


test_that("basics", {
  # all perfect
  sexdim <- calc_vs(data, fem, mal)
  expect_equal(names(sexdim), c("id", "VS"))
  expect_equal(dim(sexdim), c(102, 2))

  # Data only one line
  data_onerow <- LondonSet_scores %>% dplyr::slice_head()
  sexdim <- calc_vs(data_onerow, fem, mal)
  expect_equal(round(sexdim$VS[[1]], 3), -1.062)

  # Anchors only one line (as they would be if averages were entered)
  fem_avg <- t(apply(fem, 2, mean))
  mal_avg <- t(apply(mal, 2, mean))
  sexdim <- calc_vs(data, fem_avg, mal_avg)
  expect_equal(dim(sexdim), c(102, 2))
  expect_equal(round(sexdim$VS[[1]], 3), -1.062)
})

test_that("errors", {

  # anchors in wrong format (rows = PCs/col = specimen)
  fem_avg <- apply(fem, 2, mean)
  mal_avg <- apply(mal, 2, mean)
  expect_error(calc_vs(data, fem_avg, mal_avg), "All three input files must have the same number of PCs/columns")

})
