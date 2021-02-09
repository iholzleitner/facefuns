data("LondonSet_info")
path_to_tem <- system.file("extdata/tem", package = "facefuns")
path_to_tps <- tempfile(fileext = ".tps")
path_to_fail <- system.file("extdata", "LondonSet_info.csv", package = "facefuns")
tempdata <- convertTEMtoTPS(path_to_tem = path_to_tem,
                            path_to_tps = path_to_tps)

test_that("different_inputs", {

  # tps
  test_tps <- read_lmdata(lmdata = path_to_tps)
  expect_equal(
    dim(test_tps),
    c(189,  2, 102))

  # tems
  test_tem <- read_lmdata(lmdata = path_to_tem)
  expect_equal(
    dim(test_tem),
    c(189,  2, 102))

  expect_error(read_lmdata(lmdata = path_to_fail), "MEHP! Your data is neither of format webmorph_list, TPS nor tem.")

})
