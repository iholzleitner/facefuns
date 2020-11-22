## code to prepare `DATASET` dataset goes here

## LondonSet_data ----

remove_points <- c(45:50, 100:104, 116:125, 146:158, 159:164,
         165:170, 171:174, 175:179, 184:185)

convertTEMtoTPS(path_to_tem = "inst/extdata/tem/",
                remove_points = remove_points,
                path_to_tps = "inst/extdata/LondonSet.tps")

LondonSet_data <- read_shapedata(shapedata = "inst/extdata/LondonSet.tps",
                     specID = "ID")

usethis::use_data(LondonSet_data, overwrite = TRUE)


### LondonSet_aligned ----

gpa <- gpagen(LondonSet_data, print.progress = FALSE)
LondonSet_aligned <- rotate.coords(gpa$coords, type = "rotateC")
dimnames(LondonSet_aligned)[[3]] <- dimnames(gpa$coords)[[3]]

usethis::use_data(LondonSet_aligned, overwrite = TRUE)

## LondonSet_scores ----

LondonSet_scores <- geomorph::gm.prcomp(LondonSet_aligned) %>%
  `[[`("x") %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~make_id(102, "PC")) %>%
  # re-add IDs
  tibble::add_column(.before = 1, id = dimnames(LondonSet_aligned)[[3]])

usethis::use_data(LondonSet_scores, overwrite = TRUE)

