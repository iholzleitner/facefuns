## code to prepare `DATASET` dataset goes here

## LondonSet_data ----

remove_points <- c(45:50, 100:104, 116:125, 146:158, 159:164,
                   165:170, 171:174, 175:179, 184:185)

convertTEMtoTPS(path_to_tem = "inst/extdata/tem/",
                remove_points = remove_points,
                path_to_tps = "inst/extdata/LondonSet.tps")

LondonSet_data <- read_shapedata(shapedata = "inst/extdata/tem/",
                                 remove_points = remove_points,
                                 specID = "ID",
                                 plot = FALSE)

usethis::use_data(LondonSet_data, overwrite = TRUE)


### LondonSet_aligned ----

gpa <- geomorph::gpagen(LondonSet_data, print.progress = FALSE)
LondonSet_aligned <- geomorph::rotate.coords(gpa$coords,
                                   type = "rotateC")

dimnames(LondonSet_aligned) <- list(dimnames(gpa$coords)[[1]],
                                    dimnames(gpa$coords)[[2]],
                                    dimnames(gpa$coords)[[3]])

usethis::use_data(LondonSet_aligned, overwrite = TRUE)

## LondonSet_scores ----

LondonSet_scores <- geomorph::gm.prcomp(LondonSet_aligned) %>%
  `[[`("x") %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~make_id(102, "PC")) %>%
  # re-add IDs
  tibble::add_column(.before = 1, id = dimnames(LondonSet_aligned)[[3]])

usethis::use_data(LondonSet_scores, overwrite = TRUE)

