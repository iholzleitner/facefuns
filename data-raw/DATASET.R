## code to prepare `DATASET` dataset goes here

## LondonSet_data ----
remove_points <- c(45:50, 100:104, 116:125, 146:158, 159:164,
                   165:170, 171:174, 175:179, 184:185)

convertTEMtoTPS(path_to_tem = "inst/extdata/tem/",
                remove_points = remove_points,
                path_to_tps = "inst/extdata/LondonSet.tps")

LondonSet_data <- read_lmdata(lmdata = "inst/extdata/tem/",
                                 remove_points = remove_points,
                                 specID = "ID",
                                 plot = FALSE)

usethis::use_data(LondonSet_data, overwrite = TRUE)


### LondonSet_aligned ----
gpa <- geomorph::gpagen(LondonSet_data, print.progress = FALSE)
dimnames(gpa$coords)[[3]] <- gsub("^ID=", "", dimnames(gpa$coords)[[3]])

LondonSet_aligned <- geomorph::rotate.coords(gpa$coords,
                                   type = "rotateC")

dimnames(LondonSet_aligned) <- dimnames(gpa$coords)

usethis::use_data(LondonSet_aligned, overwrite = TRUE)

## LondonSet_scores ----
pc_sel <- selectPCs(geomorph::gm.prcomp(LondonSet_aligned))

LondonSet_scores <- geomorph::gm.prcomp(LondonSet_aligned) %>%
  `[[`("x") %>%
  tibble::as_tibble() %>%
  # only keep selected PCs
  dplyr::select(1:pc_sel$n) %>%
  # tidy colnames
  dplyr::rename_with(~make_id(pc_sel$n, "PC")) %>%
  # re-add IDs
  tibble::add_column(.before = 1, id = dimnames(LondonSet_aligned)[[3]]) %>%
  tibble::column_to_rownames(var = "id")

usethis::use_data(LondonSet_scores, overwrite = TRUE)


## LondonSet_info ----
LondonSet_info <- readr::read_csv(file = "inst/extdata/LondonSet_info.csv", col_types = readr::cols()) %>%
  dplyr::mutate(face_id = substr(face_id, start = 2, stop = 4))

usethis::use_data(LondonSet_info, overwrite = TRUE)


## Mirrrored landmarks ----
mirroredlandmarks <- as.matrix(utils::read.table("inst/extdata/mirr_lms.txt",
                                 header = F))

usethis::use_data(mirroredlandmarks, overwrite = TRUE)
