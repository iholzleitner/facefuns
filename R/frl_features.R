#' Get point indices for FRL standard template features
#'
#' @description
#' \lifecycle{experimental}
#'
#' Available features in alphabetical order: cheekbones, chin, ears, face, halo, left_brow, left_eye, lowerlip, mouth, neck1, neck2, nose, philtrum, right_brow, right_eye, smilelines, undereyes. You can also use "frlgmm" to choose features used in \href{https://doi.org/10.1037/xhp0000685}{Holzleitner et al., 2019}
#'
#' @param ...  Vector of feature names
#'
#' @return Vector of corresponding FRL template indices
#' @export
#'
#' @author Lisa DeBruine
#'
#' @examples
#' frl_features("mouth")
#' frl_features("left_eye", "right_eye")
#' frl_features("frlgmm")
#'
frl_features <- function(...) {
  # 0-based for compatibility with webmorph
  # keep consistent with frl_sym()

  named_features <- list(...) %>% unlist()

  features <- list(
    # frlgmm
    undereyes = c(44:49),
    lowerlip = c(99:103),
    ears = c(115:124),
    halo = c(145:157),
    smilelines = c(158:163),
    cheekbones = c(164:169),
    philtrum = c(170:173),
    chin = c(174:178),
    neck1 = c(183:184),
    # other features
    neck2 = c(183:184, 144, 156),
    left_eye = c(0, 2:9, 18:22, 28:30, 34:38),
    right_eye = c(1, 10:17, 23:27, 31:33, 39:43),
    left_brow = c(71:76, 83:84),
    right_brow = c(77:82, 85:86),
    nose = c(50:70, 170, 172, 179:182),
    mouth = c(87:108),
    face = c(109:114, 125:144, 185:188)
  )

  if ("frlgmm" %in% named_features) {
    named_features <- c(
      named_features, "undereyes", "lowerlip", "ears", "halo",
      "smilelines", "cheekbones",
      "philtrum", "chin", "neck1"
    )
  }

  # unavailable features are ignored
  features[named_features] %>%
    unlist() %>% unname() %>%
    unique() %>% sort()
}
