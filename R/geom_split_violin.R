#' geom_split_violin_HELPER
#'
#' @format NULL
#' @usage NULL
#' @name gsv_helper
#' @keywords internal

GeomSplitViolin <- ggplot2::ggproto("GeomSplitViolin", ggplot2::GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1,'group']
  newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x'])
  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                              1))
    quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- ggplot2::GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grid::grobTree(ggplot2::GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", ggplot2::GeomPolygon$draw_panel(newdata, ...))
  }
})

#' Split violin geom
#'
#' Create split violin plot with ggplot2 with geom_split_violin. Shamelessly copy-pasted from \href{https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot/}{jan-glx at Stack Overflow}.
#'
#' @param stat A value
#' @param draw_quantiles A value
#' @param trim A value
#' @param scale A value
#' @param na.rm A value
#'
#' @examples
#'
#'set.seed(123)
#'
#' my_data = tibble::tibble(
#'  x=c(rep('a', 200), rep('b', 200)),
#'  y=c(rnorm(100), rnorm(100, 0.5), rnorm(100, 1), rnorm(100, 1.5)),
#'  cond=c(rep('i', 100), rep('j', 200), rep('i', 100))
#' )
#'
#' ggplot2::ggplot(my_data, ggplot2::aes(x, y, fill=cond)) +
#' geom_split_violin()
#'
#'
#' @inheritParams ggplot2::stat_identity
#' @export
#'
geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}
