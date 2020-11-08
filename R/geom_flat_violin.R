#'geom_flat_violin_HELPER1
#'
#'@format NULL
#'@usage NULL
#'@name gfv_helper1
#'@import ggplot2
#'@keywords internal
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#'geom_flat_violin_HELPER2
#'
#'@format NULL
#'@usage NULL
#'@name gfv_helper2
#'@keywords internal
GeomFlatViolin <- ggplot2::ggproto(
  "GeomFlatViolin",
  ggplot2::Geom,
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    data %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(
        ymin = min(y),
        ymax = max(y),
        xmin = x,
        xmax = x + width / 2
      )

  },

  draw_group = function(data, panel_scales, coord) {
    # Find the points for the line to go all the way around
    data <- transform(data,
                      xminv = x,
                      xmaxv = x + violinwidth * (xmax - x))

    # Make sure it's sorted properly to draw the outline
    newdata <-
      rbind(plyr::arrange(transform(data, x = xminv), y),
            plyr::arrange(transform(data, x = xmaxv),-y))

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1, ])

    ggplot2:::ggname("geom_flat_violin",
                     ggplot2::GeomPolygon$draw_panel(newdata, panel_scales, coord))
  },

  draw_key = ggplot2::draw_key_polygon,

  default_aes = ggplot2::aes(
    weight = 1,
    colour = "grey20",
    fill = "white",
    size = 0.5,
    alpha = NA,
    linetype = "solid"
  ),

  required_aes = c("x", "y")
)


#' Flat violin geom by Ben Marwick
#'
#' Create flat violin plot with ggplot2 with geom_flat_violin. Shamelessly copy-pasted from \href{https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d}{Ben Marwick}.
#'
#' @param mapping A value
#' @param data A value
#' @param position A value
#' @param show.legend A value
#' @param inherit.aes A value
#' @param stat A value
#' @param trim A value
#' @param scale A value
#' @param ... A value
#'
#' @examples
#'
#'ggplot2::ggplot(ggplot2::diamonds, ggplot2::aes(cut, carat)) +
#'  ggplot2::coord_flip() +
#'  geom_flat_violin()
#'
#' @export

geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim,
                    scale = scale,
                    ...)
    )
  }
