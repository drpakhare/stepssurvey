#' WHO STEPS colour palette
#'
#' A named list of colours used in WHO STEPS reports and visualisations.
#'
#' @return A named list of hex colour codes.
#' @export
#' @examples
#' steps_colors()$blue
steps_colors <- function() {
  list(
    blue       = "#009ADE",
    dark_blue  = "#00427A",
    green      = "#7AC143",
    orange     = "#F26522",
    red        = "#ED1C24",
    grey       = "#6D6E71",
    light_grey = "#D1D3D4",
    male       = "#009ADE",
    female     = "#F26522"
  )
}

#' WHO STEPS ggplot2 theme
#'
#' A clean, minimal ggplot2 theme styled with WHO STEPS colours.
#'
#' @param base_size Base font size (default 11).
#' @return A [ggplot2::theme] object.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_steps()
theme_steps <- function(base_size = 11) {
  colors <- steps_colors()
  ggplot2::theme_minimal(base_size = base_size) %+replace%
    ggplot2::theme(
      plot.title       = ggplot2::element_text(color = colors$dark_blue, face = "bold", size = base_size + 2),
      plot.subtitle    = ggplot2::element_text(color = colors$grey, size = base_size),
      axis.title       = ggplot2::element_text(color = colors$grey, size = base_size - 1),
      axis.text        = ggplot2::element_text(color = colors$grey),
      panel.grid.major = ggplot2::element_line(color = colors$light_grey, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold", color = colors$dark_blue)
    )
}
