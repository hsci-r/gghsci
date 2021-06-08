# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' HSCI ggplot theme
#' @export
#' @inheritParams ggplot2::theme_light
#' @seealso \code{\link{ggplot}}
theme_hsci <- function(base_size=12, base_family="Helvetica") {
  ggplot2::theme_light(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
    axis.ticks.length = ggplot2::unit(0, "cm"),
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(colour = 'black')
  )
}
