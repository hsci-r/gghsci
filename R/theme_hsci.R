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

#' HSCI ggplot theme with gdocs discrete palette
#' @export
#' @inheritParams theme_hsci
#' @seealso \code{\link{theme_hsci}}
theme_hsci_discrete_gdocs <- function(base_size=12, base_family="Helvetica") {
  list(theme_hsci(base_size,base_family),ggthemes::scale_color_gdocs(),ggthemes::scale_fill_gdocs())
}

#' HSCI ggplot theme with viridis discrete palette
#' @export
#' @inheritParams theme_hsci
#' @param palette virids palette to use
#' @seealso \code{\link{theme_hsci}}
theme_hsci_discrete_viridis <- function(base_size=12, base_family="Helvetica",palette="viridis") {
  list(theme_hsci(base_size,base_family),ggplot2::scale_color_viridis_d(option=palette),ggplot2::scale_fill_viridis_d(option=palette))
}

#' HSCI ggplot theme with viridis continuous palette
#' @export
#' @inheritParams theme_hsci
#' @param palette virids palette to use
#' @seealso \code{\link{theme_hsci}}
theme_hsci_continuous_viridis <- function(base_size=12, base_family="Helvetica",palette="viridis") {
  list(theme_hsci(base_size,base_family),ggplot2::scale_color_viridis_c(option=palette),ggplot2::scale_fill_viridis_c(option=palette))
}
