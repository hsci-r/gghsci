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

#' convert a palette to grayscale
#' @export
#' @param palette palette to convert
convert_palette_to_grayscale <- function(palette) {
  function(n) {
    c(.3,.59,.11) %*% grDevices::col2rgb(palette(n)) %>%
      round() %>%
      purrr::map_chr(~rgb(.,.,.,maxColorValue=255))
  }
}

#' grayscale version of the discrete viridis palette
#' @export
#' @inheritParams ggplot2::scale_colour_viridis_d
#' @seealso \code{\link[ggplot2:scale_colour_viridis_d]{ggplot2::scale_colour_viridis_d}}
scale_viridis_d_grayscale <- function(..., alpha = 1, begin = 0, end = 1,
                                      direction = 1, option = "D", aesthetics = c("colour","fill")) {
  ggplot2::discrete_scale(
    aesthetics,
    "viridis_d",
    convert_palette_to_grayscale(scales::viridis_pal(alpha, begin, end, direction, option)),
    ...
  )
}

#' grayscale version of the continuous viridis palette
#' @export
#' @inheritParams ggplot2::scale_colour_viridis_c
#' @seealso \code{\link[ggplot2:scale_colour_viridis_c]{ggplot2::scale_colour_viridis_c}}
scale_viridis_c_grayscale <- function(..., alpha = 1, begin = 0, end = 1, direction = 1, option = "D", values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = c("colour","fill")) {
  ggplot2::continuous_scale(
    aesthetics,
    "viridis_c",
    scales::gradient_n_pal(
      convert_palette_to_grayscale(scales::viridis_pal(alpha, begin, end, direction, option))(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' HSCI ggplot theme with grayscale viridis discrete palette
#' @export
#' @inheritParams theme_hsci_discrete_viridis
#' @seealso \code{\link{theme_hsci}}
theme_hsci_discrete_viridis_grayscale <- function(base_size=12, base_family="Helvetica",palette="viridis") {
  list(theme_hsci(base_size,base_family),ggplot2::scale_color_viridis_d(option=palette),scale_viridis_d_grayscale(option=palette))
}

#' HSCI ggplot theme with grayscale viridis continuous palette
#' @export
#' @inheritParams theme_hsci_continuous_viridis
#' @param palette virids palette to use
#' @seealso \code{\link{theme_hsci}}
theme_hsci_continuous_viridis_grayscale <- function(base_size=12, base_family="Helvetica",palette="viridis") {
  list(theme_hsci(base_size,base_family),ggplot2::scale_color_viridis_c(option=palette),scale_viridis_c_grayscale(option=palette))
}

#' Save plots to PNG + SVG files
#' @param dir directory to save the plots to
#' @param basename the base name for the plot files
#' @param plot the plot to save
#' @param width plot width
#' @param height plot height
#' @param units the units in which width & height are measured
#' @param dpi plot resolution (dots per inch)
save_plots <- function(dir,basename,plot,width=6,height=5,units="in",dpi=300) {
  ggplot2::ggsave(paste0(dir,"/",basename,".png"),plot,width=width,height=height,dpi=dpi)
  ggplot2::ggsave(paste0(dir,"/",basename,".svg"),plot,width=width,height=height,dpi=dpi)
}
