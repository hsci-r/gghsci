# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' HSCI ggplot theme
#' @export
#' @inheritParams ggplot2::theme_light
#' @seealso \code{\link{ggplot}}
theme_hsci <- function(base_size=12, base_family="sans") {
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

coloropt_normal6 = c("#4053D3", "#DDB310", "#B51D14", "#00BEFF", "#FB49B0", "#00B25D", "#CACACA")
coloropt_bright6 = c("#EFE645", "#E935A1", "#00E3FF", "#E1562C", "#537EFF", "#00CB85", "#EEEEEE")
coloropt_dark6 = c("#005900", "#000078", "#490D00", "#8A034F", "#005A8A", "#443500", "#585858")
coloropt_fancy6 = c("#56641A", "#C0AFFB", "#E6A176", "#00678A", "#984464", "#5ECCAB", "#CDCDCD")
coloropt_tarnish6 = c("#274D52", "#C7A2A6", "#818B70", "#604E3C", "#8C9FB7", "#796880", "#C0C0C0")
coloropt_normal12 = c("#EBAC23", "#B80058", "#008CF9", "#006E00", "#00BBAD", "#D163E6", "#B24502", "#FF9287", "#5954D6", "#00C6F8", "#878500", "#00A76C", "#BDBDBD")

get_coloropt_pal <- function(option="normal",n) {
  if (option=="normal" & n>6)
    coloropt_normal12
  else switch (option,
    "normal" = coloropt_normal6,
    "bright" = coloropt_bright6,
    "dark" = coloropt_dark6,
    "fancy" = coloropt_fancy6,
    "tarnish" = coloropt_tarnish6,
    stop("Requested unknown palette")
  )
}

#' coloropt palette
#' @param option coloropt palette from normal, bright, dark, fancy and tarnish
#' @export
coloropt_pal <- function(option="normal") {
  function(n) {
    values = get_coloropt_pal(option,n)
    n_values <- length(values)
    if (n > n_values) {
      warning("This palette returns a maximum of ",
              n_values, " distinct values. You have requested ", n, ".",
              call. = FALSE)
    }
    rep_len(values,n)
  }
}

#' extract the coloropt final neutral gray value for different palettes
#' @param option coloropt palette from normal, bright, dark, fancy and tarnish
#' @param n the number of colors required from the palette (max 13 for normal, 7 for the other palettes)
#' @export
coloropt_na_value <- function(n,option="normal") {
  values <- get_coloropt_pal(option,n)
  values[length(values)]
}

#' coloropt discrete color/fill scale
#' @inheritParams ggplot2::scale_colour_hue
#' @param option coloropt palette from normal, bright, dark, fancy and tarnish
#' @export
scale_coloropt <- function(..., option = "normal", aesthetics = c("colour","fill")) {
  ggplot2::discrete_scale(
    aesthetics,
    "coloropt",
    coloropt_pal(option),
    na.value=coloropt_na_value(6,option),
    ...
  )
}

#' HSCI ggplot theme with a discrete coloropt palette.
#' @export
#' @inheritParams theme_hsci
#' @param palette coloropt palette from normal, bright, dark, fancy and tarnish
#' @seealso \code{\link{theme_hsci}}
theme_hsci_discrete <- function(base_size=12, base_family="sans",palette="normal") {
  list(theme_hsci(base_size,base_family),scale_coloropt(option=palette))
}

#' HSCI ggplot theme with viridis continuous palette
#' @export
#' @inheritParams theme_hsci
#' @param palette viridis palette to use. Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
#' @seealso \code{\link{theme_hsci}}
theme_hsci_continuous <- function(base_size=12, base_family="sans",palette="viridis") {
  list(theme_hsci(base_size,base_family),ggplot2::scale_color_viridis_c(option=palette),ggplot2::scale_fill_viridis_c(option=palette))
}

#srgb_to_linear <- Vectorize(function(csrgb) {
#  if (csrgb < 0.04045) csrgb/12.92 else ((csrgb+0.055)/1.055)^2.4
#})

#linear_to_srgb <- Vectorize(function(cl) {
#  if (cl<=0.0031308) 12.92*cl else 1.055*cl^(1/2.4) - 0.055
#})

#' convert a palette to grayscale
#' @export
#' @param palette palette to convert
#' @importFrom rlang .data
convert_palette_to_grayscale <- function(palette) {
  function(n) {
    colors <- dplyr::as_tibble(t(grDevices::col2rgb(palette(n))))/255
    #colors_linear <- colors %>% dplyr::transmute_all(srgb_to_linear)
    yl <- colors %>% dplyr::transmute(yl=.2126*.data$red+.7152*.data$green+.0722*.data$blue)
    #yl_srgb <- yl_linear %>% dplyr::transmute_all(linear_to_srgb)
    (yl %>% dplyr::transmute(color=grDevices::rgb(.data$yl,.data$yl,.data$yl,maxColorValue=1)))$color
  }
}

#' grayscale version of the continuous viridis palette
#' @export
#' @inheritParams ggplot2::scale_colour_viridis_c
#' @seealso \code{\link[ggplot2:scale_colour_viridis_c]{ggplot2::scale_colour_viridis_c}}
scale_viridis_c_grayscale <- function(..., alpha = 1, begin = 0, end = 1, direction = 1, option = "D", values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = c("colour","fill")) {
  ggplot2::continuous_scale(
    aesthetics,
    "viridis_c_grayscale",
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

#' grayscale version of the coloropt palette
#' @inheritParams scale_coloropt
#' @export
scale_coloropt_grayscale <- function(..., option = "normal", aesthetics = c("colour","fill")) {
  ggplot2::discrete_scale(
    aesthetics,
    "coloropt_grayscale",
    convert_palette_to_grayscale(coloropt_pal(option)),
    ...
  )
}

#' HSCI ggplot theme with a grayscale coloropt discrete palette.
#' @export
#' @inheritParams theme_hsci_discrete
#' @seealso \code{\link{theme_hsci}}
theme_hsci_discrete_grayscale <- function(base_size=12, base_family="sans",palette="normal") {
  list(theme_hsci(base_size,base_family),scale_coloropt_grayscale(option=palette))
}

#' HSCI ggplot theme with grayscale viridis continuous palette
#' @export
#' @inheritParams theme_hsci_continuous
#' @seealso \code{\link{theme_hsci}}
theme_hsci_continuous_grayscale <- function(base_size=12, base_family="sans",palette="viridis") {
  list(theme_hsci(base_size,base_family),scale_viridis_c_grayscale(option=palette))
}

