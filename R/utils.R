#' Save plots to PNG + SVG files
#' @export
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
