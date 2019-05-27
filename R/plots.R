#' @title Plot a single BSED
#' @description Plot one BSED
#' @param bsed body size energy distribution table
#' @param bsed_name community name
#' @return barplot of proportion energy use by size class
#' @export
plot_bsed <- function(bsed, bsed_name = NULL){
  
  if(is.null(bsed_name)) {
    bsed_title = "BSED"
  } else { 
    bsed_title = paste0(bsed_name, " BSED")
  }
  
  bsed <- bsed %>%
    dplyr::mutate(size_class_g = round(size_class_g, digits = 1))
  
  bsed_plot <- ggplot2::ggplot(data = bsed, ggplot2::aes(x = size_class, y = total_energy_proportional)) +
    ggplot2::scale_x_discrete(limits = c(0.6, 2, 3, 4, 4.8), labels = c(2.7, 7.4, 20.1, 54.6, 121.5)) +
    ggplot2::scale_y_continuous(limits = c(0, 0.4)) +
    ggplot2::geom_bar(stat = "identity", ggplot2::aes(x = bsed$size_class, y = bsed$total_energy_proportional)) +
    ggplot2::ggtitle(bsed_title) +
    ggplot2::theme_bw()
  
  return(bsed_plot)
}

#' @title Plot a single BSD
#' @description Plot a single species body size distribution
#' @param bsd body size distribution table
#' @param bsd_name community name
#' @return density plot of avg. size x nspeices
#' @export
plot_bsd <- function(bsd, bsd_name = NULL){
  
  if(is.null(bsd)) {
    bsd_title = "BSD"
  } else {
    bsd_title = paste0(bsd_name, " BSD")
  }
  
  bsd_plot <- ggplot2::ggplot(data = bsd, ggplot2::aes(ln_mass, xmin = 0.6, xmax = 5)) +
    ggplot2::scale_x_discrete(limits = c(0.6, 2, 3, 4, 4.8), labels = c(2.7, 7.4, 20.1, 54.6, 121.5)) +
    ggplot2::geom_density(data = bsd, stat = "density", adjust = 0.5,
                          position = "identity", na.rm = T, show.legend = NA,
                          inherit.aes = TRUE) +
    ggplot2::ggtitle(bsd_title) +
    ggplot2::theme_bw()
  
  
  return(bsd_plot)
  
}