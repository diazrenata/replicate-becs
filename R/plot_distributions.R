# plot_bsd <- function(bsd){
#   # will involve kernel smoothing and stuff.
# }

#' Plot a single BSED
#'
#' @param bsed body size energy distribution table
#' @param bsed_name community name
#'
#' @return barplot of proportion energy use by size class
#' @export
plot_bsed <- function(bsed, bsed_name = NULL){
  
  if(is.null(bsed_name)) {
    bsed_title = 'BSED'
  } else { 
    bsed_title = paste0(bsed_name, " BSED")
  }
  
  bsed <- bsed %>%
    dplyr::mutate(size_class_g = round(size_class_g, digits = 1))
  
  bsed_plot <- ggplot2::ggplot(data = bsed, ggplot2::aes(x = size_class, y = total_energy_proportional)) +
    ggplot2::scale_x_discrete(limits = c(0.6, 2, 3, 4, 4.8), labels = c(2.7, 7.4, 20.1, 54.6, 121.5)) +
    ggplot2::scale_y_continuous(limits = c(0, 0.4)) +
    ggplot2::geom_bar(stat = 'identity', ggplot2::aes(x = bsed$size_class, y = bsed$total_energy_proportional)) +
    ggplot2::ggtitle(bsed_title) +
    ggplot2::theme_bw()
  
  return(bsed_plot)
  
}

#' Replicate Figure 1
#'
#' @param bseds list of bseds
#'
#' @return 9 panel plot of bseds modeled after Ernest 2005 Figure 1
#' @export
#'
plot_paper_bseds <- function(bseds){
  bsed_plots <- list()
  for(i in 1:length(bseds)) {
    bsed_plots[[i]] <- plot_bsed(bseds[[i]], bsed_name = names(bseds)[i])
    names(bsed_plots)[i] <- names(bseds)[i]
  }
  
  bseds_plot <- gridExtra::grid.arrange(bsed_plots$andrews, bsed_plots$niwot,  bsed_plots$`sev-5pgrass`,
                                        bsed_plots$`sev-5plarrea`,  bsed_plots$`sev-goatdraw`,  bsed_plots$`sev-rsgrass`,
                                        bsed_plots$`sev-rslarrea`,  bsed_plots$`sev-two22`,  bsed_plots$portal, nrow = 3)
  
  return(bseds_plot)
  
}