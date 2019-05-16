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
#' @param dists list of bseds or bsds
#' @param dist_type 'bsed', 'bsd', 'both' ('both' not yet functional)
#'
#' @return 9 panel plot of bseds/bsds/both modeled after Ernest 2005 Figure 1
#' @export
#'
plot_paper_dists <- function(dists, dist_type){
  dists_plots <- list()
  
  if(dist_type == 'bsed') {dist_plot_fun = plot_bsed}
  if(dist_type == 'bsd') {dist_plot_fun = plot_bsd}
  if(dist_type == 'bsed_bootstraps') {dist_plot_fun = plot_bsed_bootstrap_results}
  
  for(i in 1:length(dists)) {
    dists_plots[[i]] <- dist_plot_fun(dists[[i]], names(dists)[i])
    names(dists_plots)[i] <- names(dists)[i]
  }
  
  dists_plot <- gridExtra::grid.arrange(dists_plots$andrews, dists_plots$niwot,  dists_plots$`sev-goatdraw`,
                                        dists_plots$`sev-5pgrass`,  dists_plots$`sev-rsgrass`,  dists_plots$`sev-two22`,
                                        dists_plots$`sev-5plarrea`,  dists_plots$`sev-rslarrea`,  dists_plots$portal, nrow = 3)
  
  return(dists_plot)
  
}
#' Plot a single BSD
#'
#' @param bsd body size distribution table
#' @param bsd_name community name
#'
#' @return density plot of avg. size x nspeices
#' @export
plot_bsd <- function(bsd, bsd_name = NULL){

  if(is.null(bsd)) {
    bsd_title = 'BSD'
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

#' Plot $E_D$ histogram
#'
#' @param energetic_dominances either a single data table of $E_D$ or a list of data tables. If a list, will be combined into a single data table.  
#'
#' @return histogram of $E_D$ values
#' @export
#'
plot_e_dom <- function(e_doms) {
  if(is.list(e_doms)) {
    e_doms = dplyr::bind_rows(e_doms, .id = "column_label")
  }

  e_dom_plot <- ggplot2::ggplot(data = e_doms, ggplot2::aes(e_dominance, xmin = 0, xmax = 1)) +
    ggplot2::geom_histogram(data = e_doms, stat = "bin", 
                            binwidth = 0.1,
                            show.legend = NA,
                          inherit.aes = TRUE) +
    ggplot2::labs(x = "Energetic dominance", y = "Number of modes", title = "Energetic Dominance") +
    ggplot2::theme_bw()
  
  return(e_dom_plot)
  
}