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

#' @title Replicate Figure 1
#' @description plot all 9 BSEDs or BSDs in the same arrangement as Ernest Figure 1
#' @param dists list of bseds or bsds, or P values for BSED bootstrap analysis
#' @param dist_type "bsed", "bsd", or "bsed_bootstraps"
#' @return 9 panel plot modeled after Ernest 2005 Figure 1
#' @export
#'
plot_paper_dists <- function(dists, dist_type){
  dists_plots <- list()
  
  if(dist_type == "bsed") {dist_plot_fun = plot_bsed}
  if(dist_type == "bsd") {dist_plot_fun = plot_bsd}
  if(dist_type == "bsed_bootstraps") {dist_plot_fun = plot_bsed_bootstrap_results}
  
  for(i in 1:length(dists)) {
    dists_plots[[i]] <- dist_plot_fun(dists[[i]], names(dists)[i])
    names(dists_plots)[i] <- names(dists)[i]
  }
  
  dists_plot <- gridExtra::grid.arrange(dists_plots$andrews, dists_plots$niwot,  dists_plots$`sev-goatdraw`,
                                        dists_plots$`sev-5pgrass`,  dists_plots$`sev-rsgrass`,  dists_plots$`sev-two22`,
                                        dists_plots$`sev-5plarrea`,  dists_plots$`sev-rslarrea`,  dists_plots$portal, nrow = 3)
  
  return(dists_plot)
  
}

#' @title Plot energetic dominance
#' @description Generate histogram of energetic dominance values
#' @param e_doms either a single data table of $E_D$ or a list of data tables. If a list, will be combined into a single data table.  
#' @return histogram of $E_D$ values
#' @export
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

#' Plot a single BSED bootstrapping result
#' @param bootstrap_dois list of empirical DOI and sampled DOIs
#' @param bsed_name community name, optional
#' @return histogram of bootstrapped DOI values, line for empirical value. 
#' @export
plot_bsed_bootstrap_results <- function(bootstrap_dois, bsed_name = NULL){
  
  if(is.null(bsed_name)) {
    if(length(bootstrap_dois) == 3) {
      bsed_title = paste0('Bootstrapped DOI: ', bootstrap_dois$community_names[1], ' compared to ', bootstrap_dois$community_names[2])
    } else { 
      bsed_title = 'BSED bootstrap results'
    }
  } else {
    bsed_title = paste0(bsed_name, " DOI: Bootstrap v. empirical")
  }
  
  sim_values = as.data.frame(bootstrap_dois$sampled_dois)
  colnames(sim_values) = 'value'
  empirical_value = bootstrap_dois$empirical_doi
  
  sim_plot <- ggplot2::ggplot(data = sim_values, ggplot2::aes(value, xmin = 0, xmax = 2)) +
    ggplot2::geom_histogram(data = sim_values, stat = 'bin', bins = 100) +
    ggplot2::geom_vline(data = sim_values, xintercept = empirical_value, color = 'red') + 
    ggplot2::labs(x = "DOI", y = "Sim frequency", title = bsed_title) +
    ggplot2::theme_bw()
  
  return(sim_plot)
  
}

#' Plot cross community comparisons in one panel
#'
#' @param cross_community_comparisons all comparisons
#'
#' @return comparison plots in a single plot
#' @export
plot_crosscomm_bseds <- function(cross_community_comparisons) {
  all_plots = lapply(cross_community_comparisons, FUN = plot_bsed_bootstrap_results)
  
  plots_arranged <- gridExtra::grid.arrange(grobs = all_plots, ncol = 3)
  
  return(plots_arranged)
  
}

#' Plot histogram of p values
#'
#' @param bootstrap_dois list of dois for empirical, sampled distributions
#' @return histogram of p values
#' @export
plot_bootstrap_pvals <- function(bootstrap_dois) {
  
  all_ps <- lapply(bootstrap_dois, FUN = calculate_bootstrap_p) %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::rename(pval = ".")
  
  pval_hist = ggplot2::ggplot(data = all_ps, ggplot2::aes(pval, xmin = 0, xmax = 1)) +
    ggplot2::geom_histogram(data = all_ps, stat = 'bin', 
                            binwidth = 0.05,
                            show.legend = NA,
                            inherit.aes = TRUE) +
    ggplot2::geom_vline(data = all_ps, xintercept = 0.05, color = 'red') + 
    ggplot2::labs(x = "P values", y = "Frequency", title = "P values for pairwise community compairons") +
    ggplot2::theme_bw()
  return(pval_hist)
  
}

#' Plot comparison of Ernest vs current KS test p values
#' @param crosscomm_result result of compare_ernest_ks_values
#' @return histogram of Ernest and current p values
#' @export
plot_crosscomm_ks_pvals <- function(results_comparison) {
  
  results_comparison = results_comparison %>%
    dplyr::rename(replication = p_value, ernest = ernest_p_val)
  
  
  rep_pval_hist = ggplot2::ggplot(data = results_comparison, ggplot2::aes(replication, xmin = 0, xmax = 1)) +
    ggplot2::geom_histogram(data = results_comparison, stat = 'bin', 
                            binwidth = 0.05,
                            show.legend = NA,
                            inherit.aes = TRUE) +
    ggplot2::geom_vline(data = results_comparison, xintercept = 0.05, color = 'red') + 
    ggplot2::labs(x = "P values", y = "Frequency", title = "Replication P values for pairwise community comparisons") +
    ggplot2::theme_bw()
  
  ernest_pval_hist = ggplot2::ggplot(data = results_comparison, ggplot2::aes(ernest, xmin = 0, xmax = 1)) +
    ggplot2::geom_histogram(data = results_comparison, stat = 'bin', 
                            binwidth = 0.05,
                            show.legend = NA,
                            inherit.aes = TRUE) +
    ggplot2::geom_vline(data = results_comparison, xintercept = 0.05, color = 'red') + 
    ggplot2::labs(x = "P values", y = "Frequency", title = "Ernest 2005 P values for pairwise community comparisons") +
    ggplot2::theme_bw()
  
  pval_hists = gridExtra::grid.arrange(grobs = list(rep_pval_hist, ernest_pval_hist), ncol = 2)
  
  return(pval_hists)
  
}
