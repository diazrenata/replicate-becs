#' Plot a single BSED bootstrapping result
#'
#' @param bsed_bootstrap_results output of `community_bootstrap(..., bootstrap_function = 'bootstrap_unif_bsed_doi', ...)`
#' @param bsed_name community name
#'
#' @return histogram of bootstrapped DOI values, line for empirical value, and P-value. 
#' @export
plot_bsed_bootstrap_results <- function(bsed_bootstrap_results, bsed_name = NULL){
  
  if(is.null(bsed_name)) {
    if(length(bsed_bootstrap_results) == 3) {
      bsed_title = paste0('Bootstrapped DOI: ', bsed_bootstrap_results$community_names[1], ' compared to ', bsed_bootstrap_results$community_names[2])
    } else { 
      bsed_title = 'BSED bootstrap results'
    }
  } else {
    bsed_title = paste0(bsed_name, " DOI: Bootstrap v. empirical")
  }
  
  sim_values = as.data.frame(bsed_bootstrap_results$bootstrap_values)
  colnames(sim_values) = 'value'
  empirical_value = bsed_bootstrap_results$empirical_value
  
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
#' @param bootstrap_results results objects
#'
#' @return histogram of p values
#' @export
plot_bootstrap_pvals <- function(bootstrap_result) {
  
  all_ps <- lapply(bootstrap_result, FUN = get_bootstrap_p) %>%
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