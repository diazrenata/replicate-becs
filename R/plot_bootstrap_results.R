#' Plot a single BSED
#'
#' @param bsed_bootstrap_results output of `community_bootstrap(..., bootstrap_function = 'bootstrap_unif_bsed_doi', ...)`
#' @param bsed_name community name
#'
#' @return histogram of bootstrapped DOI values, line for empirical value, and P-value. 
#' @export
plot_bsed_bootstrap_results <- function(bsed_bootstrap_results, bsed_name){
  
  if(is.null(bsed_name)) {
    bsed_title = 'BSED bootstrap results'
  } else { 
    bsed_title = paste0(bsed_name, " DOI: Bootstrap v. empirical")
  }
  
  sim_values = as.data.frame(bsed_bootstrap_results$bootstrap_values)
  colnames(sim_values) = 'value'
  empirical_value = bsed_bootstrap_results$empirical_value
  p_value = get_bootstrap_p(bsed_bootstrap_results)
  
  sim_plot <- ggplot2::ggplot(data = sim_values, ggplot2::aes(value, xmin = 0, xmax = 2)) +
    ggplot2::geom_histogram(data = sim_values, stat = 'bin', bins = 100) +
    ggplot2::geom_vline(data = sim_values, xintercept = empirical_value, color = 'red') + 
    # ggplot2::annotate(geom = 'text', x = empirical_value - .25, y = 20, label = "Empirical DOI") +
    # ggplot2::annotate(geom = 'text', x = empirical_value - .25, y = 18, label = paste0('p-value: ', p_value)) +
    ggplot2::labs(x = "DOI", y = "Sim frequency", title = bsed_title) +
    ggplot2::theme_bw()
  
  return(sim_plot)
  
}
