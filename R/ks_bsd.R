#' Two sample KS test for BSDs
#'
#' @param community_dfs list of dfs and names
#' @param ln_mass_vals whether to use the ln masses, defaults FALSE
#'
#' @return outcome list of ks_test, names
#' @export
ks_bsd <- function(community_dfs, ln_mass_vals = FALSE){
  
  community_a = community_dfs$community_a
  community_b = community_dfs$community_b
  
  
  bsd_a = community_a %>%
    make_community_table() %>%
    make_bsd()
  
  bsd_b = community_b %>%
    make_community_table() %>%
    make_bsd()
  
  if(ln_mass_vals) {
    ks_result = stats::ks.test(bsd_a$ln_mass, bsd_b$ln_mass)
  } else {
    ks_result = stats::ks.test(bsd_a$species_mean_mass,
                        bsd_b$species_mean_mass)
  }
  
  community_names = list(community_a = community_dfs$community_names[1],
                         community_b = community_dfs$community_names[2])
  
  outcome = list(ks_result = ks_result, community_names = community_names)
  
  return(outcome)
  
}

#' Extract values from two sample ks test results
#'
#' Helper function.
#'
#' @param this_test a test result
#' @param val_name name of the value to pull
#'
#' @return requested value
#' @export

extract_values_tsks <- function(this_test, val_name) {
  
  if(val_name %in% c('community_a', 'community_b')) {
    this_val = this_test$community_names[[val_name]]
  } else {
    this_val = this_test$ks_result[[val_name]]
  }
  return(this_val)
}

#' Plot histogram of p values
#'
#' @param crosscomm_result result df
#'
#' @return histogram of p values
#' @export
plot_crosscomm_ks_pvals <- function(crosscomm_result) {
  
  crosscomm_result = crosscomm_result %>%
    dplyr::rename(replication = p_value, ernest = ernest_p_val)
  
  
  rep_pval_hist = ggplot2::ggplot(data = crosscomm_result, ggplot2::aes(replication, xmin = 0, xmax = 1)) +
    ggplot2::geom_histogram(data = crosscomm_result, stat = 'bin', 
                            binwidth = 0.05,
                            show.legend = NA,
                            inherit.aes = TRUE) +
    ggplot2::geom_vline(data = crosscomm_result, xintercept = 0.05, color = 'red') + 
    ggplot2::labs(x = "P values", y = "Frequency", title = "Replication P values for pairwise community comparisons") +
    ggplot2::theme_bw()
  
  ernest_pval_hist = ggplot2::ggplot(data = crosscomm_result, ggplot2::aes(ernest, xmin = 0, xmax = 1)) +
    ggplot2::geom_histogram(data = crosscomm_result, stat = 'bin', 
                            binwidth = 0.05,
                            show.legend = NA,
                            inherit.aes = TRUE) +
    ggplot2::geom_vline(data = crosscomm_result, xintercept = 0.05, color = 'red') + 
    ggplot2::labs(x = "P values", y = "Frequency", title = "Ernest 2005 P values for pairwise community comparisons") +
    ggplot2::theme_bw()
  
  pval_hists = gridExtra::grid.arrange(grobs = list(rep_pval_hist, ernest_pval_hist), ncol = 2)
  
  return(pval_hists)
  
}