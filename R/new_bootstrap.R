sample_uniform_community(base_community, uniform_assumption) {
  
  return(sampled_community)
}

#' @title Compute distribution overlap index for BSEDs
#'
#' @description Calculate DOI for two distributions.
#'
#' @param bsed_a First distribution
#' @param bsed_b Second distribution
#' 
#' @return DOI for two distributions
#'
#' @export

doi <- function(bsed_a, bsed_b)
{
  bsed_a = bsed_a %>%
    dplyr::select(size_class, total_energy_proportional)
  
  bsed_b = bsed_b %>%
    dplyr::select(size_class, total_energy_proportional)
  
  both_bseds = bsed_a %>%
    dplyr::full_join(bsed_b, by = 'size_class') %>%
    tidyr::replace_na(list(size_class = NA, total_energy_proportional.x = 0,
                           total_energy_proportional.y = 0))
  
  doi = sum(abs(both_bseds$total_energy_proportional.x - both_bseds$total_energy_proportional.y))
  
  return(doi)
}


#' Wrapper for DOI
#'
#' @param bsed_a first bsed for comparison
#' @param comparison_dist either a BSED to compare bsed_a to, or the name of a function to use to generate the comparison distribution. 
#'
#' @return named list of doi_value = doi_value, dists = list(bsed_a, bsed_b)
#' @export
doi_general = function(bsed_a, comparison_dist) {
  
  if(is.data.frame(comparison_dist)) {
    bsed_b = comparison_dist
  } else {
    comparison_function = match.fun(comparison_dist)
    bsed_b = comparison_function(bsed_a)
  }
  
  doi_value = doi(bsed_a, bsed_b) 
  
  return(list(doi_value = doi_value, dists = list(bsed_a = bsed_a, bsed_b = bsed_b)))
  
}

uniform_size_linear = function(community_df) {
  comm_table = make_community_table(community_dfs)
  community_bsed = make_bsed(comm_table)
  
  uniform_bsed <- data.frame(individual_sizes = seq(min(community_dfs$individual_sizes),
                                                         max(community_dfs$individual_sizes), 
                                                         by = .1), 
                                  individual_species_ids = "notimpt") %>%
    make_community_table() %>%
    make_bsed()
  
  return(uniform_bsed)
}

uniform_size_log = function(community_df) {
  
}

uniform_energy_linear = function(community_df) {
  
}

uniform_energy_log = function(community_df) {
  
}