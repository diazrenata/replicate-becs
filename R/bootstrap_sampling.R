#' Bootstrap based on a community
#' Wrapper for `bootstrap_unif_bsed_doi`, possibly others. 
#' @param community_df Empirical community to base samples on
#' @param bootstrap_function Bootstrapping function to use
#' @param nbootstraps How many samples to draw
#'
#' @return vector of nbootstraps sampled test statistics
#' @export

community_bootstrap <- function(community_df, bootstrap_function, nbootstraps) {
  
  empirical_value = NULL
  
  if(as.character(bootstrap_function) == 'bootstrap_unif_bsed_doi') {
    community_bsed = make_bsed(community_df)
    empirical_value <- doi(community_bsed$total_energy_proportional)
  }
  
  bootstrap_function <- match.fun(bootstrap_function)
  
  bootstrap_values = replicate(n = nbootstraps, expr = bootstrap_function(community_df))
  
  bootstrap_results = list(bootstrap_values, empirical_value)
  names(bootstrap_results) = c('bootstrap_values', 'empirical_value')
  return(bootstrap_results)
  
}



#' Get p value of empirical value v. bootstrapped results
#'
#' @param bootstrap_results output of community_bootstrap
#'
#' @return p value of empirical value vs. sim distribution
#' @export
get_bootstrap_p <- function(bootstrap_results) {
  
  sim_ecdf = ecdf(bootstrap_results$bootstrap_values)
  p = sim_ecdf(bootstrap_results$empirical_value)
  
}

#' @title DOI of sampled community masses from uniform compared to uniform
#'
#' @description Randomly draw N masses, where N is the total number of individuals in a community, from a uniform distribution with min and max corresponding the min and max of the entire community. 
#' Calculate the DOI of this BSED compared to uniform.
#' 
#' @param community_df Community to base samples on (nind, min and max mass)
#'
#' @return doi
#'
#' @export

bootstrap_unif_bsed_doi <- function(community_df){

  sampled_community <- community_df
  sampled_community$individual_sizes <- runif(n = nrow(community_df),
                                              min = min(community_df$individual_sizes),
                                              max = max(community_df$individual_sizes))
  
  sampled_community_table <- make_community_table(sampled_community)
  
  sampled_bsed <- make_bsed(sampled_community_table)
  
  sampled_doi <- doi(sampled_bsed$total_energy_proportional)
  return(sampled_doi)
}

#' @title Get DOI for bootstrap sampled community masses from two communities
#'
#' @description Pool all individual masses from communities and randomly re-draw communities with the original number of individuals from the pool, with replacement.
#' Calculate the DOI of these new communities.
#'
#' @param community_df_a community data frame for first community
#' @param community_df_b community data frame for second community
#'
#' @return doi
#' 
#' @export

bootstrap_crosscomm_bseds <- function(community_a,
                                     community_b)
{
  
  colnames(community_a) <- c('individual_species_ids', 'individual_sizes')
  colnames(community_b) <- c('individual_species_ids', 'individual_sizes')
  
  
  nind_a = as.integer(nrow(community_a))
  nind_b = as.integer(nrow(community_b))
  
  pool = c(community_a$individual_sizes, community_b$individual_sizes)
  
  nind_tot = as.integer(length(pool))
  
  random_indices_a = sample.int(pool, size = nind_a, replace = T)
  random_indices_b = sample.int(pool, size = nind_b, replace = T)
  
  resampled_a = pool[random_indices_a]
  resampled_b = pool[random_indices_b]
  
  bootstrap_a <- as.data.frame(cbind(random_indices_a, resampled_a))
  bootstrap_b <- as.data.frame(cbind(random_indices_b, resampled_b))
  
  bootstrap_a_bsed <- make_community_table(bootstrap_a) %>%
    make_bsed()
  
  bootstrap_b_bsed <- make_community_table(bootstrap_b) %>%
    make_bsed()

  both_bseds <- bootstrap_a_bsed %>%
    dplyr::full_join(bootstrap_b_bsed, by = c("size_class", "size_class_g")) %>%
    dplyr::mutate(total_energy_proportional.x = replace(total_energy_proportional.x, is.na(total_energy_proportional.x), 0),
                  total_energy_proportional.y = replace(total_energy_proportional.y, is.na(total_energy_proportional.y), 0))
  
  ab_doi <- doi(both_bseds$total_energy_proportional.x,
                both_bseds$total_energy_proportional.y)
  
  return(ab_doi)
}
