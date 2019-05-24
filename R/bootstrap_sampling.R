#' Bootstrap based on a community
#' Wrapper for `bootstrap_unif_sizeabund_bsed_doi`, possibly others. 
#' @param community_df Empirical community to base samples on
#' @param bootstrap_function Bootstrapping function to use
#' @param nbootstraps How many samples to draw
#'
#' @return vector of nbootstraps sampled test statistics
#' @export

community_bootstrap <- function(community_dfs, bootstrap_function, nbootstraps) {
  
  empirical_value = NULL
  
  
  if(as.character(bootstrap_function) == 'bootstrap_unif_sizeabund_bsed_doi') {
    comm_table = make_community_table(community_dfs)
    community_bsed = make_bsed(comm_table)
    
    true_uniform_bsed = uniform_size_abund_bsed(community_dfs)
    
    empirical_value <- doi(community_bsed, true_uniform_bsed)
  }
  
  if(as.character(bootstrap_function) == 'bootstrap_crosscomm_bseds') {
    community_a = community_dfs$community_a
    community_b = community_dfs$community_b
    
    bsed_a = community_a %>%
      make_community_table() %>%
      make_bsed()
    
    bsed_b = community_b %>%
      make_community_table() %>%
      make_bsed()
    
    empirical_value <- doi(bsed_a, bsed_b)
  }
  
  
  
  
  
  bootstrap_function <- match.fun(bootstrap_function)
  
  bootstrap_values = replicate(n = nbootstraps, expr = bootstrap_function(community_dfs))
  
  community_names = community_dfs$community_names
  bootstrap_results = list(bootstrap_values = bootstrap_values, 
                           empirical_value = empirical_value,
                           community_names = community_names)
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
  p = 1- sim_ecdf(bootstrap_results$empirical_value)
  
}


#' Uniform size-abundance BSED
#'
#' Based on a community_df
#'
#' @param community_df table of sizes and ids
#'
#' @return bsed drawn from a uniform size abundance distribution (one individual of every size)
#' @export
#'
uniform_size_abund_bsed <- function(community_df) {
  true_uniform_bsed <- data.frame(individual_sizes = seq(min(community_df$individual_sizes),
                                                         max(community_df$individual_sizes), 
                                                         by = .1), 
                                  individual_species_ids = "notimpt") %>%
    make_community_table() %>%
    make_bsed()
  return(true_uniform_bsed)
}

#' Uniform size-energy BSED
#'
#' Based on a community_df
#'
#' @param community_df table of sizes and ids
#'
#' @return bsed drawn from a uniform body size energy distribution
#' @export
#'
uniform_size_energy_bsed <- function(community_df) {
  true_uniform_bsed <- community_df %>%
    make_community_table() %>%
    make_bsed() %>%
    dplyr::mutate(total_energy = sum(total_energy) / nrow(true_uniform_bsed),
                  total_energy_proportional = total_energy / sum(total_energy))
  return(true_uniform_bsed)
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

bootstrap_unif_sizeabund_bsed_doi <- function(community_df){
  
  sampled_community <- community_df
  
  sampled_community$individual_sizes <- runif(n = nrow(community_df),
                                              min = min(community_df$individual_sizes),
                                              max = max(community_df$individual_sizes))
  
  sampled_community_table <- make_community_table(sampled_community)
  
  sampled_bsed <- make_bsed(sampled_community_table)
  
  true_uniform_bsed = uniform_size_abund_bsed(community_df)
  
  sampled_doi <- doi(sampled_bsed, true_uniform_bsed)
  return(sampled_doi)
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

bootstrap_unif_sizeenergy_bsed_doi <- function(community_df){
  
  sampled_bsed <- community_df %>%
    make_community_table %>%
    make_bsed %>%
    dplyr::mutate(total_energy = runif(n = nrow(sampled_bsed), 
                                       min = min(total_energy), 
                                       max = max(total_energy)),
                  total_energy_proportional = total_energy / sum(total_energy))
  
  true_uniform_bsed = uniform_size_energy_bsed(community_df)
  
  sampled_doi <- doi(sampled_bsed, true_uniform_bsed)
  return(sampled_doi)
}

#' @title Get DOI for bootstrap sampled community masses from two communities
#'
#' @description Pool all individual masses from communities and randomly re-draw communities with the original number of individuals from the pool, with replacement.
#' Calculate the DOI of these new communities.
#'
#' @param community_dfs list of community data frames for two communities
#'
#' @return doi
#' 
#' @export

bootstrap_crosscomm_bseds <- function(community_dfs)
{
  
  community_a = community_dfs$community_a
  community_b = community_dfs$community_b
  
  nind_a = as.integer(nrow(community_a))
  nind_b = as.integer(nrow(community_b))
  
  pool = c(community_a$individual_sizes, community_b$individual_sizes)
  
  nind_tot = as.integer(length(pool))
  
  random_indices_a = sample(1:nind_tot, size = nind_a, replace = T)
  random_indices_b = sample(1:nind_tot, size = nind_b, replace = T)
  
  bootstrap_a_bsed = community_a %>%
    dplyr::mutate(individual_sizes = pool[random_indices_a]) %>%
    make_community_table() %>%
    make_bsed()
  bootstrap_b_bsed =  community_b %>%
    dplyr::mutate(individual_sizes = pool[random_indices_b]) %>%
    make_community_table() %>%
    make_bsed()
  
  
  ab_doi <- doi(bootstrap_a_bsed,
                bootstrap_b_bsed)
  
  return(ab_doi)
}
