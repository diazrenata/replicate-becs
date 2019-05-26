#' Calculate a BSED from a completely uniform size-abundance distribution
#' @description Based on a raw community df. For DOI comparisons.
#' @param raw_community table of sizes and ids
#' @return bsed from a uniform size abundance distribution of min and max size of the raw community(one individual of every size)
#' @export
#'
calculate_uniform_size_abund_bsed <- function(raw_community) {
  true_uniform_bsed <- data.frame(individual_sizes = seq(min(raw_community$individual_sizes),
                                                         max(raw_community$individual_sizes), 
                                                         by = .1), 
                                  individual_species_ids = "notimpt",
                                  stringsAsFactors = F) %>%
    add_energy_sizeclass() %>%
    make_bsed()
  return(true_uniform_bsed)
}

#' Draw a sample community with a uniform size-abundance distribution
#' @description With the same minimum and maximum body size as an empirical or focal community.
#' @param raw_community Community to base new community on. 
#' @return bsed of sampled community
#' @export
#' @importFrom stats runif
sample_uniform_size_abund_bsed <- function(raw_community) {
  min_size = min(raw_community$individual_sizes)
  max_size = max(raw_community$individual_sizes)
  nind = nrow(raw_community)
  sampled_community <- raw_community %>% 
    dplyr::mutate(individual_species_ids = "notimpt",
                  individual_sizes = runif(n = nind, 
                                           min = min_size,
                                           max = max_size)) %>%
    add_energy_sizeclass() %>%
    make_bsed()
  
  return(sampled_community)  
}

#' @title Bootstrap compare two communities
#' @description Randomly re-draw communities with masses and nindividuals of original communities.
#' @param community_pair list of 2 communities to compare
#' @return list of 2 bseds
#' @export
sample_cross_communities_bsed <- function(community_pair) {
  community_a = community_pair$community_a 
  community_b = community_pair$community_b
  
  nind_a = as.integer(nrow(community_a))
  nind_b = as.integer(nrow(community_b))
  
  pool = c(community_a$individual_sizes, community_b$individual_sizes)
  
  nind_tot = as.integer(length(pool))
  random_indices_a = sample(1:nind_tot, size = nind_a, replace = T)
  random_indices_b = sample(1:nind_tot, size = nind_b, replace = T)
  
  bootstrap_a_bsed = community_a %>%
    dplyr::mutate(individual_sizes = pool[random_indices_a]) %>%
    add_energy_sizeclass() %>%
    make_bsed()
  bootstrap_b_bsed =  community_b %>%
    dplyr::mutate(individual_sizes = pool[random_indices_b]) %>%
    add_energy_sizeclass() %>%
    make_bsed()
  
  bootstrap_results = list(bootstrap_a = bootstrap_a_bsed,
                           bootstrap_b = bootstrap_b_bsed)
  
  return(bootstrap_results)
}

#' Draw bootstrap samples
#' @description Wrapper for bootstrap sampling functions
#' @param raw_community community or communities to base samples on
#' @param assumption sampling condition. 
#' @param nbootstraps number of samples to draw
#' @return list of focal bsed, sampled bseds, calculated bseds
#' @export
draw_bootstrap_samples <- function(raw_community, assumption = "uniform_size_abund", nbootstraps = 25) {
  sampler_function = match.fun(paste0("sample_", assumption, "_bsed"))
  
  if(assumption == "uniform_size_abund") {
    calculate_function = match.fun(paste0("calculate_", assumption, "_bsed"))
    
    focal_bsed = raw_community %>%
      add_energy_sizeclass() %>%
      make_bsed()
    
    sampled_bseds = replicate(n = nbootstraps, expr = sampler_function(raw_community), simplify = F)
    
    calculated_bsed = calculate_function(raw_community)
    
    bootstrap_results = list(focal_bsed = focal_bsed,
                             sampled_bseds = sampled_bseds,
                             calculated_bsed = calculated_bsed)
  } else if (assumption == "cross_communities") {
    bootstrap_results = replicate(n = nbootstraps, expr = sampler_function(raw_community), simplify = F)
  }
  
  return(bootstrap_results)
}

#' Calculate DOIs for bootstrapped BSEDs
#' @param bootstrap_results output of draw_bootstrap_samples
#' @return list of focal DOI and sampled DOIs
#' @export
calculate_bootstrap_uniform_dois <- function(bootstrap_results) {
  focal_doi = doi(bootstrap_results$focal_bsed, bootstrap_results$calculated_bsed)
  sampled_dois = vapply(bootstrap_results$sampled_bseds, FUN = doi, bsed_b = bootstrap_results$calculated_bsed, FUN.VALUE = 0.5)
  doi_results = list(focal_doi = focal_doi,
                     sampled_dois = sampled_dois)
  return(doi_results)
}

#' Get p value of empirical value v. bootstrapped results
#' @param bootstrap_dois output of calculate_bootstrap_dois
#' @return p value of empirical value vs. sim distribution
#' @export
#' @importFrom stats ecdf
calculate_bootstrap_p <- function(bootstrap_dois) {
  
  sim_ecdf = ecdf(bootstrap_dois$sampled_dois)
  p = 1- sim_ecdf(bootstrap_dois$focal_doi)
  
  return(p)
}
