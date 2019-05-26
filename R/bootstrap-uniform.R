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

#' Draw bootstrap samples
#' @description Wrapper for bootstrap sampling functions
#' @param raw_community to base samples one
#' @param assumption sampling condition. 
#' @param nbootstraps number of samples to draw
#' @return list of focal bsed, sampled bseds, calculated bseds
#' @export
draw_bootstrap_samples <- function(raw_community, assumption = "uniform_size_abund", nbootstraps = 25) {
  calculate_function = match.fun(paste0("calculate_", assumption, "_bsed"))
  sampler_function = match.fun(paste0("sample_", assumption, "_bsed"))
  
  focal_bsed = raw_community %>%
    add_energy_sizeclass() %>%
    make_bsed()
  
  sampled_bseds = replicate(n = nbootstraps, expr = sampler_function(raw_community), simplify = F)
  
  calculated_bsed = calculate_function(raw_community)
  
  bootstrap_results = list(focal_bsed = focal_bsed,
                           sampled_bseds = sampled_bseds,
                           calculated_bsed = calculated_bsed)
  
  return(bootstrap_results)
}