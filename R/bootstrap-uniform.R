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
