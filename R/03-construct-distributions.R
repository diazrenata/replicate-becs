#' @title Add energy & size class to raw community df
#' @description Calculate energy and assign size classes  
#' @param raw_community df of species ids and individual sizes (real, simulated, or bootstrapped)
#' @param ln_units natural log units to define size classes, defaults to 0.2
#' @return raw_community with added columns for energy, ln_mass, size class, and the size class in g
#' @export

add_energy_sizeclass <- function(raw_community, ln_units = 0.2)
{
  community_energy <- raw_community %>%
    dplyr::mutate(individual_energy = individual_sizes ^ 0.75,
                  ln_mass = log(individual_sizes), 
                  size_class = ln_units * (floor(ln_mass/ln_units)),
                  size_class_g = exp(size_class)) %>%
    dplyr::select(-ln_mass)
  
  return(community_energy)
  
}