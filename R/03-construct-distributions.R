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

#' @title Construct BSD from df with community data
#'
#' @description species level body size distributions
#'
#' @param raw_community df of species ids and individual sizes
#' @param decimals how many decimals for rounding, defaults to NULL
#' @param ln_units  defaults to 0.2
#'
#' @return bsd of species and mean masses in g and log(mean mass)
#'
#' @export
#' @importFrom stats sd

make_bsd <- function(raw_community, decimals = NULL, ln_units = 0.2)
{
  bsd <- raw_community %>%
    dplyr::select(individual_species_ids, individual_sizes) %>%
    dplyr::group_by(individual_species_ids) %>%
    dplyr::summarize(species_mean_mass = mean(individual_sizes)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ln_mass = log(species_mean_mass),
                  size_class = ln_units * (floor(ln_mass/ln_units)),
                  size_class_g = exp(size_class), stdev = sd(ln_mass))
  
  if(!is.null(decimals)) {
    bsd <- bsd %>%
      dplyr::mutate(size_class_g = round(size_class_g, digits = decimals))
  }
  
  return(bsd)
}

#' @title Construct BSED from df with size classes
#'
#' @description Calculate total energy of individuals per size class 
#'
#' @param community_energy df of species ids, individual sizes, energy, and size class
#' @param decimals default NULL; decimals to round size classes
#'
#' @return bsed
#'
#' @export

make_bsed <- function(community_energy, decimals = NULL)
{
  
  bsed <- community_energy %>%
    dplyr::group_by(size_class, size_class_g) %>%
    dplyr::summarize(total_energy = sum(individual_energy)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(total_energy_proportional = total_energy / sum(total_energy))
  
  if(!is.null(decimals)) {
    bsed <- bsed %>%
      dplyr::mutate(size_class_g = round(size_class_g, digits = decimals))
  }
  
  return(bsed)
}