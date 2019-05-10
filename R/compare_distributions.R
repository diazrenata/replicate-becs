#' @title Add energy & size class to raw community df
#'
#' @description Calculate energy and assign size classes  
#'
#' @param community_df df of species ids and individual sizes (real, simulated, or bootstrapped)
#' @param ln_units natural log units to define size classes, defaults to 0.2
#'
#' @return expanded community_df
#'
#' @export

make_community_table <- function(community_df, ln_units = 0.2)
{
  
  community_df <-sim_community %>%
    dplyr::mutate(individual_energy = individual_sizes ^ 0.75,
                  ln_mass = log(individual_sizes), 
                  size_class = ln_units * (floor(ln_mass/ln_units)),
                  size_class_g = exp(size_class)) %>%
    dplyr::select(-ln_mass)
  
  return(community_df)
  
}