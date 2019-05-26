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