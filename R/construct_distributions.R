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

make_community_table <- function(community, ln_units = 0.2)
{
  colnames(community) <- c('individual_species_ids', 'individual_sizes')
  community_df <-community %>%
    dplyr::mutate(individual_energy = individual_sizes ^ 0.75,
                  ln_mass = log(individual_sizes), 
                  size_class = ln_units * (floor(ln_mass/ln_units)),
                  size_class_g = exp(size_class)) %>%
    dplyr::select(-ln_mass)
  
  return(community_df)
  
}

#' @title Construct BSED from df with size classes
#'
#' @description Calculate total energy of individuals per size class 
#'
#' @param community_df df of species ids, individual sizes, energy, and size class
#'
#' @return bsed
#'
#' @export

make_bsed <- function(community_df, decimals = NULL)
{
  
  bsed <- community_df %>%
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


#' @title Find energy modes from BSED
#'
#' @description Find contiguous size classes with %energy > 5% of total energy 
#'
#' @param bsed A BSED
#'
#' @return size classes with modes
#'
#' @export

find_modes <- function(bsed)
{
  these_modes <- bsed %>%
    dplyr::filter(total_energy_proportional > 0.05) %>%
    dplyr::mutate(mode_id = NA)
  
  this_mode = 1
  
  for(i in 1:nrow(these_modes)){
    if(i == 1) {
      these_modes$mode_id[i] = 1
    } else {
      if(these_modes$size_class[i-1] < these_modes$size_class[i] - 0.3) {
        this_mode = this_mode + 1
      }
      these_modes$mode_id[i] = this_mode
    } 
    
  }
  
  these_modes <- these_modes %>%
    dplyr::select(size_class, mode_id)
  
  return(these_modes)
}

#' @title Calculate energetic dominance
#'
#' @description Calculate energetic dominance
#' 
#' @param modes_list
#' @param community_df
#'
#' @return size classes with modes and energetic dominance 
#'
#' @export

energetic_dominance <- function(community_df)
{
  
  bsed = make_bsed(community_df)
  
  modes_list = find_modes(bsed)
  
  modes_df <- community_df %>%
    dplyr::left_join(modes_list, by = 'size_class') %>% 
    dplyr::filter(!is.na(mode_id)) %>%
    dplyr::group_by(individual_species_ids, mode_id) %>%
    dplyr::summarize(species_energy = sum(individual_energy)) 
  
  
  mode_totals <- modes_df %>%
    dplyr::group_by(mode_id) %>%
    dplyr::summarize(mode_energy = sum(species_energy)) %>%
    dplyr::ungroup()
  
  modes_df <- modes_df %>%
    dplyr::left_join(mode_totals, by = 'mode_id') %>%
    dplyr::mutate(species_energy_prop = species_energy / mode_energy) %>%
    dplyr::group_by(mode_id) %>%
    dplyr::filter(species_energy_prop == max(species_energy_prop)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(e_dominance = species_energy_prop) %>%
    dplyr::select(mode_id, e_dominance)
  
  modes_list <- modes_list %>%
    dplyr::left_join(modes_df, by = 'mode_id')
  
  return(modes_list)
}


#' @title Construct BSD from df with community data
#'
#' @description species level body size distributions
#'
#' @param community_table df of species ids, individual sizes, energy, and size class
#'
#' @return bsd of # species with mean mass in each size class
#'
#' @export

make_bsd <- function(community_df, ln_units = 0.2, decimals = NULL)
{
  
  bsd <- community_df %>%
    dplyr::select(individual_species_ids, individual_sizes) %>%
    dplyr::group_by(individual_species_ids) %>%
    dplyr::summarize(species_mean_mass = mean(individual_sizes)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ln_mass = log(species_mean_mass), 
    size_class = ln_units * (floor(ln_mass/ln_units)),
    size_class_g = exp(size_class)) %>%
  dplyr::group_by(size_class, size_class_g) %>%
    dplyr::summarize(n_species = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n_species_proportional = n_species/sum(n_species))
  
  if(!is.null(decimals)) {
    bsd <- bsd %>%
      dplyr::mutate(size_class_g = round(size_class_g, digits = decimals))
  }
  
  return(bsd)
}