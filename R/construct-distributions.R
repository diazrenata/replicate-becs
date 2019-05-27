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


#' @title Find energy modes from BSED
#'
#' @description Find contiguous size classes with %energy > 5% of total energy. Helper for energetic dominance. 
#'
#' @param bsed A BSED
#' @param mode_cutoff defaults to 0.05, can be another number or "prop" = proportional to # of size classes in community
#' @return size classes with modes
find_modes <- function(bsed, mode_cutoff = 0.05)
{
  
  if(mode_cutoff ==  "prop") {
    mode_cutoff = 1/(length(unique(bsed$size_class)))
  }
  
  these_modes <- bsed %>%
    dplyr::filter(total_energy_proportional > mode_cutoff) %>%
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
#' @param community_energy community table
#' @param mode_cutoff defaults to 0.05, or "prop" = proportional to # of size classes in community
#'
#' @return size classes with modes and energetic dominance 
#'
#' @export

energetic_dominance <- function(community_energy, mode_cutoff = 0.05)
{
  
  bsed = make_bsed(community_energy)
  
  modes_list = find_modes(bsed, mode_cutoff)
  
  modes_df <- community_energy %>%
    dplyr::left_join(modes_list, by = "size_class") %>% 
    dplyr::filter(!is.na(mode_id)) %>%
    dplyr::group_by(individual_species_ids, mode_id) %>%
    dplyr::summarize(species_energy = sum(individual_energy)) 
  
  
  mode_totals <- modes_df %>%
    dplyr::group_by(mode_id) %>%
    dplyr::summarize(mode_energy = sum(species_energy)) %>%
    dplyr::ungroup()
  
  modes_df <- modes_df %>%
    dplyr::left_join(mode_totals, by = "mode_id") %>%
    dplyr::mutate(species_energy_prop = species_energy / mode_energy) %>%
    dplyr::group_by(mode_id) %>%
    dplyr::filter(species_energy_prop == max(species_energy_prop)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(e_dominance = species_energy_prop) %>%
    dplyr::select(mode_id, e_dominance)
  
  modes_list <- modes_list %>%
    dplyr::left_join(modes_df, by = "mode_id") %>% 
    dplyr::group_by(mode_id) %>%
    dplyr::mutate(size_class_min = min(size_class), size_class_max = max(size_class)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-size_class) %>%
    dplyr::distinct() 
  
  return(modes_list)
}


#' @title Set up pairwise community combinations
#' @description Set up a list of lists of all possible 2-way comparisons of communities in a list of communities.
#' @param communities list of communities to compare to each other
#' @return list of all possible combinations of two communities.
#' @export
setup_community_combinations = function(communities) {
  ncommunities = length(communities)
  
community_combination_indices = utils::combn(x = c(1:ncommunities), m = 2, simplify = TRUE) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(community_a = V1, community_b = V2)

community_combinations = apply(community_combination_indices, MARGIN = 1, FUN = combine_communities, communities = communities)

return(community_combinations)
}



#' @title Combine communities
#' @description Helper function for setup_community_combinations
#' @param indices indices of communities to pair
#' @param communities list of communities
#' @return list of the two indices to pair
combine_communities = function(indices, communities) {
  community_combination = list(community_a = communities[[indices[1]]], community_b = communities[[indices[2]]], community_names = c(names(communities)[[indices[1]]], names(communities)[[indices[2]]]))
  return(community_combination)
}
