#' @title Simulate species' masses
#'
#' @description Randomly draw mean masses for S species with minimum Min and maximum Max, from a uniform distribution.
#'
#' @param nspecies S
#' @param min_mass Min average mass
#' @param max_mass Max average mass
#'
#' @return vector of body sizes for a community of S species
#'
#' @export

simulate_species_mass <- function(nspecies,
                                 min_mass,
                                 max_mass)
{
community_masses <- runif(n = nspecies, min = min_mass, max = max_mass)

return(community_masses)
}

#' @title Simulate species identities for individuals
#'
#' @description Randomly assign species identities from 1 to S to N individuals.
#'
#' @param nspecies S, number of species
#' @param nind N, number of individuals
#'
#' @return vector of species identities (1:S) for N individuals
#'
#' @export

simulate_ind_species <- function(nspecies,
                                  nind)
{
  individual_species <- sample(nspecies, size = nind, replace = TRUE)

  return(individual_species)
}


#' @title Draw a mass for an individual
#'
#' @description Draw a mass for an individual given its species ID and the community masses.
#'
#' @param species_id the species ID (int, corresponds to element of community_masses)
#' @param community_masses vector of mean masses for all species in the community
#'
#' @return a mass value
#'
#' @export

draw_single_mass <- function(species_id, community_masses)
  {
  this_mean <- community_masses[species_id]
  this_sd <- 0.1 * this_mean
  this_mass <- rnorm(1, mean = this_mean, sd = this_sd)
  return(this_mass)
}


#' @title Simulate masses for individuals
#'
#' @description Randomly generate masses for individuals given species assignments & mean masses for each species. Assumes a normal distribution with SD = 0.1 * mass.
#'
#' @param individual_species Vector of species identities for each individual
#' @param community_masses Vector of mean body size for each species
#'
#' @return vector of masses for N individuals, drawn according to the species' mean mass and the species assignments.
#'
#' @export

simulate_ind_mass <- function(individual_species,
                                 community_masses)
{

individual_masses <- vapply(individual_species, FUN = draw_single_mass, community_masses = community_masses, FUN.VALUE = 1)

return(individual_masses)
}

#' @title Simulate an entire community
#'
#' @description Randomly generate masses for N individuals randomly distributed among S species assignments & with minimum and maximum body sizes.
#'
#' @param nspecies S
#' @param nind N
#' @param min_mass Min average mass
#' @param max_mass Max average mass
#'
#' @return matrix with 1 row per individual and two columns: species assignments and mass.
#'
#' @export

simulate_community <- function(nspecies, nind, min_mass, max_mass)
{

 species_sizes <- simulate_species_mass(nspecies = nspecies, min_mass = min_mass,
                                         max_mass = max_mass)

  individual_species_ids <- simulate_ind_species(nspecies = nspecies, nind = nind)

  individual_sizes <- simulate_ind_mass(individual_species_ids, species_sizes)

  community_df <- cbind(individual_species_ids, individual_sizes)
  
  community_df <- as.data.frame(community_df)

  return(community_df)
}
