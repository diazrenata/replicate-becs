#' @title Two sample KS test for BSDs
#' @param raw_communities list of two dfs and names
#' @param ln_mass_vals whether to use the ln masses, defaults FALSE
#' @return outcome list of ks_test, names of communities
#' @export
ks_bsd <- function(raw_communities, ln_mass_vals = FALSE){
  
  community_a = raw_communities$community_a
  community_b = raw_communities$community_b
  
  
  bsd_a = community_a %>%
    add_energy_sizeclass() %>%
    make_bsd()
  
  bsd_b = community_b %>%
    add_energy_sizeclass() %>%
    make_bsd()
  
  if(ln_mass_vals) {
    ks_result = stats::ks.test(bsd_a$ln_mass, bsd_b$ln_mass)
  } else {
    ks_result = stats::ks.test(bsd_a$species_mean_mass,
                               bsd_b$species_mean_mass)
  }
  
  community_names = list(community_a = raw_communities$community_names[1],
                         community_b = raw_communities$community_names[2])
  
  outcome = list(ks_result = ks_result, community_names = community_names)
  
  return(outcome)
  
}

#' @title Extract values from two sample ks test results
#' @description Helper function.
#' @param this_test a test result
#' @param val_name name of the value to pull
#' @return requested value
#' @export
extract_values_twosampleks <- function(this_test, val_name){
  
  if(val_name %in% c("community_a", "community_b")) {
    this_val = this_test$community_names[[val_name]]
  } else {
    this_val = this_test$ks_result[[val_name]]
  }
  return(this_val)
}