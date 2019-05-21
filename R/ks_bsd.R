#' Two sample KS test for BSDs
#'
#' @param community_dfs list of dfs and names
#' @param ln_mass_vals whether to use the ln masses, defaults FALSE
#'
#' @return outcome list of ks_test, names
#' @export
ks_bsd <- function(community_dfs, ln_mass_vals = FALSE){
  
  community_a = community_dfs$community_a
  community_b = community_dfs$community_b
  
  
  bsd_a = community_a %>%
    make_community_table() %>%
    make_bsd()
  
  bsd_b = community_b %>%
    make_community_table() %>%
    make_bsd()
  
  if(ln_mass_vals) {
    ks_result = stats::ks.test(bsd_a$ln_mass, bsd_b$ln_mass)
  } else {
    ks_result = stats::ks.test(bsd_a$species_mean_mass,
                        bsd_b$species_mean_mass)
  }
  
  outcome = list(ks_result = ks_result, community_names = community_dfs$community_names)
  
  return(outcome)
  
}