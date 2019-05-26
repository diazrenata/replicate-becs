#' Rerrange and load table in appendix B
#'
#' @return table of site comparisons, and d and p values, for KS tests.
#' @export
#'
load_tidy_appendix_b <- function(){
  appendix_path = file.path(system.file(package= "replicatebecs"), "data", "ernest-2005-files")
  appendix_b_d = read.csv(file.path(appendix_path, "ernest_appendixB_maxD.csv"), stringsAsFactors = F)
  
  appendix_b_d = appendix_b_d %>%
    tidyr::gather(key = "site_b", value = "max_d", -site_a, na.rm = T) %>%
    dplyr::mutate(site_b = site_b %>%
                    stringr::str_replace(pattern = "v.", replacement = "v ") %>%
                    stringr::str_replace(pattern = ".2", replacement = " 2") %>%
                    stringr::str_replace(pattern = "s.s", replacement = "s s"))
  
  appendix_b_p = read.csv(file.path(appendix_path, "ernest_appendixB_pval.csv"), stringsAsFactors = F)
  appendix_b_p = appendix_b_p %>%
    tidyr::gather(key = "site_b", value = "ernest_p_val", -site_a, na.rm = T)%>%
    dplyr::mutate(site_b = site_b %>%
                    stringr::str_replace(pattern = "v.", replacement = "v ") %>%
                    stringr::str_replace(pattern = ".2", replacement = " 2")%>%
                    stringr::str_replace(pattern = "s.s", replacement = "s s"))
  
  appendix_b = dplyr::left_join(appendix_b_d, appendix_b_p, by = c("site_a", "site_b"))
  
  return(appendix_b)
}

#' Compare summary stats from Ernest and current data
#' @param storage_path where the user accessible data is
#' @return data frame of Ernest summary stats and current summary stats
#' @export
#'
compare_summary_stats = function(storage_path = here::here("working-data", "paper", "processed")) {
  communities <- load_paper_data(storage_path = storage_path)

  bsds <- lapply(communities, FUN = make_bsd)

  communities_summary_stats = data.frame(community_name = names(bsds),
                                         new_richness = NA,
                                         new_min_mass = NA,
                                         new_max_mass = NA,
                                         stringsAsFactors = F)

  for(i in 1:nrow(communities_summary_stats)) {
    communities_summary_stats$new_richness[i] = nrow(bsds[[i]])
    communities_summary_stats$new_min_mass[i] = min(bsds[[i]]$species_mean_mass)
    communities_summary_stats$new_max_mass[i] = max(bsds[[i]]$species_mean_mass)
  }

  communities_summary_stats$new_min_mass[4:9] = min(communities_summary_stats$new_min_mass[4:9])
  communities_summary_stats$new_max_mass[4:9] = max(communities_summary_stats$new_max_mass[4:9])


  ernest_summary_stats = read.csv(file.path(system.file(package= "replicatebecs"), "data", "ernest-2005-files", "ernest_summary_stats.csv"), stringsAsFactors = F)

  ernest_key = read.csv(file.path(system.file(package= "replicatebecs"), "data", "ernest-2005-files", "ernest_key.csv"), stringsAsFactors = F)

  joined_summary_stats = dplyr::left_join(ernest_summary_stats, ernest_key, by = "site") %>%
    dplyr::left_join(communities_summary_stats, by = "community_name") %>%
    dplyr::rename(ernest_name = site,
                  new_name = community_name) %>%
    dplyr::select(ernest_name, new_name, ernest_richness, new_richness,
                  ernest_min_mass, new_min_mass,
                  ernest_max_mass, new_max_mass)

  return(joined_summary_stats)
}