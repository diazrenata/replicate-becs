#' Compare Ernest two sample KS results to current
#' @param twosample_ks_results result of lapply(community_combinations, twosampleks)
#' @return dataframe comparing Ernest results to current results for each community
#' @export
#' @importFrom stats na.omit
compare_ernest_ks_values <- function(twosample_ks_results){

  ernest_key <- read.csv(file.path(system.file(package= "replicatebecs"), "data", "ernest-2005-files", "ernest_key.csv"), stringsAsFactors = F)

  appendix_b = load_tidy_appendix_b() %>%
    dplyr::left_join(ernest_key, by = c('site_a' = 'site')) %>%
    dplyr::rename(community_a = community_name) %>%
    dplyr::left_join(ernest_key, by = c('site_b' = 'site')) %>%
    dplyr::rename(community_b = community_name)

  ks_results = data.frame(
    community_a = vapply(twosample_ks_results, FUN = extract_values_twosampleks, val_name = "community_a", FUN.VALUE = 'portal'),
    community_b = vapply(twosample_ks_results, FUN = extract_values_twosampleks, val_name = "community_b", FUN.VALUE = 'portal'),
    ks_d =  vapply(twosample_ks_results, FUN = extract_values_twosampleks, val_name = "statistic", FUN.VALUE = .5),
    p_value =  vapply(twosample_ks_results, FUN = extract_values_twosampleks, val_name = "p.value", FUN.VALUE = .5),
    stringsAsFactors = F
  )
  ks_joined = ks_results %>%
    dplyr::left_join(appendix_b, by = c('community_a', 'community_b')) %>%
    na.omit()
  ks_joined2 = ks_results %>%
    dplyr::rename(community_b = community_a, community_a = community_b) %>%
    dplyr::left_join(appendix_b, by = c('community_a', 'community_b')) %>%
    na.omit() %>%
    dplyr::bind_rows(ks_joined)

  return(ks_joined2)
}


#' @title Load Ernest appendix A
#' @description Load results of d-corrected KS test
#' @return table of results
#' @export
load_ernest_appendixA <- function() {
  ernest_key <- read.csv(file.path(system.file(package= "replicatebecs"), "data", "ernest-2005-files", "ernest_key.csv"), stringsAsFactors = F)
  
  ernest_appendixA <- read.csv(file.path(system.file(package= "replicatebecs"), "data", "ernest-2005-files", "ernest_appendixA.csv"), stringsAsFactors = F) %>%
    dplyr::left_join(ernest_key, by = 'site')
  
  return(ernest_appendixA)
}