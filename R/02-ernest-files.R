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